# database_setup.R
setup_database <- function(config) {

  # 连接SQLite数据库（示例）
  conn <- dbConnect(RSQLite::SQLite(), config$db_path)
  
  # 初始化数据表（示例）
  if (!dbExistsTable(conn, "userlist")) {
    dbExecute(conn, "
      CREATE TABLE userlist (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        name TEXT,
        password TEXT
      )
    ")
    
    dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS users (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE,
      email TEXT UNIQUE,
      password_hash TEXT,
      role TEXT DEFAULT 'user',
      is_active INTEGER DEFAULT 0,         -- 激活标识（0 未激活，1 激活）
      failed_attempts INTEGER DEFAULT 0,
      locked_until TEXT DEFAULT NULL,
      created_at TEXT DEFAULT (datetime('now')),
      last_login TEXT DEFAULT NULL,
      reset_token TEXT DEFAULT NULL,
      reset_expiry TEXT DEFAULT NULL
    );
  ")
    
    
    # audit logs
    dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS audit_logs (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      who TEXT,
      action TEXT,
      detail TEXT,
      created_at TEXT DEFAULT (datetime('now'))
    );
  ") 
    
   
  }
  
  # 返回数据库连接
  return(conn)
}

# log action
add_audit <- function(who, action, detail = "",db_conn=db_conn) {
  dbExecute(db_conn, "INSERT INTO audit_logs (who, action, detail) VALUES (?,?,?)",
            params = list(who %||% "system", action, detail))
  dbDisconnect(db_conn)
}

# register user
register_user <- function(username, email, password,db_conn=db_conn) {
  pw_hash <- password_store(password)
  res <- tryCatch({
    dbExecute(db_conn, "INSERT INTO users (username, email, password_hash, role, is_active) VALUES (?,?,?,?,0)",
              params = list(username, email, pw_hash, "user"))
    add_audit(username, "register", paste0("email=", email))
    dbExecute(con, "UPDATE users SET is_active = 1, reset_token = NULL, reset_expiry = NULL WHERE id = ?", params = list(row$id[1]))
    TRUE
  }, error = function(e) {
    FALSE
  })
  dbDisconnect(db_conn)
  res
}


# check login: returns list(success=TRUE, username=..., role=...) or failure reason
check_login <- function(identifier, password,db_conn=db_conn) {
  # identifier can be username or email
  row <- dbGetQuery(db_conn, "SELECT * FROM users WHERE username = ? OR email = ?", params = list(identifier, identifier))
  if (nrow(row) == 0) { dbDisconnect(db_conn); return(list(success = FALSE, reason = "not_found")) }
  # locked check
  if (!is.na(row$locked_until[1]) && nzchar(row$locked_until[1])) {
    locked_until <- tryCatch(as.POSIXct(row$locked_until[1], tz = "UTC"), error = function(e) NULL)
    if (!is.null(locked_until) && Sys.time() < locked_until) {
      dbDisconnect(db_conn); return(list(success = FALSE, reason = "locked", locked_until = locked_until))
    }
  }
  # is active?
  if (row$is_active[1] == 0) { dbDisconnect(db_conn); return(list(success = FALSE, reason = "not_activated")) }
  # verify password
  ok <- password_verify(row$password_hash[1], password)
  if (ok) {
    # reset failed_attempts, update last_login
    dbExecute(db_conn, "UPDATE users SET failed_attempts = 0, locked_until = NULL, last_login = datetime('now') WHERE id = ?", params = list(row$id[1]))
    add_audit(row$username[1], "login_success", paste0("user_id=", row$id[1]))
    dbDisconnect(db_conn)
    return(list(success = TRUE, username = row$username[1], role = row$role[1]))
  } else {
    # increment failed attempts
    fa <- row$failed_attempts[1] + 1
    lock_until <- NULL
    if (fa >= 5) { # 锁定策略：连续 5 次失败 -> 锁定 15 分钟
      lock_until <- as.character(Sys.time() + 15*60)
      dbExecute(db_conn, "UPDATE users SET failed_attempts = ?, locked_until = ? WHERE id = ?", params = list(fa, lock_until, row$id[1]))
      add_audit(row$username[1], "login_locked", paste0("failed_attempts=", fa))
    } else {
      dbExecute(db_conn, "UPDATE users SET failed_attempts = ? WHERE id = ?", params = list(fa, row$id[1]))
      add_audit(row$username[1], "login_failed", paste0("failed_attempts=", fa))
    }
    dbDisconnect(db_conn)
    return(list(success = FALSE, reason = "bad_password", failed_attempts = fa, locked_until = lock_until))
  }
}


# change password (requires old password)
change_password <- function(username, old_pw, new_pw,db_conn=db_conn) {
  row <- dbGetQuery(db_conn, "SELECT id, password_hash FROM users WHERE username = ?", params = list(username))
  if (nrow(row) == 0) { dbDisconnect(db_conn); return(FALSE) }
  ok <- password_verify(row$password_hash[1], old_pw)
  if (!ok) { dbDisconnect(db_conn); return(FALSE) }
  pw_hash <- password_store(new_pw)
  dbExecute(db_conn, "UPDATE users SET password_hash = ? WHERE id = ?", params = list(pw_hash, row$id[1]))
  add_audit(username, "change_password", "")
  dbDisconnect(db_conn)
  TRUE
}
