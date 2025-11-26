# database_setup.R
setup_database <- function(config,Tc=1) {

  # 连接SQLite数据库（示例）
  if (Tc==1) {conn <- dbConnect(RSQLite::SQLite(), config$db_path)}
  else {conn <- dbConnect(RSQLite::SQLite(), config$db_path2)}
  
  # 初始化数据表（示例）
  if ( (Tc==1) && (!dbExistsTable(conn, "users"))) {
    
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
    
   
  }
  
  if ((Tc != 1) && (!dbExistsTable(conn, "audit_logs")) ) {
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
add_audit <- function(who, action, detail = "") {
  conn_aud <- dbConnect(RSQLite::SQLite(), ad_con)
  dbExecute(conn_aud, "INSERT INTO audit_logs (who, action, detail) VALUES (?,?,?)",
            params = list(who %||% "system", action, detail))
  dbDisconnect(conn_aud)
}

# register user
register_user <- function(username, email, password) {
  conn_user <- dbConnect(RSQLite::SQLite(), user_con)
  on.exit({
    tryCatch(dbDisconnect(conn), error = function(e) NULL)
  }, add = TRUE)
  
  # 事务：先检查再插入，防止并发脏读（注意：对于高度并发需依赖 DB 的唯一约束）
  tryCatch({
    dbBegin(conn_user)
    
    # 检查 username/email 已存在
    existing <- dbGetQuery(conn_user, "SELECT username, email FROM users WHERE username = ? OR email = ?",
                           params = list(username, email))
    if (nrow(existing) > 0) {
      dbRollback(conn_user)
      return(FALSE)
    }
    pw_hash <- password_store(password)
    dbExecute(conn_user, "INSERT INTO users (username, email, password_hash, role, is_active) VALUES (?, ?, ?, ?, 1)",
              params = list(username, email, pw_hash, "user"))
    
    # 获取插入的 id （SQLite 专用）
    user_id <- dbGetQuery(conn_user, "SELECT last_insert_rowid() AS id")$id[1]
    
    # 写审计（置于事务中 or 事务外都可，根据策略）
    add_audit(username, "register", paste0("email=", email, " user_id=", user_id))
    
    dbCommit(conn_user)
    
    return(TRUE)
  }, error = function(e) {
    # 捕获唯一约束或其它数据库错误
    tryCatch(dbRollback(conn_user), error = function(e2) NULL)
    add_audit(username %||% "unknown", "register_failed", conditionMessage(e))
    return(FALSE)
  })
}


# check login: returns list(success=TRUE, username=..., role=...) or failure reason
check_login <- function(identifier, password) {
  conn_user <- dbConnect(RSQLite::SQLite(), user_con)
  # identifier can be username or email
  row <- dbGetQuery(conn_user, "SELECT * FROM users WHERE username = ? OR email = ?", params = list(identifier, identifier))
  if (nrow(row) == 0) { dbDisconnect(conn_user); return(list(success = FALSE, reason = "not_found")) }
  # locked check
  if (!is.na(row$locked_until[1]) && nzchar(row$locked_until[1])) {
    locked_until <- tryCatch(as.POSIXct(row$locked_until[1], tz = "UTC"), error = function(e) NULL)
    if (!is.null(locked_until) && Sys.time() < locked_until) {
      dbDisconnect(conn_user); return(list(success = FALSE, reason = "locked", locked_until = locked_until))
    }
  }
  # is active?
  if (row$is_active[1] == 0) { dbDisconnect(conn_user); return(list(success = FALSE, reason = "not_activated")) }
  # verify password
  ok <- password_verify(row$password_hash[1], password)
  if (ok) {
    # reset failed_attempts, update last_login
    dbExecute(conn_user, "UPDATE users SET failed_attempts = 0, locked_until = NULL, last_login = datetime('now') WHERE id = ?", params = list(row$id[1]))
    add_audit(row$username[1], "login_success", paste0("user_id=", row$id[1]))
    dbDisconnect(conn_user)
    return(list(success = TRUE, username = row$username[1], role = row$role[1]))
  } else {
    # increment failed attempts
    fa <- row$failed_attempts[1] + 1
    lock_until <- NULL
    if (fa >= 5) { # 锁定策略：连续 5 次失败 -> 锁定 15 分钟
      lock_until <- as.character(Sys.time() + 15*60)
      dbExecute(conn_user, "UPDATE users SET failed_attempts = ?, locked_until = ? WHERE id = ?", params = list(fa, lock_until, row$id[1]))
      add_audit(row$username[1], "login_locked", paste0("failed_attempts=", fa))
    } else {
      dbExecute(conn_user, "UPDATE users SET failed_attempts = ? WHERE id = ?", params = list(fa, row$id[1]))
      add_audit(row$username[1], "login_failed", paste0("failed_attempts=", fa))
    }
    dbDisconnect(conn_user)
    return(list(success = FALSE, reason = "bad_password", failed_attempts = fa, locked_until = lock_until))
  }
}


# change password (requires old password)
change_password <- function(username, old_pw, new_pw) {
  conn_user <- dbConnect(RSQLite::SQLite(), user_con)
  row <- dbGetQuery(conn_user, "SELECT id, password_hash FROM users WHERE username = ?", params = list(username))
  if (nrow(row) == 0) { dbDisconnect(conn_user); return(FALSE) }
  ok <- password_verify(row$password_hash[1], old_pw)
  if (!ok) { dbDisconnect(conn_user); return(FALSE) }
  pw_hash <- password_store(new_pw)
  dbExecute(conn_user, "UPDATE users SET password_hash = ? WHERE id = ?", params = list(pw_hash, row$id[1]))
  add_audit(username, "change_password", "")
  dbDisconnect(conn_user)
  TRUE
}
