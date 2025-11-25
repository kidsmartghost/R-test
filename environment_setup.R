# environment_setup.R

setup_environment <- function(baseconfig) {
  # 定义关键路径
  base_dir <- baseconfig[[1]]
  data_dir <- file.path(base_dir, "database")
  output_dir <- file.path(base_dir, "output")
  logs_dir <- file.path(base_dir, "logs")
  db_path <- file.path(data_dir, "userlist.sqlite")
  
  # 创建必要目录
  required_dirs <- c(data_dir, output_dir, logs_dir)
  for (dir_path in required_dirs) {
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
      message("✓ 创建目录: ", dir_path)
    } else {
      message("✓ 目录已存在: ", dir_path)
    }
  }
  
  # 检查写入权限
  test_file <- file.path(output_dir, "test_write.tmp")
  tryCatch({
    writeLines("test", test_file)
    file.remove(test_file)
    message("✓ 输出目录写入权限正常")
  }, error = function(e) {
    stop("✗ 输出目录无法写入: ", e$message)
  })
  
  # 返回路径配置
  list(
    base_dir = base_dir,
    data_dir = data_dir,
    output_dir = output_dir,
    logs_dir = logs_dir,
    db_path = db_path
  )
}
