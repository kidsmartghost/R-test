# init_project.R
initialize_project <- function() {
  message("开始初始化项目环境...")
  
  # 1. 加载配置
  config <- load_config()
  message("✓ 配置加载完成")
  
  # 2. 设置环境路径
  env_config <- setup_environment(config)
  message("✓ 环境路径设置完成")
  
  # 3. 连接数据库
  db_conn <- setup_database(env_config)
  message("✓ 数据库连接建立")
  
  # 4. 安装必要包
  install_required_packages()
  message("✓ 必要包检查完成")
  
  # 5. 验证环境
  validation_result <- validate_environment(env_config, db_conn)
  
  if (validation_result$success) {
    message("🎉 环境初始化成功！")
    return(list(
      config = env_config,
      db_conn = db_conn,
      status = "ready"
    ))
  } else {
    stop("环境验证失败: ", validation_result$message)
  }
}

# 包依赖管理
install_required_packages <- function() {
  required_packages <- c(
    "dplyr", "ggplot2", "RSQLite", "DBI", "jsonlite", "yaml","shiny","readr","readxl","DT","openxlsx","haven","tools",
    "lubridate","ellmer","rjson","shinymanager","sodium","shinyjs"
  )
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("🔧 正在安装缺失的包: ", pkg)
      install.packages(pkg)
    }
    # 加载包
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
}

# 环境验证
validate_environment <- function(config, db_conn) {
  checks <- list()
  
  # 检查目录权限
  checks$dirs_writable <- all(
    file.access(config$data_dir, 2) == 0,
    file.access(config$output_dir, 2) == 0
  )
  
  # 检查数据库连接
  checks$db_connected <- DBI::dbIsValid(db_conn)
  
  # 汇总结果
  if (all(unlist(checks))) {
    return(list(success = TRUE, message = "所有检查通过"))
  } else {
    failed_checks <- names(checks)[!unlist(checks)]
    return(list(
      success = FALSE, 
      message = paste("失败的检查:", paste(failed_checks, collapse = ", "))
    ))
  }
}