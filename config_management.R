# config_management.R
load_config <- function() {
  # 从环境变量获取配置（云平台推荐方式）
  config <- list(
    base_dir <- "./",
    db_type = Sys.getenv("DB_TYPE", "sqlite")
    
  )
  
  return(config)

}

