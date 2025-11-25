# config_management.R
load_config <- function() {
  # 从环境变量获取配置（云平台推荐方式）
  config <- list(
    base_dir <- Sys.getenv("YTwork","C:/Users/Yubin.Luo/Desktop/R Shiny/R test1"),
    db_type = Sys.getenv("DB_TYPE", "sqlite")
    
  )
  
  return(config)
}