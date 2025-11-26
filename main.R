# main.R
source("environment_setup.R")
source("database_setup.R")
source("config_management.R")
source("init_project.R")
source("shiny.R")

# 初始化项目
tryCatch({
  project <- initialize_project()
  
  # 保存配置引用
  config <- project$config
  user_con <- project$config$db_path
  ad_con<- project$config$db_path2
  
  # 运行应用
   final_app()
  

}, error = function(e) {
  message("初始化失败: ", e$message)
  quit(status = 1)
})


