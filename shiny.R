
final_app <- function() {
# 定义UI
  
  # ---------- UI ----------
  ui <- fluidPage(
    useShinyjs(),
    titlePanel("YT test"),
    div(id = "login_register_ui",
        tabsetPanel(id = "auth_tabs", type = "tabs",
                    tabPanel("登录", value = "login",
                             wellPanel(
                               textInput("id_input", "用户名或邮箱"),
                               passwordInput("pw_input", "密码"),
                               actionButton("btn_login", "登录"),
                               br(), br(),
                               actionButton("btn_show_register", "注册新账户"),
                               actionButton("btn_show_forgot", "忘记密码 / 重置"),
                               br(), hr(),
                               uiOutput("user_info_box")
                             )
                    ),
                    tabPanel("修改密码", value = "reset",
                             wellPanel(
                               passwordInput("old_pw", "当前密码"),
                               passwordInput("new_pw", "新密码"),
                               passwordInput("new_pw2", "重复新密码"),
                               actionButton("btn_change_pw", "更改密码"),
                               br(), br(),
                               verbatimTextOutput("change_pw_res")
                             )
                    ),
                    tabPanel("注册", value = "register",
                             wellPanel(
                               textInput("reg_username", "用户名"),
                               textInput("reg_email", "邮箱"),
                               passwordInput("reg_pw", "密码"),
                               passwordInput("reg_pw2", "确认密码"),
                               actionButton("reg_submit", "注册"),
                               br(), br(),
                               actionLink("go_to_login", "返回登录")
                             )
                    )
        )
    ),
    hidden(div(id = "main_app_ui",
               h2("欢迎进入主界面!"),
               verbatimTextOutput("user_info"),
               actionButton("logout_btn", "退出登录")
    ))
  )
    
  
  # ---------- Server ----------
  server <- function(input, output, session) {
    
    observeEvent(input$btn_show_register, {
      updateTabsetPanel(session, "auth_tabs", selected = "register")
    })
    
    observeEvent(input$go_to_login, {
      updateTabsetPanel(session, "auth_tabs", selected = "login")
    })
    
    observeEvent(input$btn_show_forgot, {
      updateTabsetPanel(session, "auth_tabs", selected = "reset")
    })
    
    
    rv <- reactiveValues(
      logged_in = FALSE,
      username = NULL,
      role = NULL,
      message = NULL
    )
    output$user_info_box <- renderUI({
      if (rv$logged_in) {
        tagList(
          tags$b("当前用户："), rv$username, br(),
          tags$b("角色："), rv$role, br(),
          tags$b("提示："), span(rv$message)
        )
      } else {
        tags$i("未登录")
      }
    })

    
    # 登陆
    observeEvent(input$btn_login, {
      ident <- trimws(input$id_input)
      pw <- input$pw_input
      if (ident == "" || pw == "") {
        showNotification("请输入用户名/邮箱和密码。", type = "error"); return()
      }
      res <- check_login(ident, pw)
      if (isTRUE(res$success)) {
        rv$logged_in <- TRUE
        rv$username <- res$username
        rv$role <- res$role
        rv$message <- "登录成功"
        showNotification(sprintf("欢迎 %s（%s）", rv$username, rv$role), type = "message")
        # go to main tab
        hide("login_register_ui")
        show("main_app_ui")
      } else {
        # 处理失败原因
        reason <- res$reason
        if (reason == "not_found") {
          showNotification("用户不存在（用户名或邮箱错误）。", type = "error")
        } else if (reason == "not_activated") {
          showNotification("账户尚未激活，请先通过激活邮件/链接激活。", type = "warning")
        } else if (reason == "locked") {
          lu <- as.character(res$locked_until)
          showNotification(sprintf("账户已被锁定，解锁时间：%s", lu), type = "error")
        } else if (reason == "bad_password") {
          fa <- res$failed_attempts
          if (!is.null(res$locked_until)) {
            showNotification("密码错误，尝试过多已被锁定（请稍后再试或联系管理员）。", type = "error")
          } else {
            showNotification(sprintf("密码错误（失败次数=%s）", fa), type = "error")
          }
        } else {
          showNotification("登录失败（未知原因）", type = "error")
        }
      }
      # clear pw input for safety
      updateTextInput(session, "pw_input", value = "")
    })
    
    
    #注册
    
    # 注册提交
    observeEvent(input$reg_submit, {
      req(input$reg_username, input$reg_email, input$reg_pw, input$reg_pw2)
      if (input$reg_pw != input$reg_pw2) {
        showNotification("两次密码不一致", type = "error"); return()
      }
      ok <- register_user(input$reg_username, input$reg_email, input$reg_pw)
      if (!ok) {
        showNotification("注册失败：用户名或邮箱已存在。", type = "error")
      } else {
        
        message(sprintf("注册提交成功"))
        showNotification("注册提交成功")

      }
    })
 

    # 修改密码（已登录）
    observeEvent(input$btn_change_pw, {
      if (input$new_pw != input$new_pw2) { showNotification("两次密码不一致", type = "error"); return() }
      ok <- change_password(rv$username, input$old_pw, input$new_pw)
      if (ok) {
        output$change_pw_res <- renderText("密码修改成功")
        showNotification("密码修改成功", type = "message")
      } else {
        output$change_pw_res <- renderText("旧密码错误或修改失败")
        showNotification("旧密码错误或修改失败", type = "error")
      }
    })

    
    
  }
  

shinyApp(ui = ui, server = server)

}

