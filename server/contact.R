# This file is the server-end of contact page

user.name <- reactive(
  req(input$ContactPageNameInput, cancelOutput = TRUE)
)
user.email <- reactive(
  req(input$ContactPageEmailInput, cancelOutput = TRUE)
)
user.phone <- reactive(
  req(input$ContactPagePhoneInput, cancelOutput = TRUE)
)
user.subject <- reactive(
  req(input$ContactPageSubjectInput, cancelOutput = TRUE)
)
user.msg <- reactive(
  req(input$ContactPageMsgInput, cancelOutput = TRUE)
)


# action button
observeEvent(input$ContactPageSubmitBtn, {
  
  # check every input using shinyvalidate
  # shinyvalidate only check the data is OK or not, 
  # however it doesn't stop the program when value is not OK,
  # so req() is also needed.
  
  iv <- shinyvalidate::InputValidator$new()
  iv$add_rule("ContactPageNameInput", shinyvalidate::sv_required())
  iv$add_rule("ContactPageEmailInput", shinyvalidate::sv_email())
  iv$add_rule("ContactPagePhoneInput", shinyvalidate::sv_required())
  iv$add_rule("ContactPageSubjectInput", shinyvalidate::sv_required())
  iv$add_rule("ContactPageMsgInput", shinyvalidate::sv_required())
  iv$enable()
  
  # read in database at every click
  user.contact.db <- openxlsx::read.xlsx("./db/contact_user_message.xlsx",sheet = 1)
  
 # print(user.contact.db)
  
  tmp = data.frame(user.name = user.name(),
                   user.email = user.email(),
                   user.phone = user.phone(),
                   user.subject = user.subject(),
                   user.msg = user.msg(),
                   date = date())
  # print(tmp)
  user.contact.db = rbind(user.contact.db, tmp)
  # print(user.contact.db)
  
  # update data
  write.xlsx(user.contact.db, file = "./db/contact_user_message.xlsx")
  
  # check whether send emails to backup user feedback form
  # 用户反馈每积累10条，给管理员发一次邮件。
  if(nrow(user.contact.db) %% 10 == 0){
    # 查询收件人
    receivers <- openxlsx::read.xlsx("./db/maintainer_email.xlsx",sheet = 1, colNames = FALSE)[,1]
    
    # 发邮件
    source("./lib/global.R")
    sendEmail(receivers)
  }
  
  # show modaldialog
  msg.success <-
    modalDialog(
      title = "Message received",
      tags$p("We value every advice. Thank you for helping us to improve the app."),
      easyClose = TRUE,
      
      ##footer
      #footer = tagList(modalButton("OK",icon = icon("check"), class = "btn btn-prim"))
      footer = actionButton("ContactPageMsgModal",
                            label = "OK",
                            icon = icon("check"),
                            class = "btn btn-success",
                            style = "background-color:#0e8daa")
    )
  showModal(msg.success)
})

observeEvent({input$ContactPageMsgModal},{
  removeModal()
})

