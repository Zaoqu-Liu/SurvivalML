# this script render the UI of [about us] page

ui.page_contact <- function() {
  tabPanel(
    title = "Contact",
    value = "Contact",
    
    fluidPage(style = "width:80%;",
              tags$br(),
              fluidRow(
                column(
                  6,
                  tags$h2("Contact"),
                  tags$p(
                    "Feedback can be sent to best_utopia@sina.com or by using the form below. We would like to receive feedback on how to improve this resource.",
                    style = "font-size:100%"
                  ),
                  
                  tags$hr(),
                  tags$br(),
                  
                  fluidRow(
                    column(
                      6,
                      # address
                      icon("map-marker", style = "font-size:120%;"),
                      tags$b("Address", style = "color:#000;"),
                      tags$p("Beijing, China.",
                             style = "font-size:100%"),
                      
                      # affiliation
                      icon("hospital-o", style = "font-size:120%;"),
                      tags$b("Affliation", style = "color:#000;"),
                      tags$br(),
                      tags$a(href='http://www.cams.ac.cn/',target="_blank","Peking Union Medical College"),
                      
                      # email
                      tags$p(''),
                      icon("envelope", style = "font-size:120%;"),
                      tags$b("Email address", style = "color:#000;"),
                      tags$br(),
                      tags$a(href = "mailto:best_utopia@sina.com", "best_utopia@sina.com"),
                      tags$p(""),
                      
                      # phone
                      icon("phone", style = "font-size:120%;"),
                      tags$b("Phone number", style = "color:#000;"),
                      tags$p("+86-16696137348",style = "font-size:100%"),
                    ),
                    
                    column(
                      6,
                      # Website
                      icon("wechat", style = "font-size:120%;"),
                      tags$b("WeChat Official Account", style = "color:#000;"),
                      tags$br(),
                      tags$img(src = "wechat_qr_code.png", height = "200px"),
                      tags$p(''),
                      
                      ## bilibili
                      icon("desktop", style = "font-size:120%;"),
                      tags$b("Bilibili station", style = "color:#000;"),
                      tags$br(),
                      tags$a(href = "https://space.bilibili.com/375135306",target="_blank","Welcome to our video blog")
                      
                    )
                  )
                  
                ),
                
                # leave some space
                column(1,
                       tags$p(" ")),
                
                # forms
                column(
                  5,
                  tags$h2("Comment and Feedback"),
                  
                  fluidRow(column(
                    6,
                    textInput(
                      "ContactPageNameInput",
                      label = "",
                      placeholder = "Name",
                      width = "100%"
                    )
                  ),
                  column(
                    6,
                    textInput(
                      "ContactPageEmailInput",
                      label = "",
                      placeholder = "Email",
                      width = "100%"
                    )
                  ),),
                  
                  fluidRow(column(
                    6,
                    textInput(
                      "ContactPagePhoneInput",
                      label = "",
                      placeholder = "Institution",
                      width = "100%"
                    )
                  ),
                  column(
                    6,
                    textInput(
                      "ContactPageSubjectInput",
                      label = "",
                      placeholder = "Subject",
                      width = "100%"
                    ),
                  ),),
                  
                  textAreaInput(
                    "ContactPageMsgInput",
                    label = "",
                    placeholder = "Message",
                    width = "100%",
                    rows = 7
                  ),
                  
                  actionButton(
                    "ContactPageSubmitBtn",
                    label = "Submit",
                    icon = icon("check"),
                    class = "btn btn-success",
                    style = "background-color:#0e8daa;"
                  )
                  
                )
              ))
  )
  
}
