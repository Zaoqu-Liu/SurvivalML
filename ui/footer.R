ui.footer <- function() {
  tags$footer(tags$hr(),
              HTML("Copyright &copy; 2022"),
              HTML("<span><a href=\"https://rookieutopia.com/app_direct/SurvivalML/\" target=\"_blank\">SurvivalML</a>, All Rights Reserved -</span>"),
              span("An open platform for survival model discovery"),
              tags$br(),
              shinyLink(to = "PrivacyPolicy", label = "Privacy Policy"),
              span(" | "),
              shinyLink(to = "TermsAndConditions", label = "Terms and Conditions"),
              span(" | Covered by "),
              HTML("<span><a href=\"https://creativecommons.org/licenses/by-nc/4.0/\" target=\"_blank\">CC BY-NC License</a></span>"),
              span(" | "),
              HTML("<span><a href=\"https://www.beian.miit.gov.cn/\" target=\"_blank\">沪ICP备18048749号-4</a></span>"),
              span(" | "),
              HTML("<span><a href=\"http://www.beian.gov.cn/portal/registerSystemInfo?recordcode=31011402010247\" target=\"_blank\">公网安备 31011402010247号</a></span>"),
              align = "center", style = "
                           position:relative;
                           bottom:0;
                           width:100%;
                           height:50px;   /* Height of the footer */
                           padding: 10px;
                           z-index: 1000;"
  )
}
