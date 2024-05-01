# this script render the UI of [about us] page

ui.page_about<- function(){
  tabPanel(
    title = "About",
    value = "About",
    
    bootstrapPage(
      htmlTemplate("./www/about.html",name="about") # include bootstrap templates
    )
  )
  
}