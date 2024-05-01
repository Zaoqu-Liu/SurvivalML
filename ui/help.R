# this script render the UI of help page
set_md_path <- function(filename) {
  paste("./doc/help/", filename, sep = "")
}

ui.page_help_usage <- function() {
  tabPanel(
    title = "Usage",
    value = "Usage",
    
    fluidPage(
      id = "HelpPageUsageSubpage",
      shiny::includeMarkdown(set_md_path("usage.md")),
      style = "width:80%;")
    )
}

ui.page_help_termlist <- function() {
  tabPanel(
    title = "Term List",
    value = "Term List",
    
    fluidPage(
      # shiny::includeMarkdown(set_md_path("terms.md")),
      h1("Term List", style = "text-align:center;"),
      br(),
      p("The summary table of abbreviations of clinlical terms is listed below."),
      DT::dataTableOutput(
        outputId = "HelpPageTermListTable"
      ),
      style = "width:80%;")
    )
}

ui.page_help_citation <- function() {
  tabPanel(
    title = "Citation",
    value = "Citation",
    
    fluidPage(
      shiny::includeMarkdown(set_md_path("citation.md")),
      style = "width:80%;")
    )
}

ui.page_help_faq <- function() {
  tabPanel(
    title = "FAQ",
    value = "faq",
    
    fluidPage(
      shiny::includeMarkdown(set_md_path("faq.md")),
      style = "width:80%;")
  )
}

ui.page_help_parameter <- function() {
  tabPanel(
    title = "Parameter Details",
    value = "ParameterDetails",
    
    fluidPage(
      shiny::includeMarkdown(set_md_path("parameter_details.md")),
      style = "width:80%;")
  )
}

ui.page_help_privacy_policy <- function() {
  tabPanel(
    title = "Privacy Policy",
    value = "PrivacyPolicy",
      
    fluidPage(
      shiny::includeMarkdown(set_md_path("privacy_policy.md")),
      style = "width:80%;")
    )
}

ui.page_help_terms_and_conditions <- function(){
  tabPanel(
    title = "Terms and Conditions",
    value = "TermsAndConditions",
    
    fluidPage(
      shiny::includeMarkdown(set_md_path("terms_and_conditions.md")),
      style = "width:80%;")
  )
}
