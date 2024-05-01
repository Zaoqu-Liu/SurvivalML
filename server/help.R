# the server side of help page
output$HelpPageTermListTable <- DT::renderDataTable({
  tbl <- openxlsx::read.xlsx("./doc/help/Clinical_names.xlsx",sheet = 1)
  DT::datatable(tbl,
                options = list(pageLength = 30, info = FALSE))
})