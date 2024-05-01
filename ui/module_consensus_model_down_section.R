# this script render the UI of analysis page of module single gene


# set up with UI
ui.page_module_consensus_model_down_section <- function() {
  fluidPage(
    ## tab pages
    fluidRow(
      tags$head(
        tags$style(HTML("
           #ConsensusModelModulePageAnalysisType.nav li {
           width: 14vw;
         }
       "))),
      
      bs4Dash::tabsetPanel(
        id = "ConsensusModelModulePageAnalysisType",
        vertical = TRUE,
        side = "left",
        type = "pills",
        tabPanel(
          title = "Clinical association",
          ui.page_module_cm_clinical_association(),
          tags$style(HTML("
               .nav-pills .nav-link.active, .nav-pills .show>.nav-link {
                 color: #fff;
                 background-color: #09a9a2;
               }
          "))
        ),
        tabPanel(
          title = "Survival analysis",
          ui.page_module_cm_prognostic_analysis()
        ),
        tabPanel(
          title = "Enrichment analysis",
          ui.page_module_cm_functional_analysis()
        ),
        tabPanel(
          title = "Cell infiltration",
          ui.page_module_cm_cell_infiltration()
        ),
        tabPanel(
          title = "Immunomodulator",
          ui.page_module_cm_immunomodulator()
        ),
        tabPanel(
          title = "Immunotherapy",
          ui.page_module_cm_immunotherapy()
        ),
        tabPanel(
          title = "Candidate agents",
          ui.page_module_cm_candidate_agents()
        ),
        tabPanel(
          title = "Genomic alteration",
          ui.page_module_cm_genomic_alteration()
        )
      )
    )
  )
}
