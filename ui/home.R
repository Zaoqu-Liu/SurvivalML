# this script render the UI of home page
# connected to ./server/home.R

# read in home page introduction
home.intro <- read_lines("./db/home_introduction.txt")
home.intro.html <- paste(home.intro, "</p>  <p>", collapse = " ")
home.intro.html <- paste("<p>", home.intro.html)
home.intro.html <- str_replace(home.intro.html, "</p>  <p>$", "</p>")
home.intro.html <- str_remove_all(home.intro.html, "<p>\\s+</p>")
#print(home.intro.html)

# read in news and updates
home.news.and.updates <- read_lines("./db/home_news_and_updates.txt")
home.news.and.updates.html <- paste(home.news.and.updates, "</p>  <p>", collapse = " ")
home.news.and.updates.html <- paste("<p>", home.news.and.updates.html)
home.news.and.updates.html <- str_replace(home.news.and.updates.html, "</p>  <p>$", "</p>")
home.news.and.updates.html <- str_remove_all(home.news.and.updates.html, "<p>\\s+</p>")


# read in citation
home.citation <- read_lines("./db/home_citation.txt")
home.citation.html <- paste(home.citation, "</p>  <p>", collapse = " ")
home.citation.html <- paste("<p>", home.citation.html)
home.citation.html <- str_replace(home.citation.html, "</p>  <p>$", "</p>")
home.citation.html <- str_remove_all(home.citation.html, "<p>\\s+</p>")


# home page carousel images
home.carousel.images = list.files("./www/home_carousel/") # all imgs contained in this path will be explored
home.carousel.images.url = paste("./www/home_carousel/", home.carousel.images, sep = "")

# read in primary site list
primary.site = openxlsx::read.xlsx("./db/primary_site_datasets_summary.xlsx", sheet = 1)
#card.color = rep(RColorBrewer::brewer.pal(12,"Paired"),5)[1:nrow(primary.site)]


# set up with UI
ui.page_home <- function() {
  tabPanel(
    title = "Home",
    value = "Home",

    # create icon http://shiny.rstudio.com/reference/shiny/latest/icon.html
    fluidPage(
      style = "width:80%;",
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "./static/css/mystyle.css")
      ),
      tags$br(),
      div(
        img(src = "logo_single_small.png", height = 100, width = 85, style="padding-top:0px;padding-bottom:0px;"),
        p("SurvivalML", style = "font-weight:bold;font-size:220%;color:#276e7f;padding-bottom:0px;margin-bottom:0px;font-family:\"Comic Sans MS\", \"Comic Sans\", cursive;"),
        tags$p("Identification and Validation of Robust Prognostic Biomarkers in Solid Tumors",
               style = "font-size:110%;color:#999;font-style:italic;padding-top:0px;margin-top:0px;"),

        style = "text-align:center;"
      ),

      #----section1-----
      fluidRow(
        # column(
        #   6,
        #   tags$div(
        #     div(
        #       HTML(home.intro.html),
        #       class = "scrollbar-hidden",
        #       style = "height:400px; overflow-y:auto;color:white; font-size:115%;text-align:justify;padding:30px 50px 30px 50px;border-radius:10px;background-color:#296D7F;"
        #     ),
        #
        #     id = "HomePageIntroduction",
        #     style = "display:flex;align-items:center;height:450px;box-shadow:5px 5px 10px 5px #ccc;border-radius:10px;background-color:#296D7F;"
        #   )
        # ),
        column(
          12,
          div(
            div(
              slickR(obj = home.carousel.images.url,
                     height = 390,
                     width = "95%") +
                settings(dots = FALSE, autoplay = FALSE),
            ),
            style = "height:450px;box-shadow:5px 5px 10px 5px #ccc;padding:20px 0px 20px 0px;border-radius:10px;"
          )
        )
      ),

      tags$br(),
      tags$hr(),
      tags$br(),

      #---------section2------
      div(
        useShinyjs(),

        # a hidden element link to module selection section
        tags$a("to module selection",
               href="#PageHomeAnalysisModuleSelection",
               id = "PageHomeLinkToModuleSelection",
               style = "display:none"),

        fluidRow(
          column(
            6,
            div(
              p("Select Your Cancer Type",
                style = "color:#ffffff; font-size:150%; font-weight:bold; margin-bottom:0px;"),
              class = "text-box",
              style = "width:330px;"
            )
          ),
          column(
            6
          )
        ),

        # space
        tags$br(),

        # organ selection
        fluidRow(
          column(
            width = 4,
            # dropdown mean
            selectInput(
              inputId = "DatasetOrganSelect",
              label = "Select a specific cancer",
              choices = c(primary.site$Cancer),
              width = "80%"
            ),
            # plot of organ
            htmlOutput(outputId = "DatasetOrgan"),

          ),
          column(
            width = 8,
            tags$br(),

            div(
              fluidRow(
                width = 12,
                # generate card for each primary site
                lapply(1:nrow(primary.site), function(i){
                  column(
                    width = 4,
                    div(
                      useShinyjs(),
                      id = paste("DatasetPageBlock",i,sep=""),
                      div(
                        id = paste("DatasetPageInnerBlock",i,sep=""),
                        div(
                          id = paste("DatasetPageInnerInnerBlock",i,sep=""),
                          div(
                            id = paste("DatasetPageInnerInnerInnerBlock",i,sep=""),
                            div(
                              fluidRow(
                                column(4, img(src = paste("./organ/organ_small/",
                                                          str_replace_all(primary.site$Abbreviation[i]," +","_"), # replace space with _
                                                          ".png",
                                                          sep=""))),
                                column(
                                  8,
                                  h5(primary.site$Abbreviation[i],
                                     style="font-weight:bold;margin-bottom:0px;"),
                                  p(paste(
                                    primary.site$Datasets.Number[i], " datasets"),
                                    style="padding-bottom:0px;margin-bottom:0px;font-size:100%;"),
                                  p(paste(
                                    primary.site$Sample.Number[i], " samples"),
                                    style="padding-top:0px;margin-top:0px;margin-bottom:1px;font-size:100%;")
                                )
                              ),
                              style = "padding-left:15px;padding-top:15px;"
                            )
                          )
                        )
                      )
                    ),
                    tags$br()
                  )
                })
              ),
              style = "height:580px;overflow-y:scroll;overflow-x:hidden;"
            )
          )
        ),

        tags$br(),

        # section box style
        style = "box-shadow:2px 2px 5px 2px #ccc;padding-left:40px;padding-right:40px;padding-top:30px;border-radius:10px;border-width:2px;border-color:#296D7F;"
      ),

      tags$br(),
      tags$hr(),
      tags$br(),

      ##------section3-----
      div(
        # section title
        fluidRow(
          id = "PageHomeAnalysisModuleSelection",
          column(
            6,
            div(
              p("Determine Analysis Module",
                style = "color:#ffffff;font-size:150%;font-weight:bold; margin-bottom:0px;"),
              class = "text-box",
              style = "width:350px;"
            )

          ),
          column(
            6
          )
        ),

        tags$br(),

        # module 1
        # fluidRow(
        #   column(
        #     6,
        #     div(
        #       useShinyjs(),
        #       id = "HomePageAnalysisModule1",
        #       div(
        #         id = "HomePageAnalysisModule1Inner",
        #         div(
        #           id = "HomePageAnalysisModule1InnerInner",
        #           fluidRow(
        #             column(2,img(src = "model1.png", width = 120)),
        #             column(10,
        #                    h5("Single Gene"),
        #                    p("Users can enter a gene symbol or an Ensembl ID to explore a gene of interest.",
        #                      style="font-size:95%;line-height:1.2;color:#696969;"))
        #           ),
        #           style="padding-left:10px;padding-top:10px;padding-bottom:10px;padding-right:10px;color:#ffffff;"
        #         )
        #       ),
        #       # style = "background:#4ea65220;"
        #     )
        #   ),
        #
        #   # module 2
        #   column(
        #     6,
        #     div(
        #       useShinyjs(),
        #       id = "HomePageAnalysisModule2",
        #       div(
        #         id = "HomePageAnalysisModule2Inner",
        #         div(
        #           id = "HomePageAnalysisModule2InnerInner",
        #           fluidRow(
        #             column(2,img(src = "model2.png", width = 120)),
        #             column(10,
        #                    h5("Gene List"),
        #                    p("Users can enter a list of genes and pick a method to calculate the gene set score for each sample. The embedded methods include GSVA, ssGSEA, z-score, PLAGE, and the mean value.",
        #                      style="font-size:95%;line-height:1.2;color:#696969;"))
        #           ),
        #           style="padding-left:10px;padding-top:10px;padding-bottom:0px;padding-right:10px;"
        #         )
        #       ),
        #       # style = "background:#0aa9a220; "
        #     )
        #   )
        # ),

        # space
        # tags$br(),

        fluidRow(

          # module 3
          column(
            6,
            div(
              useShinyjs(),
              id = "HomePageAnalysisModule3",
              div(
                id = "HomePageAnalysisModule3Inner",
                div(
                  id = "HomePageAnalysisModule3InnerInner",
                  fluidRow(
                    column(2,img(src = "model3.png", width = 120)),
                    column(10,
                           h5("Specific Model"),
                           p("Users can fit a survival machine learning model based on a set of genes and explore its clinical value and biological implications.",
                             style="font-size:95%;line-height:1.2;color:#696969;"))
                  ),
                  style="padding-left:10px;padding-top:10px;padding-bottom:10px;padding-right:10px;"
                )
              ),
              # style = "background:#ff6b6b20;"
            )
          ),

          # module 4
          column(
            6,
            div(
              useShinyjs(),
              id = "HomePageAnalysisModule4",
              div(
                id = "HomePageAnalysisModule4Inner",
                div(
                  id = "HomePageAnalysisModule4InnerInner",
                  fluidRow(
                    column(2,img(src = "model4.png", width = 120)),
                    column(10,
                           h5("Consensus Model"),
                           p("Users can generate a consensus machine learning-derived model with best power for predicting prognosis based on 10 prevalent machine learning algorithms.",
                             style="font-size:95%;line-height:1.2;color:#696969;"))
                  ),
                  style="padding-left:10px;padding-top:10px;padding-bottom:10px;padding-right:10px;"
                )
              ),
              # style = "background:#eac42820;"
            )
          )
        ),

        # section box style
        style = "box-shadow:2px 2px 5px 2px #ccc;padding-bottom:30px;padding-left:40px;padding-right:40px;padding-top:30px;border-radius:10px;border-width:2px;border-color:#296D7F;"

      ),


      tags$br(),
      # tags$br(),

      fluidRow(
        column(
          3,
          div(
            # 该内容不做展示，仅用于数据存储及传输
            selectInput(
              inputId = "HomePageAnalysisTypeSelection",
              label = "",

              # there are totally 4 modules
              # 1 for single gene
              # 2 for gene list
              # 3 for prognosis model
              # 4 for consensus subtype
              choices = 3:4,
              selected = 3, # prognostic model
            ),
            style="display:none;"
          )
        ),
        column(
          6,

          div(
            id = "HomePageAnalysisGoBtnDiv",

            # div(
            #   id = "HomePageAnalysisGoBtnDivModule1",
            #
            #   shinyWidgets::actionBttn(
            #     inputId = "HomePageAnalysisGoBtn1",
            #
            #     # default to single gene
            #     label = shinyLink(to = "SingleGene",label = "Go Explore BEST"),
            #     icon = icon("rocket"),
            #     style = "material-flat",
            #     color = "primary",
            #     block = TRUE
            #   ),
            #
            #   # hide first
            #   class = "hidden-element"
            # ),
            #
            # div(
            #   id = "HomePageAnalysisGoBtnDivModule2",
            #
            #   shinyWidgets::actionBttn(
            #     inputId = "HomePageAnalysisGoBtn2",
            #
            #     # default to single gene
            #     label = shinyLink(to = "GeneList",label = "Go Explore BEST"),
            #     icon = icon("rocket"),
            #     style = "material-flat",
            #     color = "primary",
            #     block = TRUE
            #   ),
            #
            #   # hide first
            #   class = "hidden-element"
            # ),

            div(
              id = "HomePageAnalysisGoBtnDivModule3",

              shinyWidgets::actionBttn(
                inputId = "HomePageAnalysisGoBtn3",

                # default to single gene
                label = shinyLink(to = "PrognosticModel",label = "Go Explore SurvivalML"),
                icon = icon("rocket"),
                style = "material-flat",
                color = "primary",
                block = TRUE
              ),

              # hide first
              class = "hidden-element"
            ),

            div(
              id = "HomePageAnalysisGoBtnDivModule4",

              shinyWidgets::actionBttn(
                inputId = "HomePageAnalysisGoBtn4",

                # default to single gene
                label = shinyLink(to = "ConsensusModel",label = "Go Explore SurvivalML"),
                icon = icon("rocket"),
                style = "material-flat",
                color = "primary",
                block = TRUE
              ),

              # hide first
              class = "hidden-element"
            )

            # shiny::actionButton(
            #   inputId = "HomePageAnalysisGoBtn",
            #   label = shinyLink(
            #     to = "Dataset", label = "Dataset Page"),
            #   icon = icon("rocket"),
            #   style = "background-color:#0e8daa; color:#ffffff;"
            # )
          )
        ),
        column(
          3
        )
      ),

      # space
      tags$br(),
      tags$hr(),
      tags$br(),


      #### section 4 news and update ####
      div(
        bs4Dash::box(

          div(
            HTML(home.news.and.updates.html),
            style="height:170px;overflow-y:scroll;border:1px solid #cecece;padding:10px 20px 10px 20px;text-align:justify;"
          ),


          # box configure
          id = "HomePageLastInfoCard",
          title = "News and updates",
          solidHeader = FALSE,
          height = "200px",
          closable = FALSE,
          maximizable = FALSE,
          width = 12,
          collapsible = FALSE,
          icon = icon("dove")
        ),

        style = "box-shadow:2px 2px 5px 2px #ccc;"
      ),


      tags$br(),
      #### section 5 citation ####
      div(
        bs4Dash::box(

          div(

            HTML(home.citation.html),

            style="height:170px;overflow-y:scroll;border:1px solid #cecece;padding:10px 20px 10px 20px;text-align:justify;"
          ),

          # box configure
          id = "HomePageLastInfoCard",
          title = "Citation",
          solidHeader = FALSE,
          height = "200px",
          closable = FALSE,
          maximizable = FALSE,
          width = 12,
          collapsible = FALSE,
          icon = icon("book")
        ),

        style = "box-shadow:2px 2px 5px 2px #ccc;"
      ),

      tags$br(),
      tags$br()


    )
  )
}
