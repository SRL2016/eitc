library(shiny)
library(leaflet)
library(shinyWidgets)

load("data.RData")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  span(h1("Colorado EITC Map"),img(src="logo.png",width=200, height=80)),
  tabsetPanel(
    type = "tabs",
    tabPanel("Report Map",
            sidebarLayout(
              position = "right",
              sidebarPanel(width = 2,
                           prettyCheckboxGroup("charities",
                                               label = h4("Community Recources"),
                                               choiceNames = list(
                                                                  div(img(src="charity_icon/house.png", width="18px", height="18px"),"Housing, Shelter"),
                                                                  div(img(src="charity_icon/bene.png", width="18px", height="18px"),"Public, Society Benefit")
                                               ),
                                               choiceValues = list("L","W"),
                                               selected = NULL,
                                               shape = "curve",
                                               outline = TRUE,
                                               inline = TRUE
                           ),
                           checkboxInput("puma",
                                         label = "Estimates Qualified EITC Units",
                                         value = FALSE
                           )
              ),
              mainPanel(
               fluidPage(
                 fluidRow(
                   div(HTML("<br>")),
                   div(style="display: inline-block;vertical-align:top; width: 210px;",
                          selectInput(inputId = "geo", label = NULL,
                                      choices = c("Geographies", names(rpt_lst))
                          )
                   ),
                   div(style="display: inline-block;vertical-align:top; width: 220px;",
                          uiOutput("geo_dropdown")
                   )
                 ),
                 fluidRow(
                   column(12,
                      leafletOutput("map_rpt", width = "100%", height = "700px")
                   )
                 )
               ),
                  div(
                    tags$h3(textOutput("boundary")),
                    fluidRow(
                      column(4,
                             tags$h5(textOutput("ttlhh"))
                      ),
                      column(4,
                             tags$h5(textOutput("hh_per"))
                      )
                    ),
                    fluidRow(
                      column(4,
                             tags$h5(textOutput("ttlfam"))
                      ),
                      column(4,
                             tags$h5(textOutput("fam_per"))
                      )
                    ),
                    fluidRow(
                      column(4,
                             tags$h5(textOutput("ttlrt"))
                      ),
                      column(4,
                             tags$h5(textOutput("eitc_per"))
                      )
                    ),
                    fluidRow(
                      column(4,
                             tags$h5(textOutput("eitcamnt"))
                      )
                    ),
                    h3("Demographics"),
                    fluidRow(
                      column(4,
                             h5(textOutput("pvt"))
                        
                      ),
                      column(4,
                             h5(textOutput("pvt_per"))
                             
                      )
                    ),
                    fluidRow(
                      column(4,
                             h4("Age"),
                             plotOutput('age',  width = "100%", height = "400px")
                      ),
                      column(4,
                             h4("Race/Ethnicity"),
                             plotOutput('race',  width = "100%", height = "400px")
                      ),
                      column(4,
                             h4("Language Spoken at Home"),
                             plotOutput('language',  width = "100%", height = "400px")
                      )
                    )
                  ),
                  div(
                    uiOutput('download')
                  )
                  
                )
              )
    )
    # tabPanel("Demographic Deep DIve",
    #          sidebarLayout(
    #            position = "right",
    #            sidebarPanel(width = 2,
    #               
    #            ),
    #            mainPanel(
    #              fluidPage(
    #                fluidRow(
    #                  div(HTML("<br>")),
    #                  div(style="display: inline-block;vertical-align:top; width: 210px;",
    #                      selectInput(inputId = "demo_ind", label = NULL,
    #                                  choices = c("First, Choose an Subject", as.character(demo_index$ind)[1:13])
    #                      )
    #                  ),
    #                  div(style="display: inline-block;vertical-align:top; width: 220px;",
    #                      htmlOutput("layer")
    #                  ),
    #                  div(style="display: inline-block;vertical-align:top; width: 10px;",
    #                      checkboxInput("hide", label = "hide", value = FALSE)
    #                  )
    #                ),
    #                fluidRow(
    #                  column(12,
    #                         leafletOutput("map", width = "100%", height = "700")
    #                  )
    #                )
    #              )
    #            )
    #          )
    #         )
   )
))
