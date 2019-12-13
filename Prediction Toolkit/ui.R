library(shiny) # CRAN
library(shinyAce) # CRAN
library(shinydashboard) # CRAN
library(shinydashboardPlus) # CRAN
library(shinyjs) # CRAN
library(shinyWidgets) # CRAN
library(DBI) # CRAN
library(highcharter) # CRAN
library(rapport) # CRAN
library(foreign) # CRAN
library(viridis) # CRAN
library(rlist) # CRAN
library(iml) # CRAN
library(shinycssloaders) # CRAN
library(future) # CRAN
library(bsplus) # CRAN
library(skimr) # CRAN
library(DT) # CRAN
library(plyr) # CRAN
library(tidyverse) # CRAN
library(pracma) # CRAN
library(data.table) # CRAN
library(caret) # CRAN 
library(colorspace) # CRAN

# INPUT ---------------------------------------------------------------------------------------

toolkit_name <- "BayWa r.e."
main_color <- "#62ed55"

# ---------------------------------------------------------------------------------------------

saturation <- function(color,scaling) {
  
  color <- rgb2hsv(col2rgb(color))
  color["s", ] <- scaling*color["s", ]
  new_color<-apply(color, 2, function(x) hsv(x[1], x[2], x[3]))
  
  return(new_color)
  
}

# ----------------------------------------------------------------------------------------------

main_color_darker <- saturation(main_color, 1.1)
main_color_hover <-  saturation(main_color, 1.2)
shadow <- paste("rgba(",paste(as.vector(col2rgb(main_color)), collapse = ", "),", 0.6 )",sep="")

# ----------------------------------------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = toolkit_name, # tagList()
                  tags$li(class = "dropdown", div(style="padding: 10px 10px; margin-right: -90px; width: 400px; height: 20px", selectizeInput(inputId = "input_source",label=NULL,choices = list(
                    Relational = c("PostgreSQL","MySQL","MS SQL Server","SQLite"),
                    NonRelational = c("Oracle","Teradata","Hive","Impala","AWS Redshift","Salesforce","MonetDB","Big Query","Athena","MongoDB","Netezza","Cassandra"),
                    Other=c("File Upload","Other")
                  ),
                  selected = "File Upload"
                  ))),
                  tags$li(class = "dropdown",  tags$style(".shiny-file-input-progress {display: none}"), div(style="padding: 10px 10px; margin-right: -90px; width: 400px; height: 20px",uiOutput("data_input"))),
                  tags$li(class = "dropdown",  tags$style(".shiny-file-input-progress {display: none}"), div(style="padding: 10px 10px;",downloadButton('Download', 'Download Model', class="dlButton")))
                  #tags$li(class = "dropdown", div(style="padding-right: 25px",tags$img(src='logos.png', height ="50px")))
                  
                  
  ),
  dashboardSidebar(disable=TRUE),
  dashboardBody(
    #options(spinner.color=main_color),
    tags$head(tags$style(HTML(paste("
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: ",main_color_darker,";
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: ",main_color_hover,";
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: ",main_color,";
                              }
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: ",main_color_hover,";
         }
        .selectize-input.focus {
                              border-color: ",main_color,";
                              outline: 0;
                              -webkit-box-shadow: inset 0 1px 1px rgba(0,0,0,.075), 0 0 8px ",shadow,";
                              box-shadow: inset 0 1px 1px rgba(0,0,0,.075), 0 0 8px ",shadow,";
        }
        .nav-tabs-custom .nav-tabs li.active {
                              border-top-color: ", main_color,";
        }
                              ",sep="")))),
    useShinyjs(),
    tabBox(width = 6,
      title = "",
      id = "dataset_info", height = "400px",
      tabPanel("Data Preview",     withSpinner(DT::dataTableOutput("output_table", height = "600px")) ),
      tabPanel(id="Descriptive",
               title="Feature Plotting",
               div(
                 div(style="padding: 10px 25px;",
                     fluidRow(width=12,
                              column(width=3,selectInput(inputId="variable_x", label="X:", choices=c(""))),
                              column(width=3,selectInput(inputId="variable_y", label="Y:", choices=c(""))),
                              column(width=3,selectInput(inputId="variable_group", label="GROUP:", choices=c(""))),
                              div(style = "padding-top: 27px",
                                  column(width=3, actionButton("update_graph"," ", icon("sync")))
                              )
                     )
                 ),
                 div(style="padding: 10px 25px; margin-top: -2em",
                     hr()
                 ),
                 div(style="padding: 10px 25px;",
                     withSpinner(highchartOutput("descriptive_plot", height = "450px"))
                 )
               ))
    ),
    tabBox(width = 6,
           title = "",
           id = "classification_info", height = "400px",
           tabPanel(title="Prediction Output", 
                    withSpinner(highchartOutput("shareplot", height = "600px")) ),
           tabPanel(title="Model Interpretability",
                    div(style="padding: 10px 25px;",
                        fluidRow(width=12,
                                 column(width=4, selectInput(inputId = "selected_class",label="Select Class to Interpretate:",choices = c(""))),
                                 column(width=4, selectInput(inputId = "selected_case",label="Select Case to Interpretate:",choices = c(1:50)))
                        )
                    ),
                    div(style="padding: 10px 25px; margin-top: -2em",
                        hr()
                    ),
                    div(style="padding: 10px 25px; margin-top: -2em",
                        fluidRow(width=12,
                                 column(width=12,withSpinner(highchartOutput("shapley", height = "475px")))
                        )))
    ),
  )
)
