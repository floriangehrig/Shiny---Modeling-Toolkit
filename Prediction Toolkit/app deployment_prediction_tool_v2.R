library(RInno)

# SET DIRECTORY
setwd(paste("C:/Users/",Sys.info()[["user"]],"/Documents/GitHub/Analytics-R/Pipelines & Toolkits/Machine Learning Toolkit/Prediction Toolkit", sep=""))

# TEST APPLICATION
shiny::runApp(paste("C:/Users/",Sys.info()[["user"]],"/Documents/GitHub/Analytics-R/Pipelines & Toolkits/Machine Learning Toolkit/Prediction Toolkit", sep=""))

# DEPLOY APPLICATION
create_app(
  app_name = "Prediction Toolkit", 
  app_dir  = paste("C:/Users/",Sys.info()[["user"]],"/Documents/Prediction Toolkit",sep=""),
  include_R = TRUE,
  #privilege   = "high",   # Admin only installation
  #default_dir = "autopf",
  #user_browser="firefox",
  # app_icon   = "new_app_icon.ico", # <--- TO ADAPT; 
  # setup_icon = "new_setup_icon.ico",
  pkgs     = c("shiny",
               "shinyAce",
               "shinydashboard",
               "shinydashboardPlus",
               "shinyjs",
               "shinyWidgets",
               "DBI",
               "highcharter",
               "rapport",
               "foreign",
               "viridis",
               "rlist",
               "iml",
               "shinycssloaders",
               "future",
               "bsplus",
               "skimr",
               "shinyhelper",
               "DT",
               "plyr",
               "tidyverse",
               "pracma",
               "data.table",
               "caret",
               "colorspace")  
)

compile_iss()
