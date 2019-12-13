library(RInno)

create_app(
  app_name = "Machine Learning App", 
  app_dir  = "C:/Users/Python/Documents/AppV3",
  include_R = TRUE,
  include_Rtools = TRUE,
  pkgs     = c("shiny","shinydashboard","shinyjs","shinyWidgets","pracma","highcharter","tidyverse","caret","rapport","foreign","data.table","viridis","rlist","iml","future")  
  )

compile_iss()
