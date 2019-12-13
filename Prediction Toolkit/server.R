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

# FUNCTIONS -----------------------------------------------------------------


colors<-rep(c('#2f7ed8', '#0d233a', '#8bbc21', '#910000', '#1aadce', '#492970', '#f28f43', '#77a1e5', '#c42525', '#a6c96a'),times=100)

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
remove_all_outliers <- function(df){
  
  a<-df[,sapply(df, is.numeric), drop = FALSE]
  b<-df[,sapply(df, Negate(is.numeric)), drop = FALSE]
  
  a[]<-lapply(a, function(x) remove_outliers(x))
  
  d<-cbind(a,b)
  d[, names(df)]
  
}

descriptive_plot <- function(data, variable_y, variable_x, variable_group) {
  
  plot <- NULL
  
  if (is.numeric(data[,isolate(variable_y)]) == TRUE) { # Y Variable == NUMERIC
    
    if (isolate(variable_x) == "NONE") { # + X Variable == NONE --> DENSITY / HISTOGRAM
      
      if (isolate(variable_group) == "NONE") { # + GROUP Variable == NONE --> SINGLE DENSITY
        
        plot <- hchart(density(data[,isolate(variable_y)]), type = "area", name = isolate(variable_y))
        
      } else if (is.numeric(data[,isolate(variable_group)]) == FALSE) { # + GROUP Variable == CATEGORICAL --> GROUPED DENSITY 
        
        ds <- map(levels(data[,isolate(variable_group)]), function(x){
          dt <- density(data[,isolate(variable_y)][data[,isolate(variable_group)] == x])[1:2]
          dt <- list_parse2(as.data.frame(dt))
          list(data = dt, name = x, type = "area")
        })
        
        plot<-highchart() %>% 
          hc_add_series_list(ds)
        
      } else {  # + GROUP Variable == NUMERICAL --> NO PLOTTING POSSIBLE
        
        print("No Plotting Possible with X = NONE | Y = NUMERIC | GROUP = NUMERIC")
        
      }
      
    } else if (is.numeric(data[,isolate(variable_x)]) == FALSE) {  # + X Variable == CATEGORICAL --> BOXPLOT / VIOLIN 
      
      if (isolate(variable_group) == "NONE") { # + GROUP Variable == NONE --> SINGLE BOXPLOT
        
        data <- data[,c(isolate(variable_x), isolate(variable_y))]
        
        plot<-hcboxplot(x = data[,isolate(variable_y)], var = data[,isolate(variable_x)], outliers = FALSE) %>% 
          hc_chart(type = "column") # to put box vertical
        
      } else if (is.numeric(data[,isolate(variable_group)]) == FALSE) {  # + GROUP Variable == CATEGORICAL --> GROUPED BOXPLOT 
        
        data <- data[,c(isolate(variable_x), isolate(variable_y), isolate(variable_group))]
        
        plot<-hcboxplot(x = data[,isolate(variable_y)], var = data[,isolate(variable_x)], var2 = data[,isolate(variable_group)], outliers = FALSE) %>% 
          hc_chart(type = "column") # to put box vertical
        
      } else {  # + GROUP Variable == NUMERICAL --> NO PLOTTING POSSIBLE
        
        print("No Plotting Possible with X = CATEGORICAL | Y = NUMERIC | GROUP = NUMERIC")
        
      }
      
    } else {   # + X Variable == NUMERICAL --> SCATTER
      
      if (isolate(variable_group) == "NONE") {  # + GROUP Variable == NONE --> SINGLE SCATTER
        
        data <- data[,c(isolate(variable_x), isolate(variable_y))]
        #data <- remove_all_outliers(data)
        
        x <- isolate(variable_x)
        y <- isolate(variable_y)
        
        plot<-hchart(data, "bubble", hcaes(x = !! x, y = !! y, z=1), regression = TRUE) %>%
          hc_colors(colors[1:1]) %>%
          hc_add_dependency("plugins/highcharts-regression.js") %>%
          hc_plotOptions(
            line = list(enableMouseTracking = FALSE),
            bubble = list(
              marker = list(fillOpacity = 0.3,lineWidth=0),
              minSize = 7, maxSize = 7
            ))
        
      } else if (is.numeric(data[,isolate(variable_group)]) == FALSE) {  # + GROUP Variable == CATEGORICAL --> GROUPED SCATTER
        
        data <- data[,c(isolate(variable_x), isolate(variable_y), isolate(variable_group))]
        #data <- remove_all_outliers(data)
        
        data[,isolate(variable_group)] <- as.character(data[,isolate(variable_group)])
        print(str(data))
        x <- isolate(variable_x)
        y <- isolate(variable_y)
        group <- isolate(variable_group)
        
        
        plot<-hchart(data, "bubble", hcaes(x = !! x, y = !! y, z=1, group = !! group), regression = TRUE) %>%
          hc_colors(colors[1:length(unique(data[,isolate(variable_group)]))]) %>%
          hc_add_dependency("plugins/highcharts-regression.js") %>%
          hc_plotOptions(
            line = list(enableMouseTracking = FALSE),
            bubble = list(marker = list(fillOpacity = 0.3,lineWidth=0), minSize = 7, maxSize = 7))
        
      } else {  # + GROUP Variable == NUMERICAL --> NO PLOTTING POSSIBLE
        
        print("No Plotting Possible with X = NUMERIC | Y = NUMERIC | GROUP = NUMERIC")
        
      }
      
    }
    
  } else { # Y Variable == CATEGORICAL
    
    if (isolate(variable_x) == "NONE") { # + X Variable == NONE --> BAR
      
      if (isolate(variable_group == "NONE")) { # + GROUP Variable == NONE --> SINGLE BAR
        
        plot<-hchart(data[,isolate(variable_y)], type = "column")
        
      } else if (is.numeric(data[,isolate(variable_group)]) == FALSE) {  # + GROUP Variable == CATEGORICAL --> GROUPED BAR
        
        print(data)
        
        ds <- map(levels(data[,isolate(variable_group)]), function(x){
          
          dt <- plyr::count(data[,isolate(variable_y)][data[,isolate(variable_group)] == x])[1:2]
          
          print(dt)
          
          dt <- list_parse2(as.data.frame(dt))
          list(data = dt, name = x, type = "column")
          
        })
        
        plot<-highchart() %>% 
          hc_add_series_list(ds) %>%
          hc_xAxis(categories = unique(data[,isolate(variable_y)]))
        
      } else { # + GROUP Variable == NUMERIC --> NO PLOTTING POSSIBLE
        
        print("No Plotting Possible with X = NONE | Y = CATEGORICAL | GROUP = NUMERIC")
        
      }
      
    } else if (is.numeric(data[,isolate(variable_x)]) == FALSE) { # + X VARIABLE == CATEGORICAL --> CROSS-TABLE
      
      data <- data[,1:length(names(data))-1]
      
      x <-isolate(variable_x)
      y<-isolate(variable_y)
      
      
      data <- data %>% dplyr::group_by_(.dots = c(isolate(variable_x),isolate(variable_y))) %>% dplyr::summarise(count=n())
      data <- as.data.frame(data) %>% mutate_if(is.factor, as.character)
      
      print(data)
      
      plot<-highchart() %>% hc_add_series(data = data, type = "heatmap", hcaes(x = !! x, y = !! y, value = count)) %>%
        hc_xAxis(categories = unique(data[,isolate(variable_x)]), title = NULL) %>%
        hc_yAxis(categories = unique(data[,isolate(variable_y)]), title = NULL) %>%                  
        hc_plotOptions(
          series = list(
            boderWidth = 0,
            dataLabels = list(enabled = TRUE)
          )) %>% 
        hc_colorAxis(min = min(data$count), max = max(data$count), stops = color_stops(10, rev(inferno(10))), type = "logarithmic")
      
    } else {  # + X VARIABLE == NUMERIC --> BOXPLOTS
      
      if (isolate(variable_group) == "NONE") { # + GROUP Variable == NONE --> SINGLE BOXPLOT
        
        data <- data[,c(isolate(variable_x), isolate(variable_y))]
        
        plot<-hcboxplot(x = data[,isolate(variable_x)], var = data[,isolate(variable_y)], outliers = FALSE) %>% 
          hc_chart(type = "bar") # to put box vertical
        
      } else if (is.numeric(data[,isolate(variable_group)]) == FALSE) {  # + GROUP Variable == CATEGORICAL --> GROUPED BOXPLOT 
        
        data <- data[,c(isolate(variable_x), isolate(variable_y), isolate(variable_group))]
        
        plot<-hcboxplot(x = data[,isolate(variable_x)], var = data[,isolate(variable_y)], var2 = data[,isolate(variable_group)], outliers = FALSE) %>% 
          hc_chart(type = "bar") # to put box vertical
        
      } else {  # + GROUP Variable == NUMERICAL --> NO PLOTTING POSSIBLE
        
        print("No Plotting Possible with X = NUMERIC | Y = CATEGORICAL | GROUP = NUMERIC")
        
      }
      
    }
    
  }
  
  plot <- plot %>% hc_size(height="350px") 
  
  return(plot)
  
}

shapley_plot <- function(target_variable,train_model, test_data, selected_model, selected_case, selected_class=NULL) {
  
  library("iml")
  fitted_model <- train_model[[selected_model]]
  y <- train_model[["preprocessing_configuration"]]$target_variable
  test_X <- test_data %>% select(- !! y)
  test_y <- target_variable
  
  predictor <- Predictor$new(fitted_model, data = test_X, y = test_y)
  
  shapley_findings <- Shapley$new(predictor, x.interest = test_X[as.integer(selected_case),])
  
  shapley_values <- shapley_findings$results
  
  if(is.numeric(target_variable) == FALSE) {
    
    shapley_values$class <- as.character(shapley_values$class)
    
  }
  
  shapley_values <- shapley_values %>% mutate(direction = case_when(phi >= 0 ~ '#63ff85', phi<0 ~ '#fc6a60')) %>% arrange(desc(phi))
  
  if(is.numeric(target_variable) == TRUE) {
    
    plot <- hchart(shapley_values, "bar", hcaes(x=feature.value, y=phi, color=direction)) %>%
      hc_plotOptions(bar=list(colorByPoint = TRUE)) %>%
      hc_xAxis(title = list(enabled=FALSE)) %>%
      hc_size(height = "300px") %>%
      hc_title(text=NULL)
    
  } else {
    
    plot <- hchart(shapley_values[shapley_values[["class"]] ==selected_class,], "bar", hcaes(x=feature.value, y=phi, color=direction)) %>%
      hc_plotOptions(bar=list(colorByPoint = TRUE)) %>%
      hc_xAxis(title = list(enabled=FALSE)) %>%
      hc_size(height = "300px")  %>%
      hc_title(text=NULL)
    
  }
  
  return(plot)
  
}

server <- function(input, output, session) {
  
  addClass(selector = "body", class = "sidebar-collapse")

  output$data_input <- renderUI({
    
    if (input$input_source == "File Upload") {
      
      fileInput(inputId = "upload", label=NULL)
      
    } else {
      
      actionButton("show", "Connect to Database...", icon=icon("database"), width="300px")
      
    }
    
  }) 
  rv <- reactiveValues(queried_data = NULL)
  popupModal <- function(failed = FALSE) {
    modalDialog(
      fluidPage(
        fluidRow(
          column(width =4, div(
            uiOutput("connection_status"),
            hr(),
            textInput("database",label = "DATABASE NAME:",value = input_model()[["preprocessing_configuration"]]$database),
            textInput("host",label = "HOSTNAME / URL:",value = input_model()[["preprocessing_configuration"]]$host),
            textInput("user",label = "USER NAME:", value = input_model()[["preprocessing_configuration"]]$user),
            passwordInput("password",label = "PASSWORD:",value = ""),
            actionButton(inputId = "connect_db", label="Connect to Database", icon =icon("exchange-alt"))
          )),
          column(width=8,div(
            strong("WRITE YOUR SQL QUERY:"),
            hr(),
            shinyAce::aceEditor("sql_query", mode = "sql", value=input_model()[["preprocessing_configuration"]]$sql_query)))
        )
      ),
      footer = tagList(modalButton("Cancel"),
                       actionButton("ok", "Query Database", icon = icon("file-export"))
      )
    )
  }
  observeEvent(input$show, {
    updateTextInput(session, "database", value = input$database)
    updateTextInput(session, "host", value = input$host)
    updateTextInput(session, "user", value = input$user)
    showModal(popupModal())
    output$connection_status <- renderUI({dashboardLabel("Not Connected...", status = "primary")})
  })
  
  #DATABASE CONNECTION
  connector <- reactive({
    
    if(input$connect_db == 0) {
      
      return(NULL)
      
    } else {
      
      conn <- try(dbConnect(
        RPostgreSQL::PostgreSQL(),
        dbname = input$database,
        host = input$host,
        port = 5432,
        user = input$user,
        password = input$password)
      )
      
      return(conn)
      
    }
    
  })
  observeEvent(input$connect_db, {
    
    if (class(connector()) == "try-error" | exists("connector") == FALSE) {
      
      output$connection_status <- renderUI({
        dashboardLabel("Connection Failed!", status = "danger")
      })
      
    } else {
      
      output$connection_status <- renderUI({
        dashboardLabel("Connection Successful!", status = "success")
      })
      
    } 
    
  })
  
  # DATABASE QUERY
  observeEvent(input$ok, {
    
    if (is.null(connector()) | class(connector()) == "try-error") {
      
      print("NO CONNECTION POSSIBLE")
      sendSweetAlert(
        session = session,
        title = "DATABASE QUERY FAILED!",
        text = "Unable to query Database. Please check the configuration details.",
        type = "error"
      )
      
    } else {
      
      print("CONNECTION SUCCESSFUL")
      rv$queried_data  <- dbGetQuery(connector(),sub("\n"," ",input$sql_query))
      lapply(dbListConnections(dbDriver(drv = input$input_source)), dbDisconnect)
      output$connection_status <- renderUI({dashboardLabel("Not Connected...", status = "primary")})
      sendSweetAlert(
        session = session,
        title = "DATABASE QUERY SUCCESSFUL!",
        text = "Query was sent out to the Database. Please check the output.",
        type = "success"
      )
      removeModal()
      
    }
  })
  
  input_data <- reactive({
    
    if(is.null(rv$queried_data) == FALSE & input$input_source != "File Upload") {
      
      read <- rv$queried_data
      
    } else {
      
      upload<-req(input$upload)
      
      if(grepl(".sav", upload$datapath) == T) {
        
        read<-read.spss(upload$datapath, to.data.frame=TRUE, max.value.labels = 20)
        
      } else if (grepl(".csv",upload$datapath) == T) {
        
        read<-read.csv(upload$datapath, sep=";")
        
      }
      
    }
    
    return(read)
    
  })
  
  input_model <- reactive({
    
    # MODEL CONFIGURATIONS
    
    unzipped <- unz(paste(getwd(),"/Model_Data.zip",sep=""), filename = "model_configuration.rds")
    temp_zip <- gzcon(unzipped)
    
    model_config <- readRDS(temp_zip)
    
    close(temp_zip)
    
    # PREPROCESSING CONFIGURATIONS
    unzipped <- unz(paste(getwd(),"/Model_Data.zip",sep=""), filename = "preprocessing_configuration.rds")
    temp_zip <- gzcon(unzipped)
    
    preproc_config <- readRDS(temp_zip)
    
    close(temp_zip)
    
    input_model <- list(model_configuration = model_config, preprocessing_configuration = preproc_config)
    
    return(input_model)
    
  })
  
  data_preprocessed <-reactive({
    
    df <- input_data()
    configurations <- input_model()[["preprocessing_configuration"]]
    
    set.seed(configurations$seed)
    
    df <- df[,configurations$selected_vars]
    df <- as.data.table(df)
    
    # NA IMPUTATION
    for(k in names(df)) {
      
      if(is.numeric(df[[k]]) == TRUE) {
        
        if (configurations$na_imputation_method_num == "Median") {
          
          med <- median(df[[k]],na.rm = T)
          set(x = df, which(is.na(df[[k]])), k, med)
          
        } else if (configurations$na_imputation_method_num == "Mean") {
          
          mean <- mean(df[[k]],na.rm = T)
          set(x = df, which(is.na(df[[k]])), k, mean)    
          
        } else {
          
        }
        
      } else if(is.numeric(df[[k]]) == FALSE) {
        
        if (configurations$na_imputation_method_cat == "Mode") {
          
          mode <- names(which.max(table(df[[k]])))
          set(x = df, which(is.na(df[[k]])), k, mode)
          
        } else if (configurations$na_imputation_method_cat == "Random") {
          
          random <- sample(unique(df[[k]]), 1)
          set(x = df, which(is.na(df[[k]])), k, random)  
          
        } else {
          
        }
        
      }
      
    }
    
    # OUTLIER TREATMENT
    
    for(k in names(df)) {
      
      if(is.numeric(df[[k]]) == TRUE) {
        
        if (configurations$outlier_imputation_method== "Median") {
          
          df[] <- lapply(df, outlier_imputation, mode="Median")
          
        } else if (configurations$outlier_imputation_method == "Mean") {
          
          df[] <- lapply(df, outlier_imputation, mode="Mean")
          
        } else {
          
        }
      }
    }
    
    set.seed(42)
    df <- as.data.frame(df)
    
    # CATEGORICAL VARIABLE ENCODING
    if(configurations$encoding_method == "Dummy") {
      
      dmy <- dummyVars(" ~ .", data = df[names(df) != configurations$target_variable], fullRank=T)
      df_temp <- data.frame(predict(dmy, newdata =  df[names(df) != configurations$target_variable]))
      
      df <- cbind(df_temp, df[configurations$target_variable])
      
    } else {
      
      dmy <- dummyVars(" ~ .", data = df[names(df) != configurations$target_variable])
      df_temp <- data.frame(predict(dmy, newdata = df[names(df) != configurations$target_variable]))
      
      df <- cbind(df_temp, df[configurations$target_variable])
      
    }
    
    
    # STANDARDIZATION
    if(isempty(configurations$stand_vars)==FALSE){
      
      stand_vars <- configurations$stand_vars
      standardization <- preProcess(df[as.vector(stand_vars)], method=c("center","scale"))
      df[as.vector(stand_vars)] <- predict(standardization, df[as.vector(stand_vars)])
      
    }
    
    # NORMALIZATION
    if(isempty(configurations$norm_vars)==FALSE) {
      
      norm_vars <- configurations$norm_vars
      normalization <- preProcess(df[as.vector(norm_vars)], method=c("range"))
      df[as.vector(norm_vars)] <- predict(normalization, df[as.vector(norm_vars)])
      
    }
    
    # FEATURE TRANSFORMATION
    if(isempty(configurations$trans_vars)==FALSE){
      
      trans_method <- configurations$trans_method
      trans_vars <- configurations$trans_vars
      transformation <- preProcess(df[as.vector(trans_vars)], method=c(trans_method))
      df[as.vector(trans_vars)] <- predict(transformation,df[as.vector(trans_vars)])
      
    }
    
    # # NEAR-ZERO VARIANCE FILTERING
    if(configurations$nvz == "TRUE") {
      
      xxx <- nearZeroVar(df)
      
      if(identical(xxx, integer(0))==FALSE) {
        
        df <- df[, -xxx]
        
      } else {
        
        df <- df
        
      }
      
    }
    
    # MULTICOLLINEARITY FILTERING
    if(configurations$mcf == "TRUE") {
      
      df_temp <- df[names(df) != configurations$target_variable]
      
      highlyCorVars <- findCorrelation(cor(df_temp), cutoff = configurations$mcf_rate)
      
      if(identical(highlyCorVars, integer(0))==FALSE) {
        
        df <- cbind(df_temp[,-highlyCorVars],df[names(df) == configurations$target_variable])
        
      }
      
    }
    
    # LINEAR COMBINATION FILTERING
    if(configurations$lcf == "TRUE") {
      
      df_temp <- df[names(df) != configurations$target_variable]
      
      comboInfo <- findLinearCombos(matrix(as.numeric(unlist(df_temp)),nrow=nrow(df_temp)))
      
      if(is.null(comboInfo$remove)==FALSE) {
        
        df <- cbind(df_temp[, -comboInfo$remove], df[names(df) == configurations$target_variable])
        
      }
      
    }
    
    print(head(df))
    return(df)
    
  })
  
  prediction <- reactive({
    
    df <- data_preprocessed()
    model <- input_model()[["model_configuration"]]
    
    predictions <- predict(model, df)
    
    return(predictions)
    
  })
  
  output_data <- reactive({
    
      output_data <- cbind(input_data(),y=prediction())
      return(output_data)
      
  })
  
  observe({
    updateSelectInput(session, "variable_x", choices = c(names(output_data()),"NONE"), selected = "NONE")
    updateSelectInput(session, "variable_y", choices = c(names(output_data()),"NONE"), selected = "NONE")
    updateSelectInput(session, "variable_group", choices = c(names(output_data()),"NONE"), selected = "NONE")
    updateSelectInput(session, "selected_class", choices = as.vector(unique(prediction())))
    updateSelectInput(session, "selected_case", choices = as.vector(1:length(prediction())))
  })

  output$shapley <- renderHighchart({
    
    shapley_plot(target_variable=prediction(),train_model=input_model(), test_data=data_preprocessed(), selected_model="model_configuration", selected_case=input$selected_case, selected_class=input$selected_class)

  })
  
  output$output_table <- DT::renderDataTable({
    
    DT::datatable(output_data(),
                  extensions = c('ColReorder'), 
                  options = list(colReorder = TRUE,
                                 scrollX = TRUE,
                                 scrollY = '480px', 
                                 pageLength = 50)
    )
    
  })
  output$shareplot <- renderHighchart({
    
    df <- cbind(input_data(), target_variable = prediction())
    
    
    if(is.numeric(prediction())==FALSE) {
      
      hchart(df$target_variable, type = "column", name = "Predicted Classes") %>%
        hc_plotOptions(column=list(colorByPoint = TRUE)) %>%
        hc_colors(colors[1:length(unique(prediction()))])
      
      
    } else {
      
      hchart(density(prediction()), type = "area", name =  input_model()[["preprocessing_configuration"]]$target_variable)
      
    }

  })
  
  observeEvent(input$update_graph, {
    
    output$descriptive_plot <- renderHighchart({
      
      df <-isolate(output_data())
      descriptive_plot(df, variable_y = isolate(input$variable_y), variable_x = isolate(input$variable_x), variable_group = isolate(input$variable_group))

    })
    
  })
  
  output$Download <- downloadHandler(
    
    filename = "ClassifiedData.csv",
    
    content = function(file){
      
      withProgress(message = "Preparing selected model for export.", 
                   detail = "Please check your 'Downloads' folder.", value=1, {
                     
                     write.csv(output_data(),file)                     
                     
                   })
    }
    
  )
  
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
  
}
