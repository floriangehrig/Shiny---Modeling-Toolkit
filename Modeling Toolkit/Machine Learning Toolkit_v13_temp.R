library(shiny)
library(shinyAce)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinyWidgets)
library(pracma)
library(DBI)
library(highcharter)
library(tidyverse)
library(caret)
library(rapport)
library(foreign)
library(data.table)
library("viridis")
library(rlist)
library(iml)
library(shinycssloaders)
library(future)
library("bsplus")
library(skimr)
library(knitr)
library(kableExtra)
library(shinyhelper)
library(zip)

plan(multisession)

options(spinner.color="#e1e8e3")

colors<-rep(c('#2f7ed8', '#0d233a', '#8bbc21', '#910000', '#1aadce', '#492970', '#f28f43', '#77a1e5', '#c42525', '#a6c96a'),times=100)
model_names <-data.frame(short=c("lm","rf","lda","ada"),
                         full=c("Linear Regression","Random Forest", "Linear Discriminant Analysis","ADA Boosting")
)

# FUNCTIONS -----------------------------------------------------------------

plot_dists <- function(modelings, model, test_df, target_var, naming) {
  
  target_test <- test_df[,target_var]
  target_predicted <- predict(modelings[[model]], test_df[ , !(names(test_df) %in% c(target_var))])
  
  target_residuals <- target_test - target_predicted
  df <- data.frame(predicted = target_predicted,test = target_test) 
  
  df <- df %>% gather("group","value")
  df$group <- as.factor(df$group)
  
  ds <- map(levels(df[,"group"]), function(x){
    dt <- density(df[,"value"][df[,"group"] == x])[1:2]
    dt <- list_parse2(as.data.frame(dt))
    list(data = dt, name = x, type = "area")
  })
  
  plot <- highchart() %>% 
    hc_add_series_list(ds) %>%
    hc_size(height="350px")%>%
    hc_title(text=NULL)
  
  return (plot)
}

plot_residuals <- function(modelings,model,test_df, target_var) {
  
  target_predicted <- predict(modelings[[model]], test_df[ , !(names(test_df) %in% c(target_var))])
  target_test <- test_df[,target_var]
  
  target_residuals <- target_test - target_predicted
  
  df <- data.frame(predicted = target_predicted,
                   test = target_test,
                   residuals = target_residuals) 
  
  hchart(df, "bubble", hcaes(x = test, y = residuals, z=1)) %>%
    hc_colors(colors[1:1]) %>%
    hc_yAxis(plotLines = list(list(value = 0, color = "grey", width = 2, dashStyle = "ShortDot"))) %>%
    hc_plotOptions(
      line = list(enableMouseTracking = FALSE),
      bubble = list(
        marker = list(fillOpacity = 0.3,lineWidth=0),
        minSize = 7, maxSize = 7
      )) %>%
    hc_size(height="350px") %>%
    hc_title(text=NULL)
  
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

correlation_plot <- function(data) {
  
  object <- cor(data[, sapply(data, is.numeric)], use = "complete.obs", method = "pearson")
  
  df <- as.data.frame(object)
  is.num <- sapply(df, is.numeric)
  df[is.num] <- lapply(df[is.num], round, 2)
  dist <- NULL
  
  x <- y <- names(df)
  
  df <- tbl_df(cbind(x = y, df)) %>% 
    gather(y, dist, -x) %>% 
    mutate(x = as.character(x),
           y = as.character(y)) %>% 
    left_join(data_frame(x = y,
                         xid = seq(length(y)) - 1), by = "x") %>% 
    left_join(data_frame(y = y,
                         yid = seq(length(y)) - 1), by = "y")
  
  ds <- df %>% 
    select_("xid", "yid", "dist") %>% 
    list_parse2()
  
  fntltp <- JS("function(){
                  return this.series.xAxis.categories[this.point.x] + ' ~ ' +
                         this.series.yAxis.categories[this.point.y] + ': <b>' +
                         Highcharts.numberFormat(this.point.value, 2)+'</b>';
               ; }")
  cor_colr <- list( list(0, '#FF5733'),
                    list(0.5, '#F8F5F5'),
                    list(1, '#2E86C1')
  )
  highchart() %>% 
    hc_chart(type = "heatmap") %>% 
    hc_xAxis(categories = y, title = NULL) %>% 
    hc_yAxis(categories = y, title = NULL) %>% 
    hc_add_series(data = ds) %>% 
    hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE)
      )) %>% 
    hc_tooltip(formatter = fntltp) %>% 
    hc_legend(align = "right", layout = "vertical",
              margin = 0, verticalAlign = "top",
              y = 25, symbolHeight = 280) %>% 
    hc_colorAxis(stops= cor_colr,min=-1,max=1) %>%
    hc_size(height="350px")
  
}

shapley_plot <- function(target_variable,train_model, test_data, selected_model, selected_case, selected_class=NULL) {
  
  library("iml")
  y <-target_variable
  fitted_model <- train_model[[selected_model]]
  test_X <- test_data %>% select(- !! y)
  test_y <- test_data %>% select(!! y)
  
  predictor <- Predictor$new(fitted_model, data = test_X, y = test_y)
  
  shapley_findings <- Shapley$new(predictor, x.interest = test_X[as.integer(selected_case),])
  
  shapley_values <- shapley_findings$results
  
  if(is.numeric(test_data[,target_variable]) == FALSE) {
    
    shapley_values$class <- as.character(shapley_values$class)
    
  }
  
  shapley_values <- shapley_values %>% mutate(direction = case_when(phi >= 0 ~ '#63ff85', phi<0 ~ '#fc6a60')) %>% arrange(desc(phi))
  
  if(is.numeric(test_data[,target_variable]) == TRUE) {
    
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

importance_plot <- function(train_model, selected_model,data_processed) {

    model<-train_model[[selected_model]]
    data <- data_processed[["data"]]
    var_imp <- varImp(model)

    imp<-var_imp[1] %>% as.data.frame() %>% tibble::rownames_to_column() %>% mutate(Variable = factor(rowname)) %>% select(-rowname)
    imp<- imp %>% gather("Class","Value",-Variable) %>% mutate_if(is.character,as.factor) %>% mutate(Value = round(Value,2)) 
    
    imp <- imp %>%
      dplyr::group_by(Variable) %>% dplyr::summarise(Value = mean(Value)) %>% dplyr::arrange(desc(Value))
    
    plot <- hchart(imp, "bar", hcaes(x=Variable,y=Value, colour=Value)) %>%
      hc_title(text=NULL)  %>%
      hc_size(height = "300px")
    
    return(plot)
  
}

confusion_plot <- function(test_data,target_variable, train_model, selected_model) {
  
  predictions <- predict(train_model[[selected_model]], test_data[ , !(names(test_data) %in% c(target_variable))])

  confusion_matrix<-confusionMatrix(factor(predictions), test_data[ , names(test_data) %in% c(target_variable)])

  kpi_table<-confusion_matrix$table %>% as.data.frame()
  
  hchart(kpi_table, type = "heatmap", hcaes(x = Prediction, y = Reference, value = Freq)) %>%
    hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE)
      )) %>%
  hc_size(height="350px")
}

group_confusion_plot <- function(train_data, test_data, target_variable, train_model, selected_model)  {
  
  predictions <- predict(train_model[[selected_model]], test_data[ , !(names(test_data) %in% c(target_variable))])
  
  data<-confusionMatrix(factor(predictions), test_data[ , names(test_data) %in% c(target_variable)])
  
  df <- train_data
  
  if(length(unique(df[ , (names(df) %in% c(target_variable))])) > 2) {
    
    kpi_group<-data$byClass %>% 
      as.data.frame() %>% 
      tibble::rownames_to_column() %>% 
      separate(rowname, c("Type", "Name"), ": ") %>% 
      mutate(Class = factor(Name)) %>% 
      select(-Type,-Name) 
    
    print(kpi_group)
    
    kpi_group<- kpi_group %>% 
      gather("KPI","Value",-Class) %>% 
      mutate_if(is.character, as.factor) %>% 
      mutate(Value = round(Value,2))
    
    hchart(kpi_group,"line",hcaes(x=Class,y=Value, group="KPI")) %>% 
      hc_yAxis(max = 1) %>% 
      hc_tooltip(shared=T,table=T) %>%
      hc_size(height="350px")
    
  } else {
    
    kpi_group<-data$byClass %>% 
      as.data.frame() %>% 
      tibble::rownames_to_column()
    
    names(kpi_group) <- c("KPI","Value")
    
    hchart(kpi_group,"bar",hcaes(x=KPI,y=Value, color=KPI)) %>% 
      hc_yAxis(max = 1) %>% 
      hc_size(height="350px")

  }
  
}

remove_outliers <- function(x, na.rm = TRUE, ...) {
  
  qnt <- quantile(x, probs=c(.25, .75), mode, na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  if (mode == "Median") {
    
    y[x < (qnt[1] - H)] <- median(x, na.rm = TRUE)
    y[x > (qnt[2] + H)] <- median(x, na.rm = TRUE)
    
  } else if (mode == "Mean") {
    
    y[x < (qnt[1] - H)] <- mean(x, na.rm = TRUE)
    y[x > (qnt[2] + H)] <- mean(x, na.rm = TRUE)

  }

  y
  
}
remove_all_outliers <- function(df, mode_na){
  
  a<-df[,sapply(df, is.numeric), drop = FALSE]
  b<-df[,sapply(df, Negate(is.numeric)), drop = FALSE]
  
  if (mode_na == "Median") {

    a[]<-lapply(a, function(x) remove_outliers(x, mode = "Median"))
    
  } else if (mode_na == "Mean") {
    
    a[]<-lapply(a, function(x) remove_outliers(x, mode = "Median"))
    
  }
  
  d<-cbind(a,b)
  d[, names(df)]
  
}

outlier_imputation <- function(x, mode = "Median") {
  
  if (mode == "Median") {
    
    x[x < quantile(x,0.25) - 1.5 * IQR(x) | x > quantile(x,0.75) + 1.5 * IQR(x)] <- median(x)
    
  } else if (mode == "Mean") {
    
    x[x < quantile(x,0.25) - 1.5 * IQR(x) | x > quantile(x,0.75) + 1.5 * IQR(x)] <- mean(x[x > quantile(x,0.25) - 1.5 * IQR(x) | x < quantile(x,0.75) + 1.5 * IQR(x)])
  }
  
  x
}

seed_number <- 42

# DASHBOARD UI --------------------------------------------------------------------------------------------------

ui <- dashboardPage(

  # DASHBOARD HEADER -----------------------------------------------
  
  dashboardHeader( 
    
    title = "DASHBOARD", # tagList()
    tags$li(class = "dropdown", div(style="padding: 10px 10px;",
                                    actionButton('create_report', 'Create Report', icon = icon("poll"))
    )),
    tags$li(class = "dropdown", div(style="padding: 10px 10px;",
                                    downloadButton('Download', 'Download Model', class="dlButton")
    )),
    tags$li(class = "dropdown", div(style="padding: 10px 10px;",
                                    uiOutput("selected_model", style="width: 100px; height: 20px")
    )),
    tags$li(class = "dropdown", div(style="padding: 10px 10px;",
                                      actionButton("update_vars","UPDATE VARIABLES", icon("sync"), class = "btn-primary"),
                                      actionButton("update_model","UPDATE MODEL", icon("sync"), class = "btn-primary")))
    
  ), # dashboardHeader()
  
  
  # DASHBOARD SIDEBAR -----------------------------------------------
  
  sidebar = dashboardSidebar(
    
    useShinyjs(), # added for Collapsing-Effect
    useSweetAlert(), 
    
    sidebarMenu(
      
      tags$head(
        tags$style(HTML("hr {border-top: 1px solid #000000;}"))
      ),

      # 1. DATA SELECTION
      
      menuItem("DATA SELECTION", tabName = "data_selection", icon = icon("database"), startExpanded = FALSE, 
               div(style = "padding-left: 10px; padding-right: 10px;",
                   # 1.1 Upload Dataset
                   div(style = "padding: 10px 0px; margin-top:-0.5em",
                       fluidRow(
                         selectizeInput(inputId = "input_source",label = "Select Input Option", choices = list(
                           Relational = c("PostgreSQL","MySQL","MS SQL Server","SQLite"),
                           NonRelational = c("Oracle","Teradata","Hive","Impala","AWS Redshift","Salesforce","MonetDB","Big Query","Athena","MongoDB","Netezza","Cassandra"),
                           Other=c("File Upload","Other")
                           ),
                           selected = "File Upload"
                          ),
                         uiOutput("data_input")
                       )),
                   # 1.4 Selection of Target Variable "y"
                   div(style = "padding: 0px 0px; margin-top:-1em", 
                       fluidRow(
                         selectInput(inputId = "target_variable",label = "Select Target Variable:", choices =c(""))
                       )),
                   # 1.2 Selection of Predictor Variables (via REGEX)
                   # div(style = "padding: 0px 0px; margin-top:-2em; margin-bottom: 2em", 
                   #     fluidRow(
                   #       tagsTextInput(inputId = "predictors", "Choose Predictors!", style="width: 150px;") 
                   #     )),
                   # 1.3 Filtering of relevant variables
                   div(style = "padding: 0px 0px; margin-top:-1em", 
                       fluidRow(
                         pickerInput(inputId = "predictor_selection", label = "Filter Predictors:", choices = c(""), options = list(`actions-box` = TRUE, `selected-text-format` = "count > 3"), multiple = TRUE)
                       ))

               )),
      # 2. PRE-PROCESSING
      menuItem("PRE-PROCESSING", tabName = "pre_processing", icon = icon("broom"), startExpanded = FALSE, 
               div(style = "padding-left: 10px; padding-right: 10px;",
                   # 2.1 TREATMENT OF MISSING VALUES
                   div(style = "padding: 0px 0px; margin-top:0em",
                       fluidRow(
                         radioGroupButtons(inputId = "outlier_imputation_method", label = "OUTLIER TREATMENT:",  choices = c("Keep","Median","Mean"), justified = TRUE)
                       )),
                   div(style = "padding: 0px 0px; margin-top:-1.5em",
                       fluidRow(
                         radioGroupButtons(inputId = "na_imputation_method_num", label = "NUMERIC NAS:",  choices = c("Keep","Median","Mean"), justified = TRUE, selected = "Median")
                       )),
                   div(style = "padding: 0px 0px; margin-top:-1.5em",
                       fluidRow(
                         radioGroupButtons(inputId = "na_imputation_method_cat", label = "CATEGORICAL NAS:",  choices = c("Keep","Mode","Random"), justified = TRUE, selected = "Mode")
                       )),
                   div(style = "padding: 0px 0px; margin-top:-1.5em",
                       fluidRow(
                         radioGroupButtons(inputId = "encoding_method", label = "ENCODING APPROACH:",  choices = c("One-Hot","Dummy"), justified = TRUE)
                       )),
                   # 2.X NEAR-ZERO VARIANCE FILTERING
                   div(style = "padding: 0px 0px; margin-top:-1.0em",
                       fluidRow(
                         prettySwitch(inputId = "nvz", label = "Near-Zero Variance Filter", slim=TRUE, value=TRUE)
                       )),
                   # LINEAR COMBINATION FILTERING
                   div(style = "padding: 0px 0px; margin-top:-0.75em",
                       fluidRow(
                         prettySwitch(inputId = "lcf", label = "Linear Combination Filter",  slim=TRUE, value=TRUE)
                       )),
                   # MULTICOLLINEARITY FILTERING
                   div(style = "padding: 0px 0px; margin-top:-0.75em",
                       fluidRow(
                         prettySwitch(inputId = "mcf", label = "Multicollinearity Filter", slim=TRUE)
                       )),
                   div(style = "padding: 0px 40px;margin-top:-1em;",
                       fluidRow(
                         uiOutput("mcf_rate")
                       )),
                   # TRANSFORMATION
                   div(style = "padding: 0px 0px;", 
                       fluidRow(
                         pickerInput(inputId = "trans_method",
                                     label = "FEATURE TRANSFORMATION:",
                                     choices = c("BoxCox","YeoJohnson","expoTrans"),
                                     choicesOpt = list(subtext = c("No 0 or Negatives","Both Positive / Negative","Both Positive / Negative") ),
                                     multiple = F)
                       )),
                   div(style = "padding: 10px 0px; margin-top:-2em", 
                       fluidRow(
                         uiOutput("trans_vars")
                       )),
                   # STANDARDIZATION
                   div(style =  "padding: 10px 0px; margin-top:-2em",
                       fluidRow(
                         uiOutput("stand_vars")
                       )),
                   # NORMALIZATION
                   div(style =  "padding: 10px 0px; margin-top:-2em",
                       fluidRow(
                         uiOutput("norm_vars")
                       ))
               )),
      
      # 3. LEARNING APPROACH
      
      menuItem("LEARNING APPROACH", tabName = "learning_approach", icon = icon("ruler"), startExpanded = FALSE, 
               div(style = "padding-left: 10px; padding-right: 10px;",
                   # 3.1 MODEL TYPE
                   div(style = "padding: 10px 0px; margin-top:-0.5em",
                       fluidRow(
                         pickerInput(inputId = "considered_models",
                                     label = "SELECT MODEL:",
                                     choices = c(""),
                                     multiple = TRUE)
                       )),
                   # 3.2 HYPERPARAMETERS
                   div(style = "padding: 10px 20px; margin-top:-0.5em",
                       fluidRow(
                         uiOutput("hyperparameters")
                       )),
                   # EVALUATION CRITERIA
                   div(style = "padding: 0px 0px; margin-top:-2.5em",
                       fluidRow(
                         selectInput("evaluation_criteria", label = "SELECT EVALUATION METRIC:", choices = c(""))
                       )),
                   # PROCESSING TECHNIQUE
                   div(style = "padding: 0px 0px; margin-top:-1.0em",
                       fluidRow(
                         radioGroupButtons(inputId = "processing_technique", label = "PROCESSING:",  choices = c(c(`<i class='fa fa-code-branch'> Sequential</i>` = "Sequential", `<i class='fa fa-arrows-alt'>  Parallel</i>` = "Parallel")), justified = TRUE)
                       )),
                   # PROCESSING TECHNIQUE
                   div(style = "padding: 0px 0px; margin-top:-1em",
                       fluidRow(
                         radioGroupButtons(inputId = "hp_search_method", label = "SEARCH APPROACH:",  choices = c(c(`<i class='fa fa-braille'>  Grid</i>` = "grid", `<i class='fa fa-arrows-alt'>  Random</i>` = "random")), justified = TRUE)
                       )),
                   # 4.1 SIZE OF TRAINING SET
                   div(style = "padding: 10px 0px; margin-top:-1em",
                       fluidRow(
                         sliderInput(inputId = "training_ratio", "TRAINING SET SIZE:", 0, 1, 0.8)
                       )),
                   # 4.2 RESAMPLING METHOD 
                   div(style = "padding: 0px 0px 0px 0px; margin-top:-2em",
                       fluidRow(
                         pickerInput(inputId = "hp_validation_method",
                                     label = "RESAMPLING METHOD:",
                                     choices = c("boot", "cv", "repeatedcv", "LOOCV", "LGOCV","none"),
                                     choicesOpt = list(subtext = c("Bootstrapping","K-Fold Cross Valdiation","Repeated K-Fold Cross Valdiation", "Leave-One-Out Cross Validation","Leave-Group-Out Cross Validation", "No Validation") ),
                                     multiple = F)
                       )),
                   div(style = "padding: 0px 20px;margin-top:-0.6m;",
                       fluidRow(
                         uiOutput("lgocv_train_perc")
                       )),
                   div(style = "padding: 0px 20px;margin-top:-0.6em;",
                       fluidRow(
                         uiOutput("hp_validation_repeats")
                       )),
                   # 4.3 NUMBER OF ITERATIONS
                   div(style = "padding: 0px 0px; margin-top: -0.2em",
                       fluidRow(
                         sliderInput(inputId = "hp_validation_times", "NUMBER ITERATIONS:", 0, 100, 2)
                       ))
                   # TBD - "Training % for LGOCV"
                   # ...
               ))
      
    ) # sidebarMenu()
  ), # dashboardSidebar()
  
  
  # DASHBOARD BODY -----------------------------------------------
  
  body = dashboardBody(
    
    tags$head(
      tags$style(HTML("hr {border-top: 1px solid #e1e3e3;}"))
    ),
    fluidRow(
      valueBoxOutput(width=3,"KPI1"),
      valueBoxOutput(width=3,"KPI2"),
      valueBoxOutput(width=3,"KPI3"),
      valueBoxOutput(width=3,"KPI4")
    ),
    fluidRow(
      
      tabBox(width=6,
             tabPanel(id="Descriptive",
                      title="Descriptive Feature Analysis",
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
                      )),
             tabPanel(id="Correlations",
                      title = "Feature Correlation",
                      div(
                        div(style="padding: 10px 25px;",
                            withSpinner(highchartOutput("correlation_plot", height = "550px"))
                        )
                      )
             )
      ),
      tabBox(width=6,
             tabPanel(id="Tab1",
                      title="Overall Performance",
                      fluidRow(width=12,
                               column(width=6,withSpinner(highchartOutput("comparison_plot1", height = "600px"))),
                               column(width=6,withSpinner(highchartOutput("comparison_plot2", height = "600px")))
                      )),
             tabPanel(id="Tab2",
                      title="Classification Performance",
                      fluidRow(width=12,
                               column(width=6,withSpinner(highchartOutput("model_plot1", height = "600px"))),
                               column(width=6,withSpinner(highchartOutput("model_plot2", height = "600px")))
                      )),
             tabPanel(id="Tab3",
                      title = "Feature Importance",
                      withSpinner(highchartOutput("imp", height = "600px"))
             ),
             tabPanel(id="Tab4",
                      title="Model Interpretability",
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
                                   column(width=12,withSpinner(highchartOutput("shapley", height = "469px")))
                          ))
             )
             
             
      )
      
    ),
    
    tags$script(HTML("$('body').addClass('sidebar-mini');"))
    
  ) # dashboardBody()
  
  
) # dashboardPage()











# DASHBOARD SERVER --------------------------------------------------------------------------------------------------

server <- function(input, output, server, session) {
  
  #options(shiny.maxRequestSize=1000*1024^2)
  
  # KPI BOXES
  output$KPI1 <- renderValueBox({valueBox("-", "KPI 1", color = "green")})
  output$KPI2 <- renderValueBox({valueBox("-", "KPI 2", color = "yellow")})
  output$KPI3 <- renderValueBox({valueBox("-", "KPI 3", color = "blue")})
  output$KPI4 <- renderValueBox({valueBox("-", "KPI 4", color = "red")})
  
  
  # DYNAMIC INPUTS --------------------------------------------------------------
  
  # DATA CONNECTION
  output$data_input <- renderUI({
    
    if (input$input_source == "File Upload") {
      
      fileInput(inputId = "upload", "Upload your Dataset!")
      
    } else {
      
      actionButton("show", "Connect to Database...", icon=icon("database"), width='86%')

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
                   textInput("database",label = "DATABASE NAME:",value = "", placeholder = "Data Repository"),
                   textInput("host",label = "HOSTNAME / URL:",value = "", placeholder = "localhost"),
                   textInput("user",label = "USER NAME:",value = "", placeholder = "FG"),
                   passwordInput("password",label = "PASSWORD:",value = ""),
                   actionButton(inputId = "connect_db", label="Connect to Database", icon =icon("exchange-alt"))
                   )),
                 column(width=8,div(
                   strong("WRITE YOUR SQL QUERY:"),
                   hr(),
                   shinyAce::aceEditor("sql_query", mode = "sql")))
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
  
  # REPORT DATA ----
  report_data <- reactiveValues(

    # USER-DEFINED INPUT
    project_name= NULL,
    project_background= NULL,
    decision_comment= NULL,

    # DATA ANALYSIS
    n_rows= NULL,
    n_cols= NULL,
    n_numeric= NULL,
    n_categorical= NULL,
    data_structure_numeric= NULL,
    data_structure_integer= NULL,
    data_structure_factor= NULL,
    eda_plots = list(),
    correlation_plot= NULL,

    # DATA PRE-PROCESSING
    mcf= NULL,
    mcf_rate= NULL,
    nvz= NULL,
    lcf= NULL,
    stand_vars= NULL,
    norm_vars= NULL,
    trans_vars= NULL,
    trans_method= NULL,
    categorical_vars= NULL,
    encoding_method= NULL,
    na_imputation_method_num= NULL,
    na_imputation_method_cat= NULL,
    outlier_sd_range= NULL,
    outlier_imputation_method= NULL,
    data_structure_numeric_clean= NULL,
    data_structure_integer_clean= NULL,
    data_structure_factor_clean= NULL,

    # DATA MODELING
    target_variable= NULL,
    variable_names= NULL,
    considered_models = NULL,
    algorithm_names = NULL,
    holdout_ratio= NULL,
    hp_search_method= NULL,
    hp_validation_method= NULL,
    hp_validation_times= NULL,
    # var_validation_method= NULL,
    # var_validation_times= NULL ,
    # rfe_value= NULL,
    
    # MODEL EVALUATION
    evaluation_criteria= NULL,
    model_highest= NULL,
    value_highest= NULL,
    model_final = NULL,
    final_kpi1_name = NULL,
    final_kpi1_value = NULL,
    final_kpi2_name = NULL,
    final_kpi2_value = NULL,
    comparison_plot1= NULL,
    comparison_plot2= NULL,
    model_plots1 = list(),
    model_plots2 = list(),
    importance_plots= NULL,
    shapley_plots1= list(),
    shapley_plots2= list(),
    shapley_case1= NULL,
    shapley_case2= NULL

  )
  
  # REPORTING
  reportModal <- function() {
    modalDialog(
      fluidPage(
        textInput(inputId = "project_name",label = "PROJECT NAME:",value = "ILLUSTRATIVE PROJECT - ", placeholder = "(REQUIRED)",width="200%"),
        textAreaInput(inputId = "project_background",label = "ADD DETAILS ON THE PROJECT BACKGROUND:",width="180%",height="300%",value = "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum", placeholder = "(OPTIONAL)"),
        selectInput(inputId = "model_final",label="CHOOSE YOUR PRIORITIZED MODEL:",choices = input$considered_models),
        textAreaInput(inputId = "decision_comment",label = "ADD A COMMENT ABOUT YOUR FINAL DECISION:",width="180%",height="300%",value = "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum", placeholder = "(OPTIONAL)")
      ),
      footer = tagList(modalButton("Cancel"),
                       actionButton("prepare_export", "Prepare", icon = icon("file-export")),
                       downloadButton("download_report", "Download Report", icon = icon("download"))
      )
    )
  }
  observeEvent(input$create_report, {
    showModal(reportModal())
  })
  
  output$download_report <- downloadHandler(
    
    filename = "report.html",
    content = function(file) {
      
      tempReport <- file.path(tempdir(), "ReportTemplate.Rmd")
      file.copy("ReportTemplate.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(

                     # USER-DEFINED INPUT
                     project_name= input$project_name, # CHECK
                     project_background= input$project_background, # CHECK
                     decision_comment= input$decision_comment, # CHECK

                     # DATA ANALYSIS
                     n_rows= report_data$n_rows, # CHECK
                     n_cols= report_data$n_cols, # CHECK
                     n_numeric= report_data$n_numeric, # CHECK
                     n_categorical= report_data$n_categorical, # CHECK
                     data_structure_numeric= report_data$data_structure_numeric, # CHECK
                     data_structure_integer= report_data$data_structure_integer, # CHECK
                     data_structure_factor= report_data$data_structure_factor, # CHECK
                     eda_plots = report_data$eda_plots, # CHECK
                     correlation_plot = report_data$correlation_plot, # CHECK

                     # DATA PRE-PROCESSING
                     mcf= report_data$mcf, # CHECK
                     mcf_rate= report_data$mcf_rate, # CHECK
                     nvz= report_data$nvz, # CHECK
                     lcf= report_data$lcf, # CHECK
                     stand_vars= report_data$stand_vars, # CHECL
                     norm_vars= report_data$norm_vars, # CHECK
                     trans_vars= report_data$trans_vars, # CHECK
                     trans_method= report_data$trans_method, # CHECK
                     categorical_vars= report_data$categorical_vars, # TO DO!!!
                     encoding_method= report_data$encoding_method, #TO DO!!!
                     na_imputation_method_num= report_data$na_imputation_method_num,
                     na_imputation_method_cat= report_data$na_imputation_method_cat,
                     outlier_sd_range= report_data$outlier_sd_range,
                     outlier_imputation_method= report_data$outlier_imputation_method,
                     data_structure_numeric_clean = report_data$data_structure_numeric_clean, # CHECK
                     data_structure_integer_clean= report_data$data_structure_integer_clean, # CHECK
                     data_structure_factor_clean= report_data$data_structure_factor_clean, # CHECK

                     # DATA MODELING
                     target_variable= report_data$target_variable, # CHECK
                     variable_names= report_data$variable_names,# CHECK
                     considered_models = report_data$considered_models, # CHECK
                     algorithm_names = report_data$algorithm_names, # CHECK
                     holdout_ratio= report_data$holdout_ratio, # CHECK
                     hp_search_method= report_data$hp_search_method, # CHECK - TO DO
                     hp_validation_method= report_data$hp_validation_method, # CHECK
                     hp_validation_times= report_data$hp_validation_times, # CHECK
      #                var_validation_method= report_data$var_validation_method,
      #                var_validation_times= report_data$var_validation_times,
      #                rfe_value= report_data$rfe_value,
                      
                    # MODEL EVALUATION
                     evaluation_criteria= report_data$evaluation_criteria, # CHECK
                     model_highest= report_data$model_highest, # CHECK
                     value_highest= report_data$value_highest, # CHECK
                     model_final= report_data$model_final, # CHECK
                     final_kpi1_name = report_data$final_kpi1_name, # CHECK
                     final_kpi1_value = report_data$final_kpi1_value, # CHECK
                     final_kpi2_name = report_data$final_kpi2_name, # CHECK
                     final_kpi2_value = report_data$final_kpi2_value, # CHECK
                     comparison_plot1= report_data$comparison_plot1, # CHECK
                     comparison_plot2= report_data$comparison_plot2, # CHECK
                     model_plots1 = report_data$model_plots1, # CHECK
                     model_plots2 = report_data$model_plots2, # CHECK
                     importance_plots= report_data$importance_plots, # CHECK - UPDATE
                     shapley_plots1= report_data$shapley_plots1, # CHECK - UPDATE
                     shapley_plots2= report_data$shapley_plots2, # CHECK - UPDATE
                     shapley_case1= report_data$shapley_case1, # CHECK
                     shapley_case2= report_data$shapley_case2  # CHECK
      #                
                     )
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  
  # UPLOADING OF DATA SET -----
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
    
      report_data$n_rows <- nrow(read)
      report_data$n_cols <- ncol(read)
      report_data$n_numeric <- sum(sapply(read,is.numeric))
      report_data$n_categorical <- (ncol(read) - sum(sapply(read,is.numeric)))
      report_data$data_structure_numeric <- skimr::skim_to_list(read)$numeric
      report_data$data_structure_integer <- skimr::skim_to_list(read)$integer
      report_data$data_structure_factor <- skimr::skim_to_list(read)$factor
      
      return(read)
      
    })
    
  # UPDATE OF TARGET VARIABLE
  observe({
    updateSelectInput(session, "target_variable", choices = names(input_data()))
  })
  
  observe({
    if(is.numeric(input_data()[[input$target_variable]]) == TRUE) {
      
      updatePickerInput(session, "considered_models", 
                        choices = c("lm","rf"),
                        choicesOpt = list(subtext = c("Linear Regression",
                                                      "Random Forest")),
                        selected = "lm")
      
      updateSelectInput(session, inputId = "evaluation_criteria", choices = c("RMSE", "Rsquared"))
      
      
    } else {
      
      updatePickerInput(session, "considered_models", 
                        choices = c("lda","ada","rf"),
                        choicesOpt = list(subtext = c("Linear Discriminant Analysis",
                                                      "AdaBoost Classification Trees",
                                                      "Random Forest")),
                        selected = "lda")
      updateSelectInput(session, inputId = "evaluation_criteria", choices = c("Accuracy", "Kappa"))
    }
    
  })
  
  # ALL POTENTIAL VARIABLES   
  predictor_vars <- reactive({
      predictor_vars <- names(input_data()[,! names(input_data()) %in% input$target_variable ,drop=FALSE])
      return(predictor_vars)
  })
  
  observe({
    updatePickerInput(session, "predictor_selection", choices = as.vector(predictor_vars()), selected = as.vector(predictor_vars()))
  })
  observe({
    updateSelectInput(session, "variable_x", choices = c(names(input_subset()),"NONE"), selected = "NONE")
    updateSelectInput(session, "variable_y", choices = c(names(input_subset()),"NONE"), selected = "NONE")
    updateSelectInput(session, "variable_group", choices = c(names(input_subset()),"NONE"), selected = "NONE")
    updateSelectInput(session, "selected_class", choices = as.vector(unique(input_data()[[input$target_variable]])))
  })

  # PRE-PROCESSING VARIABLES
  output$trans_vars <- renderUI({
    
    pickerInput(inputId = "trans_vars",
                choices = numerical_variables(),
                multiple = T)
    
  })
  output$stand_vars <- renderUI({
    
    pickerInput(inputId = "stand_vars", label = "STANDARDIZATION:",  choices = numerical_variables(),
                options = list(`actions-box` = TRUE, size = 10,`selected-text-format` = "count > 3"), 
                multiple = TRUE
    )
    
  })
  output$norm_vars <- renderUI({
    
    pickerInput(inputId = "norm_vars", label = "NORMALIZATION: (0-1)",  choices = numerical_variables(), 
                options = list(`actions-box` = TRUE, size = 10,`selected-text-format` = "count > 3"), 
                multiple = TRUE
    )
    
  })
  output$mcf_rate <- renderUI({
    
    if (input$mcf == TRUE) {
      
      sliderInput(inputId = "mcf_rate", label = NULL, min=0.1,max = 1.0, value=0.75, step = 0.05) 
      
    }
    
  })

  output$hyperparameters <- renderUI({
    
    if("lda" %in% input$considered_models) {
      
    } else if ("adaboost" %in% input$considered_models) {
      
      div(style = "padding: 10px 0px; margin-top:-0.5em",
          fluidRow(
            sliderInput(inputId = "nIter", label = "Number of Trees:", min=0.1,max = 1.0, value=0.75, step = 0.05),
            sliderInput(inputId = "method", label = "Method:", min=0.1,max = 1.0, value=0.75, step = 0.05) 
          ))
      
    } else if ("rf" %in% input$considered_models) {

      div(style = "padding: 10px 0px; margin-top:-0.5em",
          fluidRow(
            sliderInput(inputId = "mtry", label = "#Randomly Selected Predictors:", min=0.1,max = 1.0, value=0.75, step = 0.05)
          ))
    }
  })
  output$hp_validation_repeats <- renderUI({
    
    if (input$hp_validation_method == "repeatedcv") {
      
      sliderInput(inputId = "hp_validation_repeats", label = "NUMBER REPEATS:", min=2,max = 20, value=5, step = 1) 
      
    }
    
  })
  output$lgocv_train_perc <- renderUI({
    
    if (input$hp_validation_method == "LGOCV") {
      
      sliderInput(inputId = "lgocv_train_perc", label = "TRAINING %:", min=0.1,max = 1.0, value=0.5, step = 0.1) 
      
    }
    
  })
  output$selected_model <- renderUI({
    
    if(isempty(input$considered_models) == T) {
      
      selectInput(inputId = "selected_model",label=NULL,choices = c())
      
    } else {
      
      selectInput(inputId = "selected_model",label=NULL,choices = input$considered_models)
      
    }
    
  })
  
  # FILTERING OF RELEVANT VARIABLES
  input_subset <- eventReactive(input$update_vars, {
    
    data<-input_data()
    y<-isolate(input$target_variable)
    vars <-isolate(input$predictor_selection)
    
    input_subset<-data[,which(names(data) %in% vars)] %>% as.data.frame()
    y<-data[,names(data) %in% y]
    
    if (is.character(y)==TRUE){
      
      y <- as.factor(y)
      
    }
    
    input_subset[,input$target_variable]<-y
    return(input_subset)
    
  })
  
  # EXTRACTION OF ALL NUMERICAL COLUMNS IN FILTERED DATAFRAME
  numerical_variables <- reactive({
    
    numerical_vars <- c()
    
    if(isempty(input_subset())) {
      
      append(numerical_vars, "")
      
    } else {
      
      df <- input_subset()
      
      for(k in names(df)) {
        
        print(k)
        
        if(is.numeric(df[[k]]) == TRUE) {
          
          print(paste(k," is numeric"))
          
          numerical_vars <- c(numerical_vars, k)
          
        }
        
      }
      
    }
    
    return(numerical_vars)  
    
  })
  categorical_vars <- reactive({
    
    categorical_vars <- c()
    
    if(isempty(input_subset())) {
      
      append(categorical_vars, "")
      
    } else {
      
      df <- input_subset()
      
      for(k in names(df)) {
        
        print(k)
        
        if(is.numeric(df[[k]]) == FALSE) {
          
          print(paste(k," is categorical"))
          
          categorical_vars <- c(categorical_vars, k)
          
        }
        
      }
      
    }
    report_data$categorical_vars <- categorical_vars
    
    return(categorical_vars)
    
  })
  
  # ALL MODELING ACTIVITIES (UPDATED PER ACTION BUTTON)
  observeEvent(input$update_model, {

    # PRE-PROCESSING OF FILTERED DATAFRAME 
    data_processed <- reactive({
      
      preprocess_config <- list()
      
      df <- input_subset()
      preprocess_config <- list.append(preprocess_config, selected_vars = names(df))
      preprocess_config <- list.append(preprocess_config, target_variable = input$target_variable)
      
      if(input$input_source != "File Upload") {
        
        preprocess_config <- list.append(preprocess_config, database = input$database)
        preprocess_config <- list.append(preprocess_config, host = input$host)
        preprocess_config <- list.append(preprocess_config, user = input$user)
        preprocess_config <- list.append(preprocess_config, sql_query = input$sql_query)

      }

      df <- as.data.table(df)

      # NA IMPUTATION
      report_data$na_imputation_method_num <- input$na_imputation_method_num
      report_data$na_imputation_method_cat <- input$na_imputation_method_cat

      for(k in names(df)) {
          
          if(is.numeric(df[[k]]) == TRUE) {
            
            if (input$na_imputation_method_num == "Median") {
              
              med <- median(df[[k]],na.rm = T)
              set(x = df, which(is.na(df[[k]])), k, med)
              
              preprocess_config <- list.append(preprocess_config, na_imputation_method_num = "Median")

            } else if (input$na_imputation_method_num == "Mean") {
              
              mean <- mean(df[[k]],na.rm = T)
              set(x = df, which(is.na(df[[k]])), k, mean)    
              
              preprocess_config <- list.append(preprocess_config, na_imputation_method_num = "Mean")

            } else {
              
              preprocess_config <- list.append(preprocess_config, na_imputation_method_num = "Mean")
              
            }
            
          } else if(is.numeric(df[[k]]) == FALSE) {
            
            if (input$na_imputation_method_cat == "Mode") {
              
              mode <- names(which.max(table(df[[k]])))
              set(x = df, which(is.na(df[[k]])), k, mode)
              
              preprocess_config <- list.append(preprocess_config, na_imputation_method_cat = "Mode")

            } else if (input$na_imputation_method_cat == "Random") {
              
              random <- sample(unique(df[[k]]), 1)
              set(x = df, which(is.na(df[[k]])), k, random)  
              
              preprocess_config <- list.append(preprocess_config, na_imputation_method_cat = "Random")

            } else {
              
              preprocess_config <- list.append(preprocess_config, na_imputation_method_cat = "Keep")
              
            }
            
          }
        
      }
      
      # OUTLIER TREATMENT
      report_data$outlier_imputation_method <- input$outlier_imputation_method
      
      for(k in names(df)) {

        if(is.numeric(df[[k]]) == TRUE) {

          if (input$outlier_imputation_method== "Median") {
            
            df[] <- lapply(df, outlier_imputation, mode="Median")

            preprocess_config <- list.append(preprocess_config, outlier_imputation_method = "Median")

          } else if (input$outlier_imputation_method == "Mean") {

            df[] <- lapply(df, outlier_imputation, mode="Mean")

            preprocess_config <- list.append(preprocess_config, outlier_imputation_method = "Mean")

          } else {
            
            preprocess_config <- list.append(preprocess_config, outlier_imputation_method = "Keep")
            
          }
        }
      }
      
      set.seed(42)
      df <- as.data.frame(df)
      
      # CATEGORICAL VARIABLE ENCODING
      report_data$encoding_method <- input$encoding_method
      preprocess_config <- list.append(preprocess_config, encoding_method = input$encoding_method)

      if(input$encoding_method == "Dummy") {

        dmy <- dummyVars(" ~ .", data = df[names(df) != input$target_variable], fullRank=T)
        df_temp <- data.frame(predict(dmy, newdata =  df[names(df) != input$target_variable]))

        df <- cbind(df_temp, df[input$target_variable])

      } else {

        dmy <- dummyVars(" ~ .", data = df[names(df) != input$target_variable])
        df_temp <- data.frame(predict(dmy, newdata = df[names(df) != input$target_variable]))

        df <- cbind(df_temp, df[input$target_variable])

      }
      
      # STANDARDIZATION
      if(isempty(input$stand_vars)==FALSE){
        
        stand_vars <- input$stand_vars
        
        preprocess_config <- list.append(preprocess_config, stand_vars = input$stand_vars)
        report_data$stand_vars <- input$stand_vars
        
        standardization <- preProcess(df[as.vector(stand_vars)], method=c("center","scale"))
        df[as.vector(stand_vars)] <- predict(standardization, df[as.vector(stand_vars)])
        
      }
      
      # NORMALIZATION
      if(isempty(input$norm_vars)==FALSE) {
        
        norm_vars <- input$norm_vars
        
        preprocess_config <- list.append(preprocess_config, norm_vars = input$norm_vars)
        report_data$norm_vars <- input$norm_vars
        
        normalization <- preProcess(df[as.vector(norm_vars)], method=c("range"))
        df[as.vector(norm_vars)] <- predict(normalization, df[as.vector(norm_vars)])
        
      }
      
      # FEATURE TRANSFORMATION
      if(isempty(input$trans_vars)==FALSE){
        
        preprocess_config <- list.append(preprocess_config, trans_method = input$trans_method)
        preprocess_config <- list.append(preprocess_config, trans_vars = input$trans_vars)
        
        report_data$trans_method <- input$trans_method
        report_data$trans_vars <- input$trans_vars
        
        trans_method <- input$trans_method
        trans_vars <- input$trans_vars
        transformation <- preProcess(df[as.vector(trans_vars)], method=c(trans_method))
        df[as.vector(trans_vars)] <- predict(transformation,df[as.vector(trans_vars)])
        
      }
      
      # NEAR-ZERO VARIANCE FILTERING
      if(input$nvz == TRUE) {
        
        preprocess_config <- list.append(preprocess_config, nvz = "TRUE")
        report_data$nvz <-input$nvz
        
        xxx <- nearZeroVar(df)
        
        if(identical(xxx, integer(0))==FALSE) {
          
          df <- df[, -xxx]
          
        } else {
          
          df <- df
          
        }

      } else {
        
        preprocess_config <- list.append(preprocess_config, nvz = "FALSE")
        
      }
      
      # MULTICOLLINEARITY FILTERING
      if(input$mcf == TRUE) {
        
        df_temp <- df[names(df) != input$target_variable]
          
        highlyCorVars <- findCorrelation(cor(df_temp), cutoff = input$mcf_rate)
        
        preprocess_config <- list.append(preprocess_config, mcf = "TRUE")
        preprocess_config <- list.append(preprocess_config, mcf_rate = input$mcf_rate)
        
        report_data$mcf <- input$mcf
        report_data$mcf_rate <- input$mcf_rate
        
        if(identical(highlyCorVars, integer(0))==FALSE) {
          
          df <- cbind(df_temp[,-highlyCorVars], df[names(df) == input$target_variable])
        }
        
      } else {
        
        preprocess_config <- list.append(preprocess_config, mcf = "FALSE")
        preprocess_config <- list.append(preprocess_config, mcf_rate = NULL)
        
      }
      
      # LINEAR COMBINATION FILTERING
      if(input$lcf == TRUE) {
        
        df_temp <- df[names(df) != input$target_variable]
        
        preprocess_config <- list.append(preprocess_config, lcf = "TRUE")
        report_data$lcf <- input$lcf

        comboInfo <- findLinearCombos(matrix(as.numeric(unlist(df_temp)),nrow=nrow(df_temp)))
        
        if(is.null(comboInfo$remove)==FALSE) {
          
          df <- cbind(df_temp[, -comboInfo$remove], df[names(df) == input$target_variable])
          
        }

      } else {
        
        preprocess_config <- list.append(preprocess_config, lcf = "FALSE")
        
      }
      
      print("Preprocessing Successful!")
      
      report_data$data_structure_numeric_clean <- skimr::skim_to_list(df)$numeric
      report_data$data_structure_integer_clean <- skimr::skim_to_list(df)$integer
      report_data$data_structure_factor_clean  <- skimr::skim_to_list(df)$factor
      
      return(list(data=df,configuration=preprocess_config))
      
    })
    
    # MODELING CRITERIA
    train_controls <-reactive({
      
      
      
      if (input$hp_validation_method == "repeatedcv") {
        
        train_controls<-trainControl(method=input$hp_validation_method,
                                     number = input$hp_validation_times,
                                     repeats = input$hp_validation_repeats,
                                     verboseIter = TRUE)
        
      } else if (input$hp_validation_method == "LGOCV") {
        
        train_controls<-trainControl(method=input$hp_validation_method,
                                     number = input$hp_validation_times,
                                     p = input$lgocv_train_perc,
                                     verboseIter = TRUE)
        
      } else {
        
        train_controls<-trainControl(method=input$hp_validation_method,
                                     number = input$hp_validation_times,
                                     verboseIter = TRUE)
        
      }
      
      print("TrainControl Passed")
      report_data$holdout_ratio <- (1-input$training_ratio)*100
      report_data$hp_validation_method <- input$hp_validation_method
      report_data$hp_validation_times <- input$hp_validation_times
      report_data$hp_search_method <- input$hp_search_method
      
      return(train_controls)
      
    })
    
    # DATA SPLIT-UP
    index <- reactive({
      
      data <- data_processed()[["data"]]
      
      index<-createDataPartition(data[,input$target_variable],
                                 p = input$training_ratio,
                                 list = FALSE,
                                 times = 1)
      
    })
    
    # TRAIN DATA PARTITION
    train_data <- reactive({
      
      data <- data_processed()[["data"]]
      
      train_data <- data[isolate(index()),]
      
      return(train_data)
      
    })
    
    # TEST DATA PARTITION
    test_data <- reactive({
      
      data <- data_processed()[["data"]]
      
      test_data <- data[-isolate(index()),]
      
      return(test_data)    
      
    })
    
    # MODEL TRAINING
    train_model <- reactive({
      
      withProgress(message = "Model Options are being evaluated", 
                   detail = 'This may take a while...', value = 0, {

      data <- train_data()
      
      models<-input$considered_models
      
      report_data$considered_models <- input$considered_models
      report_data$target_variable <- input$target_variable
      
      set.seed(seed_number)
      model_list <-list()
      model_perf <- list()
      algorithm_names <- list()
      
      for (i in 1:length(models)) {
        
        incProgress(1/length(models))
        
        model_list[[i]] <- train(as.formula(paste(input$target_variable,'~ .')), 
                                 data = data, 
                                 method = models[i],
                                 metric = input$evaluation_criteria,
                                 importance = TRUE, 
                                 verbose=TRUE,
                                 trControl = train_controls()
        )
        
        algorithm_names[[models[i]]] <- as.character(model_names[model_names[["short"]] == models[i],][["full"]])
        model_perf[[models[i]]] <- max(model_list[[i]]$results[input$evaluation_criteria])
        
      }
      
      report_data$algorithm_names <- algorithm_names
      report_data$evaluation_criteria <- input$evaluation_criteria
      report_data$value_highest <- max(unlist(model_perf))
      report_data$model_highest <- as.character(model_names[model_names[["short"]] == names(model_perf[model_perf==max(unlist(model_perf))]),][["full"]])
      
      names(model_list) <- models
      train_model <- model_list
      
      })
      
      return(train_model)
      
    })
    
    # PREDICTION ON TEST PARTITION
    predictions <- reactive({
      
      data <- test_data()
      fitted_models <- train_model()
      
      predictions <- predict(fitted_models[[input$selected_model]], data[ , !(names(data) %in% c(input$target_variable))])
      
      return(predictions)
      
    })
    
    # MODEL OUTPUT / KPIS ------------------------------------------------
    
    model_results <- reactive({
      
      model_results <- resamples(train_model()) 
      
      print("MODEL RESULTS:")
      print(model_results)
      
      model_results <- model_results$values %>% 
        gather("key","value",-Resample) %>%
        separate(key,c("model","metric"),"~")
      
      return(model_results)
      
    })
    
    observeEvent(input$prepare_export, {
      
        report_data$model_final <-  as.character(model_names[model_names[["short"]]==input$model_final,][["full"]])
        report_data$final_kpi1_name <- gsub("Train","",colnames(getTrainPerf(train_model()[[input$model_final]])[1]))
        report_data$final_kpi1_value <- getTrainPerf(train_model()[[input$model_final]])[[1]]
        report_data$final_kpi2_name <- gsub("Train","",colnames(getTrainPerf(train_model()[[input$model_final]])[2]))
        report_data$final_kpi2_value <- getTrainPerf(train_model()[[input$model_final]])[[2]]

    })
    
    confusion_matrix <- reactive({
      
      data<-test_data()
      
      if(is.numeric(data[,input$target_variable])){
        
        return(NULL)
        
      } else {
        
        confusion_matrix<-confusionMatrix(factor(predictions()), test_data()[ , (names(test_data()) %in% c(input$target_variable))])
        
      }
      
      return(confusion_matrix)
      
    })

    output$comparison_plot1 <- renderHighchart({
      
      if(is.numeric(test_data()[,input$target_variable]) == TRUE) {

        df <- model_results() %>%
          filter(metric == "Rsquared")
        
        plot <- hcboxplot(x=df$value,var = df$model, name = "RSquared") %>%
          hc_colors(colors[1:length(unique(df$model))]) %>%
          #hc_xAxis(categories = as.character(model_names[["full"]])) %>%
          hc_plotOptions(errorbar=list(colorByPoint = TRUE, lineWidth = 3)) %>%
          hc_title(text = "Model Comparison - R Squared") %>% 
          hc_chart(type = "column")%>%
          hc_size(height="350px") 
        
      } else {
        
        df <- model_results() %>%
          filter(metric == "Accuracy")
        
        plot <- hcboxplot(x=df$value,var = df$model, name = "Accuracy") %>%
          hc_colors(colors[1:length(unique(df$model))]) %>%
          #hc_xAxis(categories = as.character(model_names[["full"]])) %>%
          hc_plotOptions(errorbar=list(colorByPoint = TRUE, lineWidth = 3)) %>%
          hc_title(text = "Model Comparison - Accuracy") %>% 
          hc_chart(type = "column") %>%
          hc_size(height="350px") 
          
        
      }
      
      report_data$comparison_plot1 <- plot
      
      plot
      
    })
    output$comparison_plot2 <- renderHighchart({
      
      if(is.numeric(test_data()[,input$target_variable]) == TRUE) {

        df <- model_results() %>%
          filter(metric == "RMSE")
        
        plot <- hcboxplot(x=df$value,var = df$model, name = "RMSE") %>%
          hc_colors(colors[1:length(unique(df$model))]) %>%
          #hc_xAxis(categories = as.character(model_names[model_names[["short"]] %in% input$considered_models,][["full"]])) %>%
          hc_plotOptions(errorbar=list(colorByPoint = TRUE, lineWidth = 3)) %>%
          hc_title(text = "Model Comparison - RMSE") %>% 
          hc_chart(type = "column") %>%
          hc_size(height="350px")
          
        
      } else {
        
        df <- model_results() %>%
          filter(metric == "Kappa")
        
        plot <- hcboxplot(x=df$value,var = df$model, name = "Kappa") %>%
          hc_colors(colors[1:length(unique(df$model))]) %>%
         #hc_xAxis(categories = as.character(model_names[model_names[["short"]] %in% input$considered_models,][["full"]])) %>%
          hc_plotOptions(errorbar=list(colorByPoint = TRUE, lineWidth = 3)) %>%
          hc_title(text = "Model Comparison - Kappa") %>% 
          hc_chart(type = "column") %>%
          hc_size(height="350px") 
          
        
      }
      
      report_data$comparison_plot2 <- plot
      
      plot
      
    })

    output$KPI1 <- renderValueBox({
      
      if(is.numeric(test_data()[,input$target_variable]) == TRUE) {
        
        fitted_model <- train_model()[[input$selected_model]]
        data <-  max(fitted_model$results[["Rsquared"]])
        value <- round(data,2)
        
        valueBox(as.character(value), "R-Squared", icon = icon("thumbs-up", lib = "glyphicon"), color = "green")
        
      } else {
        
        data<-confusion_matrix()
        data<-data[3] %>% as.data.frame()
        
        value <- round(data[1,1]*100,2)
        
        valueBox(paste(value,"%"), "Accuracy", icon = icon("thumbs-up", lib = "glyphicon"), color = "green")
        
      }

    })
    output$KPI2 <- renderValueBox({
      
      if(is.numeric(test_data()[,input$target_variable]) == TRUE) {
        
        fitted_model <- train_model()[[input$selected_model]]
        data <-  max(fitted_model$results[["RMSE"]])
        value <- round(data,2)
        
        valueBox(as.character(value), "RMSE", icon = icon("thumbs-up", lib = "glyphicon"), color = "yellow")
        
      } else {
        
        data<-confusion_matrix()
        data<-data[3] %>% as.data.frame()
        
        value <- round(data[5,1]*100,2)
        
        valueBox(paste(value,"%"), "No Information Rate", icon = icon("thumbs-up", lib = "glyphicon"), color = "yellow")
        
      }
      
    })
    output$KPI3 <- renderValueBox({
      
      if(is.numeric(test_data()[,input$target_variable]) == TRUE) {
        
        fitted_model <- train_model()[[input$selected_model]]
        data <-  max(fitted_model$results[["MAE"]])
        value <- round(data,2)
        
        valueBox(as.character(value), "MAE", icon = icon("thumbs-up", lib = "glyphicon"), color = "blue")
        
      } else {
        
        data<-confusion_matrix()
        data<-data[3] %>% as.data.frame()
        
        value <- round(data[6,1]*100,2)
        
        valueBox(paste(value,"%"), "P-Value", icon = icon("thumbs-up", lib = "glyphicon"),color = "blue")
        
      }
      
    })
    output$KPI4 <- renderValueBox({
      
      data<-confusion_matrix()
      data<-data[3] %>% as.data.frame()
      value <- round(data[2,1]*100,2)
      
      valueBox(paste(value,"%"), "Kappa", icon = icon("thumbs-up", lib = "glyphicon"),color = "red" )

    })
    
    output$imp <- renderHighchart({
      
      report_data$importance_plots <- sapply(input$considered_models, function(i) importance_plot(train_model=isolate(train_model()), selected_model=i,data_processed=isolate(data_processed())), simplify = FALSE)
      
      importance_plot(train_model=isolate(train_model()), selected_model=isolate(input$selected_model),data_processed=isolate(data_processed()))

    })

    output$model_plot1 <- renderHighchart({
      
      if(is.numeric(test_data()[,input$target_variable]) == TRUE) {

        report_data$model_plots1 <- sapply(input$considered_models, function(i) plot_dists(modelings=isolate(train_model()), model=i, test_df=isolate(test_data()), target_var=isolate(input$target_variable),naming="XX"), simplify = FALSE)
        
        plot_dists(modelings=isolate(train_model()), model=isolate(input$selected_model), test_df=isolate(test_data()), target_var=isolate(input$target_variable),naming="XX")

      } else {
        
        report_data$model_plots1 <- sapply(input$considered_models, function(i) confusion_plot(test_data=isolate(test_data()),target_variable=isolate(input$target_variable), train_model=isolate(train_model()), selected_model=i), simplify = FALSE)
        
        confusion_plot(test_data=isolate(test_data()),target_variable=isolate(input$target_variable), train_model=isolate(train_model()), selected_model=input$selected_model)
        
        
      }
      

    })
    output$model_plot2 <- renderHighchart({
      
      if(is.numeric(test_data()[,input$target_variable]) == TRUE) {
        
        report_data$model_plots2 <- sapply(input$considered_models, function(i) plot_residuals(modelings=isolate(train_model()), model=i, test_df=isolate(test_data()), target_var=isolate(input$target_variable)), simplify = FALSE)
        plot_residuals(modelings=isolate(train_model()), model=isolate(input$selected_model), test_df=isolate(test_data()), target_var=isolate(input$target_variable))

        
      } else {
        
        report_data$model_plots2 <- sapply(input$considered_models, function(i) group_confusion_plot(train_data=train_data(),test_data=isolate(test_data()),target_variable=isolate(input$target_variable), train_model=isolate(train_model()), selected_model=i), simplify = FALSE)
        group_confusion_plot(train_data=train_data(),test_data=isolate(test_data()),target_variable=isolate(input$target_variable), train_model=isolate(train_model()), selected_model=input$selected_model)
        
      }

    })

    output$shapley <- renderHighchart({
      
      shapley_cases <- sample(1:100, 2)
      
      report_data$shapley_case1 <- shapley_cases[1]
      report_data$shapley_case2 <- shapley_cases[2]

      report_data$shapley_plots1 <- sapply(input$considered_models, function(i) shapley_plot(target_variable = isolate(input$target_variable),train_model = isolate(train_model()), test_data = isolate(test_data()), selected_model = i, selected_case = shapley_cases[1], selected_class = isolate(input$selected_class)), simplify = FALSE)
      report_data$shapley_plots2 <- sapply(input$considered_models, function(i) shapley_plot(target_variable = isolate(input$target_variable),train_model = isolate(train_model()), test_data = isolate(test_data()), selected_model = i, selected_case = shapley_cases[2], selected_class = isolate(input$selected_class)), simplify = FALSE)
      
      shapley_plot(target_variable = input$target_variable,train_model = train_model(), test_data = test_data(), selected_model = input$selected_model, selected_case = input$selected_case, selected_class = input$selected_class)
      
    })
    
    output$Download <- downloadHandler(
      filename = "Model_Data.zip",
      content = function(file){
        
        withProgress(message = "Preparing selected model for export.", 
                     detail = "Please check your 'Downloads' folder.", value=1, {
          
          temp <- setwd(tempdir())
          on.exit(setwd(temp))
          
          files <- c("model_configuration.rds", "preprocessing_configuration.rds")
          
          model_configuration <- train_model()[[input$selected_model]]
          preprocessing_configuration <-data_processed()[["configuration"]]

          future({
            saveRDS(model_configuration,"model_configuration.rds")
            saveRDS(preprocessing_configuration, file="preprocessing_configuration.rds")
            zip(zipfile = file, files = files)
          }) 
          
        })
        
      }
    )
    
  })
  
  output$correlation_plot <- renderHighchart({
    
    data <- input_subset()
    
    report_data$correlation_plot <- correlation_plot(data)
    
    correlation_plot(data)
    
    
  })
  observeEvent(input$update_graph, {
    
    output$descriptive_plot <- renderHighchart({
      
      df <-isolate(input_subset())
      names <- names(df)[names(df) != isolate(input$target_variable)]
      report_data$variable_names  <- names
      
      if (is.numeric(df[[input$target_variable]])==TRUE) {
        
        report_data$eda_plots <- sapply(names, function(q) descriptive_plot(data = df, variable_y = isolate(input$target_variable), variable_x=q, variable_group = "NONE"), simplify = FALSE)
        
      } else {
        
        report_data$eda_plots <- sapply(names, function(q) descriptive_plot(data = df, variable_y = q, variable_x= "NONE", variable_group = isolate(input$target_variable)), simplify = FALSE)
        
      }
      
      descriptive_plot(df, variable_y = isolate(input$variable_y), variable_x = isolate(input$variable_x), variable_group = isolate(input$variable_group))

    })
    
  })
  
  # --------------------------------
  
  # HIDE SECTIONS IN SIDEBAR
  observeEvent(input$hideMe, {
    shinyjs::hide(selector = "ul.menu-open");
  })
  
  # CLOSE APP - FOR RINNO DESKTOP APPLICATION REQUIRED
  if (!interactive()) {
    session$onSessionEnded(function() { 
      stopApp() 
      q("no") 
      })
  }
  
}

# RUN APPLICATION --------------------------------------------------------------------------------------------------

shinyApp(ui, server)
