
# Shiny - Experis Demo ----------------------------------------------------

# Packages
pacman::p_load(
  tidyverse,
  rlang,
  shiny,
  shinyBS,
  shinydashboard,
  scales,
  plotly,
  DT,
  rhandsontable)


# Load: demo_prepare()
# Load: demo_model()
# Load: demo_score()
source("R/fx_prep.R")
source("R/fx_model.R")
source("R/fx_score.R")
source("R/fx_segment.R")

# Load:
## .plot_theme for ggplot
## .fx_describe() for table summary
source("R/fx_utility_functions.R")


# UI ----------------------------------------------------------------------

ui <- dashboardPage(

  dashboardHeader(title = "Predictive Analytics Demonstration"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Prepare", tabName = "ui_prepare", icon = icon("wrench", lib = "glyphicon")),
      menuItem("Model",   tabName = "ui_model",   icon = icon("tasks", lib = "glyphicon")),
      menuItem("Score",   tabName = "ui_score",   icon = icon("fullscreen", lib = "glyphicon"))
    )
  ),

  dashboardBody(

    tabItems(


      # | UI - Prepare -----------------------------------------------------------


      tabItem(tabName = "ui_prepare",

              fluidRow(
                box(width = 3,
                    selectInput("data_path", label = "Select Raw Data", choices = dir("data", pattern = ".csv")),
                    textInput("data_delim",   label = "Data Delimiter", value = ";"),
                    hr(),
                    actionButton("prep_import", "Import")),

                box("Data Summary", DTOutput("data"), width = 9, solidHeader = TRUE)
              ),

              fluidRow(

                box("Explore Variable", uiOutput("qplot_column_list"), width = 3, status = "info"),
                box("Variable Plot", plotOutput("quick_plot"), width = 9, status = "info")

              ),

              fluidRow(
                box("Prepare Training Data", width = 3,
                    uiOutput("id_column_list"),
                    uiOutput("sample_column_list"),
                    numericInput("p_training", label = "Holdout Proportion", value = 0.60, min = 0, max = 1, step = 0.05),
                    uiOutput("drop_column_list"),
                    textInput("row_filter", label = "Filter Logic", value = NULL, placeholder = "col_1 > 10 & col_2 == 'value' & ..."),
                    hr(),
                    h5(strong("Create New Columns")), style = "text-align:left",
                    h6(em("Double click to add values.", br(), "Right click to add rows.")),
                    rHandsontableOutput("rht_newcols")
                ),

                column(width = 3,

                       box(width = 12,
                           actionButton("prepare_data_submit", "Submit All Manipulations", icon("refresh", lib = "glyphicon")),
                           hr(),
                           actionButton("export_data_submit", "Save Data", icon("floppy-save", lib = "glyphicon")),
                           hr(),

                           radioButtons("prep_segment_create",
                                        "Create Segments",
                                        inline = TRUE,
                                        selected = FALSE,
                                        choiceNames = c("Yes", "No"),
                                        choiceValues = c(TRUE, FALSE)),
                           radioButtons("prep_segment_search",
                                        "Segment Search",
                                        selected = TRUE,
                                        choiceNames = c("Automatic", "Manual"),
                                        choiceValues = c(TRUE, FALSE)),
                           numericInput("prep_segment_k", "Number of Segments", value = 20, min = 1, max = 100)
                       )
                ),

                tabBox(width = 6,
                       # tabPanel("Debug List", textOutput("debug")),
                       tabPanel("Training Data", DTOutput("train_view")),
                       tabPanel("Holdout Data", DTOutput("holdout_view")),
                       tabPanel("Segment Summary", DTOutput("segment_view"))
                )
              )
      ),


      # | UI - Model ------------------------------------------------------------

      tabItem(tabName = "ui_model",

              fluidRow(
                box(width = 3,
                    uiOutput("model_data_list"),
                    hr(),
                    actionButton("model_data_import", label = "Import")
                ),

                box("Training Data Summary", width = 9, DTOutput("model_data_train"))
              ),

              fluidRow(
                box(width = 2,
                    radioButtons("model_selected_algorithms",
                                 label = "Select Models to Build",
                                 choices = c(
                                   "Auto" = "auto",
                                   "All" = "all",
                                   "Deep Learning" = "dl",
                                   "Gradient Boosted Trees" = "gbm",
                                   "Logistic Regression" = "lr",
                                   "Random Forest" = "drf"),
                                 selected = "all")),

                box("Model Parameters", width = 2,
                    uiOutput("model_data_list_id"),
                    uiOutput("model_data_list_group"),
                    uiOutput("model_data_list_response")

                ),

                tabBox(width = 8,
                       tabPanel("Overview",
                                actionButton("model_build", "Build Models"),
                                actionButton("model_export", "Save Champion Model"),
                                actionButton("model_shutdown_h2o", "Shutdown H2O"),
                                h6("Please allow ~90 seconds per model."),
                                hr(),
                                DTOutput("model_overview")),
                       tabPanel("ROC Curve", plotOutput("model_roc", height = 500)),
                       tabPanel("Cumulative Lift", plotOutput("model_lift", height = 500)),
                       tabPanel("Cumulative Capture Rate", plotOutput("model_capture", height = 500)),
                       tabPanel("Variable Importance", plotOutput("model_varimp", height = 500)))

              )

              # fluidRow(shiny::textOutput("model_debug"))


      ),


      # | UI - Score ------------------------------------------------------------

      tabItem(tabName = "ui_score",
              fluidRow(
                box(width = 3, label = NULL,
                    uiOutput("score_model_list"),
                    actionButton("score_import_model", "Import Model")
                ),
                box(width = 3, label = NULL,
                    uiOutput("score_data_list"),
                    actionButton("score_import_data", "Import Data")
                ),
                box(width = 3, label = NULL,
                    uiOutput("score_data_list_id"),
                    hr(),
                    actionButton("score_submit", "Begin Score"),
                    actionButton("score_export", "Export Score"),
                    actionButton("score_shutdown_h2o", "Shutdown H2O"),
                    h6("Please allow ~5 minutes for scoring")
                )
              ),

              fluidRow(
                tabBox(width = 12,
                       # tabPanel("Debug", textOutput("score_debug")),
                       tabPanel("Model Summary", DTOutput("score_model_summary")),
                       tabPanel("Data Summary", DTOutput("score_data_summary")),
                       tabPanel("Score Summary", DTOutput("score_summary")),
                       tabPanel("Cumulative Lift Chart", plotOutput("score_lift", height = 600))
                )
              )

      ) # Close Score Tab
    ) # Close Dashboard Tabs
  ) # Close Dashboard Body
) # Close UI



# Server ------------------------------------------------------------------

server <- function(input, output) {


  # Tab - Preparation -------------------------------------------------------


  output$data_path <- renderText(input$data_path)

  raw_data_frame <- eventReactive(input$prep_import, {
    input$data_path %>%
      str_c("data/", .) %>%
      read_delim(input$data_delim)
  })

  raw_data_column_names <- reactive(raw_data_frame() %>% colnames())

  output$data <- renderDT(
    raw_data_frame() %>%
      .fx_describe() %>%
      datatable(extensions = c('Scroller'),
                options = list(dom = 't',
                               scrollY = 200,
                               scroller = TRUE,
                               scrollX = TRUE)))

  basic_table_new_cols <-
    tibble(Name = rep(na_chr, 4),
           Expression = rep(na_chr, 4))

  output$rht_newcols <- renderRHandsontable(
    basic_table_new_cols %>% rhandsontable(rowHeaders = NULL)
  )

  new_cols_logic <- reactive(
    input$rht_newcols %>%
      hot_to_r() %>%
      filter(!is.na(Name)) %>%
      deframe()
  )

  data_prep_final <- reactive(
    raw_data_frame() %>%
      mutate(!!! new_cols_logic())
  )

  output$data_prep_final <- renderDT({
    data_prep_final() %>%
      datatable(extensions = c('Scroller'),
                options = list(dom = 't',
                               scrollY = 200,
                               scroller = TRUE,
                               scrollX = TRUE))

  })




  output$qplot_column_list <- renderUI(
    selectInput("qplot_column",
                label = NULL,
                choices = raw_data_column_names()))

  output$id_column_list <- renderUI(
    selectizeInput("col_id",
                   label = "ID Column",
                   multiple = TRUE,
                   selected = NULL,
                   choices = raw_data_column_names(),
                   options = list(maxItems = 1, placeholder = "NULL")))

  output$sample_column_list <- renderUI(
    selectizeInput("sample_group",
                   label = "Sampling Column",
                   multiple = TRUE,
                   selected = NULL,
                   choices = raw_data_column_names(),
                   options = list(maxItems = 1, placeholder = "NULL")))

  output$drop_column_list <- renderUI(
    selectizeInput("cols_drop",
                   label = "Drop Columns",
                   choices = raw_data_column_names(),
                   selected = NULL,
                   multiple = TRUE,
                   options = list(placeholder = 'NULL'))

  )


  output$quick_plot <- renderPlot(
    if (input$qplot_column %>% is_empty()) NULL else {
      qplot(x = !! sym(input$qplot_column), data = raw_data_frame()) + .plot_theme
    }

  )

  output$debug <- renderPrint({
    list(
      id = input$col_id,
      sm = input$sample_group,
      pr = input$p_training,
      dr = input$cols_drop,
      f2 = reactive_row_filter(),
      nc = new_cols_logic(),
      p_training = input$p_training,
      sample_group = input$sample_group,
      create_segments = input$prep_segment_create,
      segment_estimate_k = input$prep_segment_search,
      segment_k = input$prep_segment_k
    )
  })

  reactive_row_filter <- reactive({

    if (str_length(input$row_filter) > 0) input$row_filter else NULL

  })


  data_prep_intermediate <- eventReactive(
    input$prepare_data_submit, {
      demo_prepare(data = raw_data_frame(),
                   data_path = NULL,
                   col_id = input$col_id,
                   cols_new = new_cols_logic(),
                   cols_drop = input$cols_drop,
                   row_filter = reactive_row_filter(),
                   p_training = input$p_training,
                   sample_group = input$sample_group,
                   create_segments = input$prep_segment_create %>% parse_expr(),
                   segment_estimate_k = input$prep_segment_search %>% parse_expr(),
                   segment_k = input$prep_segment_k)
    })

  output$train_view <- renderDT({
    data_prep_intermediate()$train %>%
      .fx_describe() %>%
      datatable(extensions = c('Scroller'),
                options = list(dom = 't',
                               scrollY = 500,
                               scroller = TRUE,
                               scrollX = TRUE))
  })

  output$holdout_view <- renderDT({
    data_prep_intermediate()$holdout %>%
      .fx_describe() %>%
      datatable(extensions = c('Scroller'),
                options = list(dom = 't',
                               scrollY = 500,
                               scroller = TRUE,
                               scrollX = TRUE))
  })


  output$segment_view <- renderDT({

    if (input$prep_segment_create == FALSE) {
      NULL
    } else {
      data_prep_intermediate()$segment %>%
        mutate_if(is_numeric, scales::number_format(0.001, big.mark = ",")) %>%
        datatable(extensions = c('Scroller'),
                  options = list(dom = 't',
                                 scrollY = 500,
                                 scroller = TRUE,
                                 scrollX = TRUE))
    }
  })


  observeEvent(
    input$export_data_submit, {

      if (data_prep_intermediate()$train %>% is.data.frame() %>% is_false()) {
        session$sendCustomMessage(type = 'testmessage', message = 'Thank you for clicking')
      } else {
        write_rds(data_prep_intermediate()$train, str_glue("data/train/{Sys.Date()}_train_data.rds"), compress = "gz")
        write_rds(data_prep_intermediate()$holdout,   str_glue("data/train/{Sys.Date()}_holdout_data.rds"), compress = "gz")
      }

    })


  # Tab - Model -------------------------------------------------------------

  output$model_data_list <- renderUI(
    selectizeInput("model_data_path",
                   label = "Select Training Data",
                   multiple = TRUE,
                   selected = NULL,
                   choices = dir("data/train", pattern = ".rds"),
                   options = list(maxItems = 1, placeholder = "NULL"))
  )


  model_data_train <- eventReactive(input$model_data_import, {
    str_glue("data/train/{input$model_data_path}") %>% read_rds()
  })


  output$model_data_train <- renderDT(
    model_data_train() %>%
      .fx_describe() %>%
      datatable(extensions = c('Scroller'),
                options = list(dom = 't',
                               scrollY = 200,
                               scroller = TRUE,
                               scrollX = TRUE)))


  model_data_column_list <- reactive(
    model_data_train() %>% colnames()
  )


  # Model - ID Parameter
  output$model_data_list_id <- renderUI(
    selectizeInput("model_data_id",
                   label = "ID Column",
                   multiple = TRUE,
                   selected = NULL,
                   choices = model_data_column_list(),
                   options = list(maxItems = 1, placeholder = "NULL"))
  )


  # Model - Group Parameter
  output$model_data_list_group <- renderUI(
    selectizeInput("model_data_group",
                   label = "Group Column",
                   multiple = TRUE,
                   selected = NULL,
                   choices = model_data_column_list(),
                   options = list(maxItems = 1, placeholder = "NULL"))
  )


  # Model - Response Parameter
  output$model_data_list_response <- renderUI(
    selectizeInput("model_data_response",
                   label = "Response Column",
                   multiple = TRUE,
                   selected = NULL,
                   choices = model_data_column_list(),
                   options = list(maxItems = 1, placeholder = "NULL"))
  )


  # Model - Algorithm Selection
  model_algorithm_list <- reactive(
    if (input$model_selected_algorithms == "all") {
      c("drf", "gbm", "dl", "lr")
    } else {
      input$model_selected_algorithms
    })


  # Model - Process
  model_results <- eventReactive(input$model_build, {
    demo_model(data = model_data_train(),
               response = input$model_data_response,
               group = input$model_data_group,
               col_id = input$model_data_id,
               algorithms = model_algorithm_list())
  })


  # Model - Summary
  output$model_overview <- renderDT(
    model_results() %>%
      pluck("metadata_results") %>%
      unnest(summary) %>%
      select_if(~!is.list(.)) %>%
      mutate(algorithm = str_to_upper(algorithm),
             auc = number(auc, accuracy = 0.001),
             rmse = number(rmse, accuracy = 0.001),
             runtime = number(runtime, accuracy = 0.01)) %>%
      datatable(extensions = c('Scroller'),
                options = list(dom = 't',
                               scrollY = 300,
                               scroller = TRUE,
                               scrollX = TRUE))
  )


  # Model - Plot Process
  model_plots <- reactive(
    model_results() %>%
      pluck("metadata_plots") %>%
      deframe()
  )


  # Model - Plot Lift
  output$model_lift <- renderPlot(
    model_plots() %>% pluck("plot_lift")
  )


  # Model - Plot Capture Rate
  output$model_capture <- renderPlot(
    model_plots() %>% pluck("plot_capture")
  )


  # Model - Plot ROC
  output$model_roc <- renderPlot(
    model_plots() %>% pluck("plot_roc")
  )


  # Model - Plot Variable Importance
  output$model_varimp <- renderPlot(
    model_plots() %>% pluck("plot_varimp")
  )

  observeEvent(input$model_export, {

    inform("Export RDS")
    model_results() %>%
      pluck("metadata_results") %>%
      select(-model) %>%
      write_rds(str_glue("data/model/{Sys.Date()}_model.rds"), compress = "gz")

    inform("Export H2O Model")
    model_results() %>%
      pluck("metadata_results") %>%
      filter(champion == 1) %>%
      distinct(model_id) %>%
      pull(model) %>%
      walk(~ .x %>% h2o.saveModel("data/model/h2o", force = TRUE))

  })

  observeEvent(input$model_shutdown_h2o, {
    inform("Shutdown H2O")
    h2o.shutdown(prompt = FALSE)
  })


  output$model_debug <- renderPrint(
    list(path = str_glue("data/train/{input$model_data_path}"),
         algs = model_algorithm_list(),
         exact  = input$model_export,
         expath = str_glue("data/model/{Sys.Date()}_models.rds"))
  )


  # Tab - Score -------------------------------------------------------------
  output$score_model_list <- renderUI(
    selectizeInput("score_model_path",
                   label = "Select Model",
                   multiple = TRUE,
                   selected = NULL,
                   choices = dir("data/model", pattern = ".rds"),
                   options = list(maxItems = 1, placeholder = "NULL"))
  )


  output$score_data_list <- renderUI(
    selectizeInput("score_data_path",
                   label = "Select Data",
                   multiple = TRUE,
                   selected = NULL,
                   choices = dir("data/train", pattern = ".rds"),
                   options = list(maxItems = 1, placeholder = "NULL"))
  )


  score_data <- eventReactive(input$score_import_data, {
    str_glue("data/train/{input$score_data_path}") %>% read_rds()
  })

  score_model <- eventReactive(input$score_import_model, {
    str_glue("data/model/{input$score_model_path}") %>% read_rds()
  })

  score_data_column_list <- reactive(
    score_data() %>% colnames()
  )

  output$score_data_list_id <- renderUI(
    selectizeInput("score_data_id",
                   label = "ID Column",
                   multiple = TRUE,
                   selected = NULL,
                   choices = score_data_column_list(),
                   options = list(maxItems = 1, placeholder = "NULL"))
  )


  output$score_model_summary <- renderDT(
    score_model() %>%
      select(algorithm, runtime, group_id, response, auc, champion) %>%
      mutate(algorithm = str_to_upper(algorithm),
             auc = number(auc, accuracy = 0.001),
             runtime = number(runtime, accuracy = 0.01)) %>%
      datatable(extensions = c('Scroller'),
                options = list(dom = 't',
                               scrollY = 400,
                               scroller = TRUE,
                               scrollX = TRUE))
  )

  output$score_data_summary <- renderDT(
    score_data() %>%
      .fx_describe() %>%
      datatable(extensions = c('Scroller'),
                options = list(dom = 't',
                               scrollY = 400,
                               scroller = TRUE,
                               scrollX = TRUE))

  )

  score_results <- eventReactive(input$score_submit, {
    demo_score(data = score_data(),
               model = score_model(),
               col_id = input$score_data_id)
  })


  output$score_summary <- renderDT(
    score_results() %>%
      pluck("lift_table") %>%
      ungroup() %>%
      mutate_at(vars(model_id), as_factor) %>%
      mutate_at(vars(n, cumulative_n), scales::comma) %>%
      mutate_at(vars(cumulative_data_fraction), scales::percent, accuracy = 0.1) %>%
      mutate_at(vars(response_rate, cumulative_response_rate), scales::percent, accuracy = 0.01) %>%
      mutate_at(vars(lower_threshold, lift, cumulative_lift), scales::number, accuracy = 0.0001) %>%
      datatable(extensions = c('Scroller'),
                rownames = FALSE,
                filter = "top",
                options = list(scrollY = 400,
                               scroller = TRUE,
                               scrollX = TRUE))
  )

  output$score_lift <- renderPlot(
    score_results() %>% pluck("lift_plot")

  )

  observeEvent(input$score_export, {
    inform("Output Raw Scores")
    score_results() %>% pluck("score") %>% write_csv(str_glue("data/score/{Sys.Date()}_score.csv"))

    inform("Output Score RDS")
    score_results() %>%
      enframe() %>%
      write_rds(
        str_glue("data/score/{Sys.Date()}_score.rds",
                 compress = "bz2"))
  })

  observeEvent(input$score_shutdown_h2o, {
    inform("Shutdown H2O")
    h2o.shutdown(prompt = FALSE)
  })

  output$score_debug <- renderPrint(
    list(score_path = input$score_data_path,
         model_path = input$score_model_path,
         button_model = input$score_import_model,
         button_data = input$score_import_data,
         id_col = input$score_data_id,
         input_path = str_glue("data/model/{input$score_model_path}"),
         model = score_model() %>% class(),
         data = score_data() %>% class(),
         button_score = input$score_submit
    )
  )


}


# Submit ------------------------------------------------------------------


shinyApp(ui = ui, server = server)

