server <- function(input, output, session) {

  # Reactive Values ------------------------------------------------------------

  user_data <- reactiveVal(NULL) # Stores dataframe of user-inputted data

  valid_vars_check <- reactive({

    req(c(input$continuous_vars, input$categorical_vars, input$grouping_vars))

    if(anyDuplicated(c(input$continuous_vars, input$categorical_vars))){
      FALSE
    } else {
      TRUE
    }}) # Check for valid input variables

  ref_levels <- reactive({

    req(input$categorical_vars)

    sapply(seq_len(length(input$categorical_vars)), function(x) {
      input[[paste0("ref_", x)]]
    }, simplify = TRUE)
  }) # Reactive list of reference levels for categorical variables

  ci_level <- reactive({
    val <- if (input$ci_level == "custom") {
      input$ci_custom
      } else {
        as.numeric(input$ci_level)
      }
    val
  }) # Credible interval reactive

  # Observe Events -------------------------------------------------------------

  ## Execute when user presses 'Upload Data' -------
  observeEvent(
    eventExpr = list(
      input$upload_data
    ),
    handlerExpr = {

    req(input$file)

    user_data(dana::read_data(file = input$file,
                    start_row = input$start_row,
                    end_row = input$end_row)
    )

    serverUploadInfo(id = "uploadinfo",
                       data = user_data(),
                     cat_vars = input$categorical_vars,
                     cont_vars = input$continuous_vars,
                     ci_level = ci_level())
    serverDataTable(id = "data_table",
                    data = user_data())

    # Determine variable types
    var_types <- dana::detect_var_type(user_data())

    updateSelectInput(
      session,
      inputId = "categorical_vars",
      choices = var_types$all,
      selected = var_types$categorical
    )
    updateSelectInput(
      session,
      inputId = "continuous_vars",
      choices = var_types$all,
      selected = var_types$continuous
    )

    updateSelectInput(
      session,
      inputId = "grouping_vars",
      choices = c("None", var_types$categorical),
      selected = "None"
    )

  })

  ## Execute when user presses 'Continue' or when any variables are changed ----

  observeEvent(
    eventExpr = list(
      input$continuous_vars, input$categorical_vars, ci_level()
    ),
    handlerExpr = {

      req(user_data())

      serverUploadInfo(id = "uploadinfo",
                       data = user_data(),
                       cat_vars = input$categorical_vars,
                       cont_vars = input$continuous_vars,
                       ci_level = ci_level())
    updateSelectInput(
      session,
      inputId = "grouping_vars",
      choices = c("None", input$categorical_vars),
      selected = "None"
    )

    }

  )


  # Reference Level UI ---------------------------------------------------------

  output$ref_level_selection_ui <- renderUI({

    req(user_data(), input$categorical_vars)

    if(!length(input$categorical_vars) == 0){
    level_names <- input$categorical_vars

    level_vals <- lapply(input$categorical_vars, function(x){
      unique(user_data()[[x]])
    })

    ## Reference level widget ids indexed by ref_i
    level_ids <- lapply(1:length(input$categorical_vars), function(i) {
        paste0("ref_", i)
      })

    ## Render all of the specified selectInput widgets
    tagList(
      hr(),
      tags$p(tags$b("Select reference levels for categorical variables:")),
      lapply(1:length(level_names), function(i){
        selectInput(inputId = level_ids[[i]],
                    label = level_names[i],
                    choices = level_vals[[i]])
      })
    )
    }
  })





  # Render exploratory data analysis --------------------------------

  observeEvent(
    input$grouping_vars,

    handlerExpr = {

      req(user_data())

      serverContScrollPlot(id = "contscrollplot", data = user_data(), cont_vars = input$continuous_vars, group_var = input$grouping_vars
                           )
      serverCatScrollPlot(id = "catscrollplot", data = user_data(), cat_vars = input$categorical_vars, group_var = input$grouping_vars
      )

      serverFiveNum(id = "fivenum", data = user_data(), cat_vars = input$categorical_vars,
                    cont_vars = input$continuous_vars,
                    group_var = input$grouping_vars)
  })













    observe({

    req(user_data())

    updateSelectInput(
      session,
      "response_var_csr",
      choices = names(user_data())
    )

    updateSelectInput(
      session,
      "response_var_csnr",
      choices = names(user_data())
    )


  })

  observe({

    req(user_data())

    updateSelectInput(
      session,
      "group_var_twosample_csnr",
      choices = names(user_data())
    )
  })

  observe({

    req(user_data())


    updateSelectInput(
      session,
      "group_var_onesample_csnr",
      choices = names(user_data())
    )
  })


  observe({

    req(user_data())

    updateSelectInput(
      session,
      "group_vals_csnr",
      choices = user_data()[[input$group_var_twosample_csnr]],
      selected = user_data()[[input$group_var_twosample_csnr]][1:2]
    )

    updateSelectInput(
      session,
      "group_val_csnr",
      choices = user_data()[[input$group_var_onesample_csnr]]
    )



  })

  observe({

    req(user_data())

    updateSelectInput(
      session,
      "offset_var",
      choices = c("None",names(user_data()))
    )
  })



  observe({

    req(user_data())

    updateSelectInput(
      session,
      "time_var_rcr",
      choices = c(names(user_data()))
    )

    updateSelectInput(
      session,
      "response_var_rcr",
      choices = c(names(user_data()))
    )

    updateSelectInput(
      session,
      "group_var_rcr",
      choices = c(names(user_data()))
    )

    updateSelectInput(
      session,
      "offset_var_rcr",
      choices = c("None", names(user_data()))
    )

  })




  observeEvent(input$study_design, {
    serverStudyInfo(id = "studyinfo", type = input$study_design)
      })

  ns_interpret <- shiny::NS("analysis_interpret")

  # Run based on selected analysis ----------------------------------
  observeEvent(input$run_analysis, {

    req(user_data())
    all_vars <- names(user_data())

    output$go_on <- renderUI(NULL)
    output$analysis_interpret <- renderUI(NULL)
    output$analysis_interpret_notes <- renderUI(NULL)
    output$fit_info <- renderDT(NULL)
    output$analysis_info <- renderDT(NULL)

    pdir_notes <- (paste0("PDir measures the maximum certainty that an effect is strictly positive or negative. ",
                          "A PDir greater than 0.975 is often considered statistically significant. ",
                          "However, this cutoff is not strict and should be interpreted in context."))

    switch(input$study_design,
      "Cross-sectional (Regression)" = {
        if (input$response_type == "Continuous"){

          analysis2 <- fit_cross_sectional(
            response_type = input$response_type,
            data = user_data()[, all_vars],
            prior = "improper",
            response_var = input$response_var_csr,
            pred_cat_vars = setdiff(input$categorical_vars, input$response_var_csr),
            pred_cont_vars = setdiff(input$continuous_vars, input$response_var_csr),
            pred_cat_vars_ref_levels = ref_levels(),
            ci_level = ci_level()
          )
          print(analysis2)
          print("here1")
          print("here")
          print("here3")
          output$flowchart <- renderGrViz({
           grViz(analysis2$flowchart)
          })








        analysis <- cross_sectional(data = user_data()[,all_vars],
                                    prior = "improper",
                                    family = "gaussian",
                                    outcome_var = input$response_var_csr,
                                    pred_cat = setdiff(input$categorical_vars, input$response_var_csr),
                                    pred_cont = setdiff(input$continuous_vars, input$response_var_csr),
                                    ref_levels = ref_levels(),
                                    ci_level = ci_level())
          }

        if (input$response_type == "Count"){
          analysis <- cross_sectional(data = user_data()[,all_vars],
                                      prior = "improper",
                                      family = "poisson",
                                      outcome_var = input$response_var_csr,
                                      offset_var = input$offset_var,
                                      pred_cat = setdiff(input$categorical_vars, c(input$offset_var,input$response_var_csr)),
                                      pred_cont = setdiff(input$continuous_vars, c(input$offset_var,input$response_var_csr)),
                                      ref_levels = ref_levels(),
                                      ci_level = ci_level())
        }

        if (input$response_type == "Binary"){
          analysis <- cross_sectional(data = user_data()[,all_vars],
                                      prior = "improper",
                                      family = "binomial",
                                      outcome_var = input$response_var_csr,
                                      pred_cat = setdiff(input$categorical_vars, input$response_var_csr),
                                      pred_cont = setdiff(input$continuous_vars, input$response_var_csr),
                                      ref_levels = ref_levels(),
                                      ci_level = ci_level())

          print(analysis$bpval)

          if (analysis$bpval < 0.95 && analysis$bpval > 0.05){
          serverFlowchart(id = "flowchart", vals = analysis$bpval, final_node = 4)}
          serverFlowchart(id = "flowchart", vals = analysis$bpval, final_node = 3)


        }
        serverScrollPlot(id = "scrollplot", ggplot_list = analysis$plots)
        print(length(analysis$plots))
        output$analysis_info <- renderDT({analysis$table_hr})
        output$fit_info <- renderDT({analysis$fit_table})

        observe({
          req(analysis$interpret)

          updateSelectInput(
            session,
            ns_interpret("vars_interpret"),
            choices = names(analysis$interpret),
            selected = character(0)
          )
        })

        observeEvent(input[[ns_interpret("select_all_vars_interpret")]], {
          updateSelectInput(
            session,
            ns_interpret("vars_interpret"),
            selected = names(analysis$interpret)
          )
        })

        observeEvent(input[[ns_interpret("clear_all_vars_interpret")]], {
          updateSelectInput(
            session,
            ns_interpret("vars_interpret"),
            selected = character(0)
          )
        })


        output$analysis_interpret <- renderUI({
          req(length(input[[ns_interpret("vars_interpret")]]) > 0)
          tagList(
            lapply(input[[ns_interpret("vars_interpret")]], function(x) {
              var <- analysis$interpret[[x]]
              tagList(
                tags$h4(x),
                tags$p(
                  tags$strong("Effect size: "),
                  var$estimate
                ),
                tags$p(
                  tags$strong("PDir: "),
                  var$pdir
                ),
                tags$p(
                  tags$strong("ROPE: "),
                  var$rope
                ),
                tags$div(style = "margin-bottom: 25px;")
              )
            })
          )
        })

        output$analysis_interpret_notes <- renderUI({
          tagList(
            tags$hr(),
            tags$h4(paste0(ci_level() * 100,"% Credible Interval")),
            tags$p(paste0("The \"", ci_level() * 100, "% CI\" reported here is a credible interval. Based on the data, there is a ", ci_level() * 100, "% probability that the true effect falls within this range.")),
            tags$h4("Probability of Direction (PDir)"),
            tags$p(pdir_notes),
            tags$h4("Region of Practical Equivalence (ROPE)"),
            tags$p(analysis$interpret_notes),
            tags$hr()
          )
        })

        output$go_on <- renderUI({
          tagList(
          tags$h3(tags$b("Diagnostic Check")),
            tags$p(analysis$diagnostic)
          )
        })

      },
      "Cross-sectional (No Regression)" = {



        if (input$t_test_type_csnr == "One Sample Mean"){

          # req((input$group_var_onesample_csnr != input$response_var_csnr) || input$one_sample_group_csnr == 0)

          analysis <- dana::t_test(
            data = user_data(),
            outcome_var = input$response_var_csnr,
            two_sample = FALSE,
            group_var = input$group_var_onesample_csnr,
            one_sample_group_val = input$group_val_csnr,
            ci_level = ci_level()
          )

          output$analysis_info <- renderDT({analysis$table_hr})
          output$fit_info <- renderDT({analysis$fit_table})
          serverScrollPlot(id = "scrollplot", ggplot_list = analysis$plots)
          output$analysis_interpret <- renderUI({HTML(paste(analysis$interpret, "<br>"))})
          output$analysis_interpret_notes <- renderUI({
            tagList(
              tags$hr(),
              tags$h4(paste0(ci_level() * 100,"% Credible Interval")),
              tags$p(paste0("The \"", ci_level() * 100, "% CI\" reported here is a credible interval. Based on the data, there is a ", ci_level() * 100, "% probability that the true parameter of interest falls within this range.")),
              tags$hr()
            )
          })
        }

        if (input$t_test_type_csnr == "Two Sample Mean"){

          req(input$group_var_twosample_csnr != input$response_var_csnr)

          analysis <- dana::t_test(
            data = user_data(),
            outcome_var = input$response_var_csnr,
            two_sample = TRUE,
            group_var = input$group_var_twosample_csnr,
            two_sample_group_val = input$group_vals_csnr,
            ci_level = ci_level()
          )

          output$analysis_info <- renderDT({analysis$table_hr})
          output$fit_info <- renderDT({analysis$fit_table})
          serverScrollPlot(id = "scrollplot", ggplot_list = analysis$plots)

          output$analysis_interpret <- renderUI({
            tagList(
              tags$strong("Population mean:"),
              tags$ul(
                lapply(analysis$interpret$mean, tags$li)
              ),

              tags$strong("Population variance:"),
              tags$ul(
                lapply(analysis$interpret$var, tags$li)
              ),

              tags$p(
                tags$strong("Population difference: "),
                analysis$interpret$diff
              )
            )
          })

          output$analysis_interpret_notes <- renderUI({
            tagList(
              tags$hr(),
              tags$h4(paste0(ci_level() * 100,"% Credible Interval")),
              tags$p(paste0("The \"", ci_level() * 100, "% CI\" reported here is a credible interval. Based on the data, there is a ", ci_level() * 100, "% probability that the true parameter of interest falls within this range.")),
              tags$h4("Region of Practical Equivalence (ROPE)"),
              tags$p(analysis$interpret_notes),
              tags$hr()
            )
          })

          output$go_on <- renderUI({
            tagList(
              tags$h3(tags$b("Diagnostic Check")),
              tags$p(analysis$diagnostic)
            )
          })
        }
      },
      "Retrospective Cohort (Regression)" = {
        if (input$response_type_rcr == "Continuous"){
          analysis <- dana::retrospective_cohort_regression(
            data = user_data(),
            family = "gaussian",
            outcome_var = input$response_var_rcr,
            time_var = input$time_var_rcr,
            pred_cat = setdiff(input$categorical_vars, c(input$response_var_rcr, input$time_var_rcr, input$group_var_rcr)),
            pred_cont = setdiff(input$continuous_vars, c(input$response_var_rcr, input$time_var_rcr, input$group_var_rcr)),
            group_var = input$group_var_rcr,
            ref_levels = ref_levels(),
            ci_level = ci_level()
          )
        }

        if (input$response_type_rcr == "Count"){
          analysis <- dana::retrospective_cohort_regression(
            data = user_data(),
            family = "poisson",
            outcome_var = input$response_var_rcr,
            time_var = input$time_var_rcr,
            pred_cat = setdiff(input$categorical_vars, c(input$response_var_rcr, input$time_var_rcr, input$offset_var_rcr, input$group_var_rcr)),
            pred_cont = setdiff(input$continuous_vars, c(input$response_var_rcr, input$time_var_rcr, input$offset_var_rcr, input$group_var_rcr)),
            group_var = input$group_var_rcr,
            ref_levels = ref_levels(),
            offset_var = input$offset_var_rcr,
            ci_level = ci_level()
          )
        }

        if (input$response_type_rcr == "Binary"){
          analysis <- dana::retrospective_cohort_regression(
            data = user_data(),
            family = "binomial",
            outcome_var = input$response_var_rcr,
            time_var = input$time_var_rcr,
            pred_cat = setdiff(input$categorical_vars, c(input$response_var_rcr, input$time_var_rcr, input$group_var_rcr)),
            pred_cont = setdiff(input$continuous_vars, c(input$response_var_rcr, input$time_var_rcr, input$group_var_rcr)),
            group_var = input$group_var_rcr,
            ref_levels = ref_levels(),
            ci_level = ci_level()
          )
        }

        # Connect analysis to UI components
        output$analysis_info <- renderDT({analysis$table_hr})
        glmm_ui <- serverGLMMInterpret(id = "analysis_interpret", analysis = analysis)
        output$analysis_interpret <- renderUI({ glmm_ui() })
        output$analysis_interpret_notes <- renderUI({
          tagList(
            tags$hr(),
            tags$h4(paste0(ci_level() * 100,"% Credible Interval")),
            tags$p(paste0("The \"", ci_level() * 100, "% CI\" reported here is a credible interval. Based on the data, there is a ", ci_level() * 100, "% probability that the true effect falls within this range.")),
            tags$h4("Probability of Direction (PDir)"),
            tags$p(pdir_notes),
            tags$hr()
          )
        })
        output$fit_info <- renderDT({analysis$fit_table})
        serverScrollPlot(id = "scrollplot", ggplot_list = analysis$plots)
        output$go_on <- renderUI({
          tagList(
            tags$h3(tags$b("Diagnostic Check")),
            tags$p(analysis$diagnostic)
          )
        })

      }
    )


  })


  # output$variable_inputs <- renderUI({ #Fix this name
  #
  #     req(user_data())
  #     data <- user_data()
  #     all_vars <- names(data)
  #
  #     switch(input$study_design,
  #     "Cross-sectional (No Regression)" = {
  #       tagList()
  #     },
  #     "Cross-sectional (Regression)" = {
  #       tagList(
  #           selectInput("response_var", "Response Variable:", choices = all_vars),
  #           selectInput("response_type", "Response Variable Type:", choices = c("Count", "Binary", "Continuous")),
  #           conditionalPanel(
  #             condition = "input.response_type == 'Count'",
  #             selectInput("offset_var", "Offset Variable:", choices = c("None", all_vars), selected = "None")
  #           )
  #       )
  #     },
  #     "Retrospective Cohort (No Regression)" = {
  #       tagList(
  #         selectInput("group_var", "Grouping Variable:", choices = all_vars),
  #         selectInput("time_var", "Time Variable:", choices = all_vars),
  #         selectInput("response_var", "Response Variable:", choices = all_vars)
  #       )
  #     },
  #     "Prospective Cohort (No Regression)" = {
  #       tagList(
  #         selectInput("group_var", "Grouping Variable:", choices = all_vars),
  #         selectInput("time_var", "Time Variable:", choices = all_vars),
  #         selectInput("response_var", "Response Variable:", choices = all_vars)
  #       )
  #     },
  #     "Prospective Cohort (Regression)" = {
  #       tagList(
  #         selectInput("response_var", "Response Variable:", choices = all_vars),
  #         selectInput("time_var", "Time Variable:", choices = all_vars),
  #         #selectInput("covariates", "Covariates:", choices = choices, multiple = TRUE),
  #         selectInput("group_var", "Grouping Variable:", choices = all_vars))
  #       },
  #     "Retrospective Cohort (Regression)" = {
  #       tagList(
  #         selectInput("response_var", "Response Variable:", choices = all_vars),
  #         selectInput("time_var", "Time Variable:", choices = all_vars),
  #         #selectInput("covariates", "Covariates:", choices = choices, multiple = TRUE),
  #         selectInput("group_var", "Grouping Variable:", choices = choices)
  #       )
  #     },
  #       "Case-Control (Unmatched)" = {
  #         tagList(
  #           selectInput("response_var", "Response Variable:", choices = all_vars),
  #           #selectInput("covariates", "Covariates:", choices = choices, multiple = TRUE),
  #           selectInput("status", "Case/Control:", choices = choices)
  #         )
  #     },
  #       "Case-Control (Matched)" = {
  #         tagList(
  #           selectInput("response_var", "Response Variable:", choices = all_vars),
  #           selectInput("covariates", "Covariates:", choices = all_vars, multiple = TRUE),
  #           selectInput("group_var", "ID for marching:", choices = all_vars),
  #           selectInput("status", "Case/Control:", choices = all_vars)
  #       )
  #       },
  #       "Interrupted Time-Series" = {
  #         tagList(
  #           selectInput("analysis_type", "Select Analysis Type:", choices = c("Visualize Data", "Analysis")),
  #
  #                     conditionalPanel(
  #                       condition = "input.analysis_type == 'Visualize Data'",
  #                       selectInput("response_var", "Response Variable:", choices = all_vars),
  #                       selectInput("time_var", "Time Variable:", choices = all_vars),
  #                       selectInput("intervention_var", "Intervention Variable:", choices = all_vars)
  #                     ),
  #
  #                     conditionalPanel(
  #                       condition = "input.analysis_type == 'Analysis'",
  #                       selectInput("response_var", "Response Variable:", choices = all_vars),
  #                       selectInput("x_var", "(x) Variable:", choices = all_vars),
  #                       selectInput("time_var", "Time Variable:", choices = all_vars),
  #                       selectInput("intervention_var", "Intervention Variable:", choices = all_vars)
  #                     )
  #
  #                   )
  #
  #       },
  #       "Stepped Wedge" = {
  #                   tagList(
  #                     selectInput("response_var", "Response Variable:", choices = all_vars),
  #                     selectInput("treatment_var", "Treatment Variable:", choices = all_vars),
  #                     selectInput("subject_id", "Subject ID:", choices = all_vars),
  #                     selectInput("time_var", "Time Variable:", choices = all_vars),
  #                     selectInput("covariates", "Covariates:", choices = all_vars, multiple = TRUE)
  #                   )
  #       },
  #                 "Cluster Randomized Trial (CRT)" = {
  #                   tagList(
  #                     selectInput("response_var", "Response Variable:", choices = all_vars),
  #                     selectInput("treatment_var", "Treatment Variable:", choices = all_vars),
  #                     selectInput("cluster_var", "Cluster Variable:", choices = all_vars),
  #                     selectInput("covariates", "Covariates:", choices = all_vars, multiple = TRUE)
  #                   )
  #       },
  #                 "Randomized Controlled Trial (RCT)" = {
  #                   tagList(
  #                     selectInput("response_var", "Response Variable:", choices = all_vars),
  #                     selectInput("treatment_var", "Treatment Variable:", choices = all_vars)
  #                   )
  #                 })
  #        })

  # Perform analysis based on the selected design
  # observeEvent(input$run_analysis, {
  #
  #   # --------------------------------------------------------------------------
  #   # Define all isolated, nondynamic objects. This is to prevent
  #   # code from running excessively. These values only update upon clicking 'run_analysis'
  #
  #   data <- isolate(user_data())
  #   design <- isolate(input$study_design)
  #   file <- isolate(input$file)
  #
  #   nominal_vars <- isolate(input$nominal_vars)
  #   binary_vars <- isolate(input$binary_vars)
  #   ordinal_vars <- isolate(input$ordinal_vars)
  #   continuous_vars <- isolate(input$continuous_vars)
  #   discrete_vars <- isolate(input$discrete_vars)
  #
  #   categorical_vars <- c(nominal_vars, binary_vars, ordinal_vars)
  #   numeric_vars <- c(continuous_vars, discrete_vars)
  #
  #   response_var <- isolate(input$response_var)
  #   group_var <- isolate(input$group_var)
  #
  #   selected_covariates <- setdiff(c(nominal_vars, binary_vars, ordinal_vars, continuous_vars, discrete_vars), response_var)
  #   selected_numeric <- setdiff(numeric_vars, response_var)
  #   selected_categorical <- setdiff(categorical_vars, response_var)
  #
  #   offset_var <- isolate(input$offset_var)
  #
  #   response_type <- isolate(input$response_type)
  #
  #   treatment_var <- isolate(input$treatment_var)
  #   time_var <- isolate(input$time_var)
  #   subject_id <- isolate(input$subject_id)
  #   strata_var <- isolate(input$strata_var)
  #   intervention_var <- isolate(input$intervention_var)
  #   status <- isolate(input$status)
  #   cluster_var <- isolate(input$cluster_var)
  #   y <- isolate(input$response_var)
  #   x_var <- isolate(input$x_var)
  #
  #
  #   # --------------------------------------------------------------------------
  #   # Clear any lingering outputs. This runs every time the analysis
  #   # is run, and plots according to the inputs are then generated to
  #   # possibly replace them.
  #
  #   ## Clear plots
  #   output$plot_output_1 <- renderPlot({ plot.new() })
  #   output$plot_output_2 <- renderPlot({ plot.new() })
  #   output$plot_output_3 <- renderPlot({ plot.new() })
  #
  #   ## Clear tables
  #   output$table_output_1 <- renderTable(NULL)
  #   output$table_output_2 <- renderTable(NULL)
  #
  #   ## Clear text
  #   output$formula_display <- renderText({ "" })
  #
  #   ## Clear UI elements if needed
  #   output$Some_UI <- renderUI({ NULL })
  #
  #   # --------------------------------------------------------------------------







  # Reactive values to keep track of the state of the decision tree
  tree_data <- reactiveValues(
    question = "What is your primary objective?",  # Initial question
    options = c("Descriptive", "Cause-and-Effect"),  # Initial options
    details = "Choose whether your study is aimed at describing characteristics (Descriptive) or determining cause-and-effect relationships (Cause-and-Effect).",
    path = NULL,  # Keep track of the user's choices
    terminal_node = FALSE  # Check if we've reached a terminal node
  )

  # Dynamically update the question and options based on the user's choices
  output$question_ui <- renderUI({
    if (!tree_data$terminal_node) {
      tagList(
        h3(tree_data$question),  # Show the current question
        p(tree_data$details),    # Show additional details or examples
        radioButtons("answer", "Choose one:", choices = tree_data$options),
        actionButton("Next", "Next")  # Button to go to the next question
      )
    }
  })

  # Observe when the "Next" button is pressed
  observeEvent(input$Next, {
    # Ensure the user has selected an answer
    if (!is.null(input$answer)) {
      tree_data$path <- c(tree_data$path, input$answer)  # Add the selected answer to the path

      # Update the question and options based on the user's input
      if (input$answer == "Descriptive") {
        tree_data$question <- "What is the timeframe for your study?"
        tree_data$options <- c("Cross-sectional", "Retrospective", "Prospective")
        tree_data$details <- "Choose the timeframe of your study:\n- Cross-sectional: Snapshot in time\n- Retrospective: Looking back\n- Prospective: Following subjects into the future."
      } else if (input$answer == "Cause-and-Effect") {
        tree_data$question <- "Is your study observational or experimental?"
        tree_data$options <- c("Observational", "Experimental")
        tree_data$details <- "Choose whether your study is observational (no intervention) or experimental (an intervention is performed)."

        # Observational Study -> Rare Disease Question
      } else if (input$answer == "Observational") {
        tree_data$question <- "Is your study focusing on a rare disease?"
        tree_data$options <- c("Rare", "Not rare")
        tree_data$details <- "Choose whether your study is focused on a rare disease, which may influence the choice of study design (e.g., case-control study)."

        # If the user selects "Yes" for rare disease, show Case-control study
      } else if (input$answer == "Rare" && tree_data$path[length(tree_data$path)-1] == "Observational") {
        tree_data$terminal_node <- TRUE
        output$study_type <- renderUI({
          HTML(paste("Study Design: Case-control Study. Learn more <a href='https://health.ucdavis.edu/media-resources/ctsc/documents/pdfs/case-control-studies-2016.pdf'>here</a>"))
        })
        output$data_example <- renderUI({
          tagList(
            h4("Example of Case-control Data:"),
            p("Researchers aimed to investigate the association between smoking and lung cancer.
              They compared a group of patients diagnosed with lung cancer (cases) to a similar group without lung cancer (controls). The columns of the data consist of anonymized participant ID, age, gender, ethnicity, smoking status, years of smoking, lung cancer diagnosis, family history of lung cancer, and exposure to secondhand smoke."),
            h4("Additional Consideration"),
            p("Can you meaningfully match cases to controls?  (You may match more than one control to each case.)")
          )
        })



        # If the user selects "No" for rare disease, branch into timeframes for the observational study
      } else if (input$answer == "Not rare" && tree_data$path[length(tree_data$path)-1] == "Observational") {
        tree_data$question <- "What is the timeframe for your study?"
        tree_data$options <- c("Cross-sectional", "Retrospective", "Prospective")
        tree_data$details <- "Choose the timeframe of your study:\n- Cross-sectional: Snapshot in time\n- Retrospective: Looking back\n- Prospective: Following subjects into the future."

        # Cross-sectional terminal node
      } else if (input$answer == "Cross-sectional") {
        tree_data$terminal_node <- TRUE
        output$study_type <- renderUI({
          HTML(paste("Study Design: Cross-sectional Observational Study. Learn more <a href='https://www.sciencedirect.com/science/article/pii/S0012369220304621'>here.</a>"))
        })
        output$data_example <- renderUI({
          tagList(
            h4("Example of Cross-sectional Data:"),
            p("Researchers took well water samples from randomly selected wells across Iowa and tested for several enteric pathogens using PCR.  The columns of the data consist of an anonymized well ID and the PCR Ct values for each of the pathogens tested."),
            p("This is a snapshot of data collected at a single time point.")
          )
        })

        # Retrospective terminal node
      } else if (input$answer == "Retrospective") {
        tree_data$terminal_node <- TRUE
        output$study_type <- renderUI({
          HTML(paste("Study Design: Retrospective Cohort Observational Study. Learn more <a href='https://www.sciencedirect.com/topics/medicine-and-dentistry/retrospective-cohort-study'>here.</a>"))
        })
        output$data_example <- renderUI({
          tagList(
            h4("Example of Retrospective Cohort Data:"),
            p("Researchers analyzed medical records from patients who visited a hospital in Iowa between 2010 and 2020. They aimed to describe the prevalence of diabetes and related conditions such as hypertension, cardiovascular disease, and kidney disease. The columns of the data consist of anonymized patient ID, year of visit, age, gender, ethnicity, diabetes diagnoses, hypertension diagnoses, cardiovascular disease  diagnoses, and kidney disease diagnoses."),
            p("This data traces back from the outcome to previous exposures.")
          )
        })

        # Prospective terminal node
      } else if (input$answer == "Prospective") {
        tree_data$terminal_node <- TRUE
        output$study_type <- renderUI({
          HTML(paste("Study Design: Prospective Cohort Observational Study. Learn more  <a href='https://www.sciencedirect.com/topics/medicine-and-dentistry/prospective-cohort-study'>here.</a>"))
        })
        output$data_example <- renderUI({
          tagList(
            h4("Example of Prospective Cohort Data:"),
            p("Researchers initiated a study to observe the incidence of various lung diseases in children living in urban areas of Iowa over a five-year period. They aimed to describe the prevalence of lung diseases such as asthma, bronchitis, pneumonia, and COPD. The columns of the data consist of anonymized child ID, year of enrollment, age, gender, ethnicity, rural-urban continuum, asthma diagnoses, bronchitis diagnoses, pneumonia diagnoses, and COPD diagnoses."),
            p("This data follows individuals over time to observe future outcomes.")
          )
        })

        # Experimental Study -> Control group question
      } else if (input$answer == "Experimental") {
        tree_data$question <- "Does your study have a control group?"
        tree_data$options <- c("Controls present", "No controls")
        tree_data$details <- "Choose whether your study includes a control group, which can affect the study's structure."

        # Experimental -> Yes control group -> Unit of randomization
      } else if (input$answer == "Controls present" && tree_data$path[length(tree_data$path)-1] == "Experimental") {
        tree_data$question <- "What is the unit of randomization?"
        tree_data$options <- c("Group/Cluster", "Individual")
        tree_data$details <- "Choose the unit of randomization for your study."

        # Experimental -> No control group -> Intervention in waves
      } else if (input$answer == "No controls" && tree_data$path[length(tree_data$path)-1] == "Experimental") {
        tree_data$question <- "Can the intervention be done in waves?"
        tree_data$options <- c("Waves", "No waves")
        tree_data$details <- "Choose whether the intervention can be phased in (e.g., Stepped Wedge Design)."

        # Group/Cluster terminal node
      } else if (input$answer == "Group/Cluster") {
        tree_data$terminal_node <- TRUE
        output$study_type <- renderUI({
          HTML(paste("Study Design: Cluster Randomized Trial (CRT). Learn more  <a href='https://www.taylorfrancis.com/books/mono/10.4324/9781315370286/cluster-randomised-trials-richard-hayes-lawrence-moulton'>here.</a>"))
        })
        output$data_example <- renderUI({
          tagList(
            h4("Example of CRT Data:"),
            p("Researchers aimed to evaluate the effectiveness of a community-based intervention to reduce childhood obesity rates in rural areas of Iowa. Schools were randomly assigned to either implement the intervention or continue with standard practices. The columns of the data consist of anonymized school ID, anonymized student ID, age, gender, ethnicity, BMI, physical activity levels, intervention arm (intervention, control)")
          )
        })

        # Individual terminal node
      } else if (input$answer == "Individual") {
        tree_data$terminal_node <- TRUE
        output$study_type <- renderUI({
          HTML(paste("Study Design: Randomized Clinical Trial (RCT). Learn more  <a href='https://ajronline.org/doi/full/10.2214/ajr.183.6.01831539'>here.</a>"))
        })
        output$data_example <- renderUI({
          tagList(
            h4("Example of RCT Data:"),
            p("Researchers aimed to evaluate the effectiveness of a community-based smoking cessation program in reducing smoking rates among adults in Iowa. Participants were randomly assigned to either the intervention group, which received the smoking cessation program, or the control group, which received standard health education. The columns of the data consist of anonymized participant ID, age, gender, ethnicity, smoking status, number of cigarettes smoked per day, nicotine dependence level, and treatment arm (intervention, control)."),
            h4("Additional Considerations:"),
            p("Are there any spatial considerations, or clustering (e.g., patients within the same clinic) of the data?  If yes, make sure to record latitude/longitude, county, or cluster identifier as applicable.")

          )
        })

        # Stepped Wedge terminal node
      } else if (input$answer == "Waves" && tree_data$path[length(tree_data$path)-1] == "No controls") {
        tree_data$terminal_node <- TRUE
        output$study_type <- renderUI({
          HTML(paste("Study Design: Stepped Wedge Design. Learn more  <a href='https://link.springer.com/article/10.1186/1471-2288-6-54'>here.</a>"))
        })
        output$data_example <- renderUI({
          tagList(
            h4("Example of Stepped Wedge Data:"),
            p("Researchers aimed to evaluate the effectiveness of a new community-based nutrition program in improving dietary habits and reducing obesity rates among adults in Iowa. The program was rolled out sequentially to different communities over a two-year period. The columns of the data consist of anonymized community ID, date of implementation, anonymized participant ID, age, gender, ethnicity, BMI, dietary intake, physical activity level, and obesity designation.")
          )
        })

        # Interrupted Time Series (ITS) terminal node
      } else if (input$answer == "No waves" && tree_data$path[length(tree_data$path)-1] == "No controls") {
        tree_data$terminal_node <- TRUE
        output$study_type <- renderUI({
          HTML(paste("Study Design: Interrupted Time Series (ITS). Learn more  <a href='https://link.springer.com/article/10.1023/A:1010024016308'>here.</a>"))
        })
        output$data_example <- renderUI({
          tagList(
            h4("Example of ITS Data:"),
            p("Researchers aimed to evaluate the impact of a new traffic safety law on reducing the number of road traffic accidents in Iowa. The law, which introduced stricter penalties for speeding and mandatory seatbelt use, was implemented in January 2020. The study analyzed monthly data on road traffic accidents before and after the law's implementation. The columns of the data consist of month, year, number of road traffic accidents, number of fatal accidents, number of roadside injuries, average speed of vehicles involved in accidents, and seatbelt usage rate.")
          )
        })
      }

      # Display additional considerations
      # output$additional_considerations <- renderUI({
      #   tagList(
      #     h4("Additional Considerations:"),
      #     p("Are there any spatial considerations, or clustering (e.g., patients within the same clinic) of the data?  If yes, make sure to record latitude/longitude, county, or cluster identifier as applicable.")
      #   )
      # })

      # Display resources for data security and privacy
      output$resources <- renderUI({
        tagList(
          h4("Data Security and Privacy Resources:"),
          p("Ensure that the following practices are implemented to protect data privacy and security:"),
          tags$ul(
            tags$li(a("Creating anonymous IDs", href = "https://docs.tealium.com/server-side/visitor-stitching/anonymous-user-visitor-id-attributes/")),
            tags$li(a("Guidelines for data security", href = "https://cloudian.com/guides/data-protection/data-protection-and-privacy-7-ways-to-protect-user-data/"))
          )
        )
      })

      # Display the selected path as a summary
      output$summary <- renderUI({
        HTML(paste("You have selected:", paste(tree_data$path, collapse = " &rarr; ")))
      })
    }
  })


  #Downloading plots
  output$download_all_plots <- downloadHandler(
    filename = function() {
      paste0("DANA_Plots_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Open PNG device with dimensions for 3 stacked plots
      png(file, width = 10, height = 18, units = "in", res = 300)

      # Arrange only non-null plots vertically
      plots_to_draw <- list(plots$plot1, plots$plot2, plots$plot3)
      plots_to_draw <- Filter(Negate(is.null), plots_to_draw)

      if (length(plots_to_draw) == 0) {
        plot.new()
        text(0.5, 0.5, "No plots to save", cex = 2)
      } else {
        gridExtra::grid.arrange(grobs = plots_to_draw, ncol = 1)
      }

      dev.off()
    }
  )


  # Reset the decision tree to the initial state
  observeEvent(input$reset, {
    tree_data$question <- "What is your primary objective?"
    tree_data$options <- c("Descriptive", "Cause-and-Effect")
    tree_data$details <- "Choose whether your study is aimed at describing characteristics (Descriptive) or determining cause-and-effect relationships (Cause-and-Effect)."
    tree_data$path <- NULL
    tree_data$terminal_node <- FALSE
    output$study_type <- renderText("")
    output$data_example <- renderUI(NULL)
    output$additional_considerations <- renderUI(NULL)
    output$resources <- renderUI(NULL)
  })




  #-----------------------------------------------------------------------------



}



