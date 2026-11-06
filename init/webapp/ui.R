
ns_interpret <- shiny::NS("ns_interpret")

ui <- navbarPage(
  title = "DANA",
  tabPanel(
    title = "About the app",
    div(
      class = "content",
      HTML("<h2>Welcome to <b>DANA</b>: <b>D</b>esign and <b>An</b>alysis <b>A</b>ssistant</h2>"),
      p("This app is designed to guide you through the process of selecting an appropriate quantitative study design based on research objectives and study conditions, and then analyze your data accordingly.  DANA has two parts."),
      p(),
      p(),
      HTML("<h4><u>QUANTITATIVE STUDY DESIGN DECISION TREE</u></h4>"),
      p("By answering a series of questions, you will be directed to the most suitable study design and provided with relevant resources and examples."),
      tags$ol(
        tags$li("Read the question displayed on the left side of the screen and choose the most appropriate answer."),
        tags$li("Click the 'Next' button to proceed to the next question."),
        tags$li("The app will dynamically update and guide you through a decision tree."),
        tags$li("At the end of the decision tree, you will be provided with your study type, example data formats, and additional considerations for your study design."),
        tags$li("If you want to start over, click the 'Start Over' button.")
        ),
      p("We hope this app assists you in planning and designing robust public health studies."),
      p(),
      p(),
      HTML("<h4><u>DATA ANALYSIS</u></h4>"),
      p("This app also provides tools to analyze your data."),
      tags$ol(
        tags$li("Upload your data"),
        tags$li("Choose your study design"),
        tags$li("Follow the subsequent directions (which will vary based on your data and study design).")
        )
      )
    ),
  tabPanel(
    title = "Decision Tree",
    fluidPage(
      tags$style(
        HTML(
          "
          body
          {
          background-image: url('https://prc.public-health.uiowa.edu/sites/prc.public-health.uiowa.edu/files/styles/ultrawide__1312_x_562/public/2022-05/PXL_20210909_001344683_2.jpg?h=6abcd482&itok=U2a29jwj'); /* Replace with the actual URL of the PRC UIowa logo */
          background-size: cover; /* Ensure the image covers the entire background */
          background-attachment: fixed; /* Fix the background image */
          font-family: 'Arial', sans-serif;
          color: #000000; /* UIowa black */
          }
          .navbar-default
          {
          background-color: #FFD700; /* UIowa gold */
          border-color: #FFD700;
          }
          .navbar-default .navbar-brand
          {
          color: #000000; /* Black text for branding */
          }
          .navbar-default .navbar-nav > li > a {
          color: #000000; /* Black text for navbar items */
          }
          .content
          {
          background-color: rgba(255, 255, 255, 0.9); /* White background with slight transparency */
          padding: 20px;
          border-radius: 10px;
          box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1);
          color: #000000; /* Black text */
          }
          .btn
          {
          background-color: #FFD700; /* UIowa gold */
          color: #000000; /* Black text */
          border-radius: 5px;
          border: none;
          }
          .btn:hover
          {
          background-color: #FFC107; /* Lighter gold */
          color: #000000; /* Black text */
          }
          .panel
          {
          background-color: rgba(255, 255, 255, 0.95); /* Slightly transparent white */
          border-radius: 10px;
          padding: 15px;
          box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1);
          }
          "
          )
        ),
      titlePanel("Quantitative Study Design Decision Tree"),
      sidebarLayout(
        sidebarPanel(
          uiOutput("question_ui"),
          actionButton("reset", "Start Over")
          ),
        mainPanel(
          div(class = "content",
              uiOutput("summary"),
              uiOutput("study_type"),
              uiOutput("data_example"),
              uiOutput("resources"),
              uiOutput("additional_considerations"),
              uiOutput("regression_output")
              )
          )
        )
      )
    ),
  tabPanel("Data Analysis",
    page_fluid(
      fluidRow(
        column(
          width = 12,
          theme = bs_theme(version = 5),
          titlePanel("Data Analysis"),
          class = "panel",
          tabsetPanel(
            tabPanel(
              title = "Upload Data",
              tags$h3(tags$b("Upload Data")),
              fileInput(
                inputId = "file",
                label = tags$p(
                  tags$b("Choose File:"),
                  tags$br("Accepts .csv .xlsx .rds")),
                accept = c(".csv", ".xlsx", ".rds")),
              checkboxInput("select_rows",
                            label = tagList(
                              "Select Custom Subset ",
                              tooltip(
                                bs_icon(
                                  name = "info-circle-fill",
                                  style = "color: #007bc0; cursor: pointer;"
                                ),
                                "
                                Choose this option if only a subset of the
                                data should be analyzed.
                                ",
                                placement = "right"
                              )
                            )),

              conditionalPanel(
                condition = "input.select_rows",
                numericInput("start_row", "Start Row (Row with Variable Names)", value = NULL),
                numericInput("end_row", "End Row (row with Final Observation)", value = NULL)
                ),
              actionButton("upload_data", "Upload Data"),
              conditionalPanel(
                condition = "input.upload_data > 0",
                dana::uiUploadInfo(id = "uploadinfo"),
                fluidRow(
                  column(
                    width = 3,
                    tags$h3(
                      tags$b("Variable Specification")
                      ),
                    selectInput(
                      inputId = "categorical_vars",
                      label = tagList(
                        "Select Categorical Variables: ",
                        tooltip(
                          bs_icon(
                            name = "info-circle-fill",
                            style = "color: #007bc0; cursor: pointer;"
                            ),
                          "
                          These are variables that do not have
                          inherent ordering. Examples can
                          include sex (M,F) or
                          treatment status (treatment, placebo).",
                          placement = "right"
                          )
                        ),
                      selected = NULL,
                      choices = NULL,
                      multiple = TRUE
                    ),
                    selectInput(
                      inputId ="continuous_vars",
                      label = tagList(
                        "Select Numeric Variables: ",
                        tooltip(
                          bs_icon(
                            name = "info-circle-fill",
                            style = "color: #007bc0; cursor: pointer;"
                            ),
                          "
                          These are variables that are inherently ordered.
                          Examples can include age or weight.
                          ",
                          placement = "right"
                          )
                        ),
                      selected = NULL,
                      choices = NULL,
                      multiple = TRUE
                      ),
                    conditionalPanel(
                      condition = "input.upload_data > 0",
                      uiOutput("ref_level_selection_ui")
                      ),
                    selectInput(
                      inputId = "ci_level",
                      label = "Select Credible Interval Level: ",
                      selected = 0.95,
                      choices = c("99%" = 0.99, "95%" = 0.95, "90%" = 0.90, "Custom" = "custom"),
                      multiple = FALSE
                    ),
                    conditionalPanel(
                      condition = "input.ci_level == 'custom'",
                      numericInput(
                        inputId = "ci_custom",
                        label = "Custom interval level: ",
                        value = 0.95,
                        min = 0.50,
                        max = 0.99,
                        step = 0.01
                    )
                  ),
                  uiOutput("ci_warning"),
                                    actionButton("continue", "Continue")),
                              column(width = 8,
                                     tags$h3(tags$b("Data Preview")),
                                     withSpinner(uiDataTable("data_table"), type = 4),
                                     offset = 1)
                            )
                            ),
                    ),
                   tabPanel("Data Exploration",
                            selectInput("grouping_vars", label = tagList("Select Grouping Variable: ",
                                                                         tooltip(
                                                                           bs_icon("info-circle-fill", style = "color: #007bc0; cursor: pointer;"),
                                                                           "This is a variable where repeated entries refer to a specific group. An example
                                                                           might be hospital IDs, where multiple patients are located in the same hospital.",
                                                                           placement = "right"
                                                                         )
                            ), selected = NULL, choices = NULL, multiple = FALSE),
                            fluidRow(
                              column(
                            conditionalPanel(
                              condition = "input.continuous_vars.length > 0",
                              uiContScrollPlot(id = "contscrollplot")),
                              width = 6
                              ),
                              column(
                            conditionalPanel(
                              condition = "input.categorical_vars.length > 0",
                              uiCatScrollPlot(id = "catscrollplot")),
                              width = 6
                              ),
                            conditionalPanel(
                              condition = "input.categorical_vars.length > 0 || input.continuous_vars.length > 0",
                              shiny::includeCSS(system.file(package="table1", "table1_defaults_1.0/table1_defaults.css")),
                              uiFiveNum(id = "fivenum"))
                            )),

                   tabPanel("Analysis Output",
                            fluidRow(
                              column(width = 3,
                                     tagList(tags$h3(tags$b("Study Design"))),
                                     selectInput(
                                       inputId = "study_design",
                                       label = NULL,
                                       selected = "Cross-sectional (Regression)",
                                       choices = c(
                                         "Cross-sectional (Regression)",
                                         "Cross-sectional (No Regression)",
                                         "Retrospective Cohort (No Regression)",
                                         "Retrospective Cohort (Regression)",
                                         "Prospective Cohort (No Regression)",
                                         "Prospective Cohort (Regression)",
                                         "Case-Control (Unmatched)",
                                         "Case-Control (Matched)",
                                         "Interrupted Time-Series",
                                         "Stepped Wedge",
                                         "Cluster Randomized Trial (CRT)",
                                         "Randomized Controlled Trial (RCT)"
                                       )),
                                     conditionalPanel(
                                       condition = "input.study_design == 'Cross-sectional (Regression)'",
                                       selectInput(
                                         "response_var_csr",
                                         "Response Variable:", choices = NULL),
                                       selectInput("response_type", "Response Variable Type:", choices = c("Count", "Binary", "Continuous")),
                                       conditionalPanel(
                                         condition = "input.response_type == 'Count'",
                                         selectInput("offset_var", label = tagList("Offset Variable:",
                                                                                   tooltip(
                                                                                     bs_icon("info-circle-fill", style = "color: #007bc0; cursor: pointer;"),
                                                                                     "This is a variable that refers to the area or duration of time over
                                                                                     which count data are collected. An example might be how long a particular
                                                                                     patient was monitored over in order to count the number of headaches they had.",
                                                                                     placement = "right"
                                                                                   )
                                         ), choices = c("None"), selected = "None")
                                       )
                                     ),
                                     conditionalPanel(
                                       condition = "input.study_design == 'Cross-sectional (No Regression)'",
                                       selectInput("t_test_type_csnr", "Response Comparison:", choices = c("Two Sample Mean", "One Sample Mean")),
                                       selectInput(
                                         "response_var_csnr",
                                         "Response Variable:", choices = NULL),
                                       conditionalPanel(
                                         condition = "input.t_test_type_csnr == 'Two Sample Mean'",
                                         selectInput("group_var_twosample_csnr", label = "Grouping Variable:", choices = NULL, selected = NULL),
                                         selectizeInput("group_vals_csnr", "Groups:", choices = NULL, multiple = TRUE,
                                                     options = list(maxItems = 2))
                                       ),
                                       conditionalPanel(
                                         condition = "input.t_test_type_csnr == 'One Sample Mean'",
                                          checkboxInput("one_sample_group_csnr", label = "Choose Group?"),
                                          conditionalPanel(
                                            condition = "input.one_sample_group_csnr",
                                            selectInput("group_var_onesample_csnr", label = "Grouping Variable:", choices = NULL, selected = NULL),
                                            selectInput("group_val_csnr", "Group:", choices = NULL)
                                       ),
                                     )),
                                     conditionalPanel(
                                       condition = "input.study_design == 'Retrospective Cohort (Regression)'",
                                       selectInput(
                                         "response_var_rcr",
                                         "Response Variable:", choices = NULL),
                                       selectInput("response_type_rcr", "Response Variable Type:", choices = c("Count", "Binary", "Continuous")),
                                       conditionalPanel(
                                         condition = "input.response_type_rcr == 'Count'",
                                         selectInput("offset_var_rcr", "Offset Variable:", choices = c("None"), selected = "None")
                                       ),
                                       selectInput(
                                         "time_var_rcr",
                                         "Time Variable:", choices = NULL),
                                       selectInput(
                                         "group_var_rcr",
                                         "Group Variable:", choices = NULL),
                                     ),

                                     actionButton("run_analysis", "Run Analysis")),
                              column(width = 8,
                                     tagList(tags$h3(tags$b("Study Info"))),
                                     uiStudyInfo("studyinfo")),
                            ),
                            conditionalPanel(
                              condition = "input.run_analysis",
                              hr(),
                              tags$h3(tags$b("Reported Quantities")),
                              withSpinner(uiOutput("analysis_interpret_notes"), type = 4),
                              tags$h3(tags$b("Results")),
                              withSpinner(DTOutput(outputId = "analysis_info"), type= 4),
                            ),
                            hr(),
                            tagList(tags$h3(tags$b("Interpretation"))),
                            conditionalPanel(
                              condition = "(input.study_design === 'Cross-sectional (Regression)' ||
                                            input.study_design === 'Retrospective Cohort (Regression)') &&
                                            input.run_analysis > 0",
                              selectInput(
                              inputId = "id_vars_interpret_selection",
                              label = "Select a variable to interpret",
                              choices = NULL,
                              selected = NULL,
                              multiple = TRUE),
                              uiOutput("id_vars_interpret"),
                              uiOutput("analysis_interpret"),
                            ),
                            hr(),
                            uiOutput("analysis_interpret"),
                            conditionalPanel("input.run_analysis > 0",
                              checkboxInput("show_interpret_advanced", "Show advanced information", FALSE),
                            ),
                            conditionalPanel(
                              condition = "(input.study_design === 'Cross-sectional (Regression)' ||
                                            input.study_design === 'Retrospective Cohort (Regression)') &&
                                            input.show_interpret_advanced",
                              tags$h3(tags$b("Regression Fit Info")),
                              actionButton("show_flowchart", "Show Flowchart"),
                              DTOutput(outputId = "fit_info"),
                              uiOutput("interpret_advanced")

                            ),
                            conditionalPanel(
                              condition = "(input.study_design === 'Cross-sectional (No Regression)') &&
                                            input.show_interpret_advanced",
                              tags$h3(tags$b("Model Fit Info")),
                              uiOutput("interpret_advanced"),
                              DTOutput(outputId = "fit_info"),
                              uiScrollPlot(id = "scrollplot")
                            ),
                            hr(),








                            ),

                 )

           )
      )
    ),
           div(class = "content",
               HTML("<b>Disclaimer:</b>"),
               p("The results and visualizations provided by this application are intended solely as a preliminary exploration of your study data.
                 These analyses should be considered a first pass and are not a substitute for professional statistical consultation.
                 We strongly recommend consulting with a trained statistician or biostatistician to ensure that appropriate models are selected,
                 assumptions are met, and all potential analytical issues are adequately addressed.")
               )
  ),

  # Model Cascade -------------------------------------------------------------------

  tabPanel(
    title = "Model Cascade",
    page_fluid(
      titlePanel("Model Cascade Information"),
      selectInput(
        inputId = "id_select_flowchart",
        choices = c("Continuous", "Count"),
        label = "Select flowchart"),
      grVizOutput("demo_flowchart")
      )
    ),

  # Examples -------------------------------------------------------------------



  # About Page Tab
  tabPanel("Who we are",
           fluidPage(
             div(class = "content",
                 h2("About the Authors"),
                 HTML("This work is part of the <a href='https://prc.public-health.uiowa.edu/'>University of Iowas Prevention Research Center for Rural Health</a>"),
                 HTML("<br><br><br>"),
                 HTML("Abran Nicolas, PhD Candidate, Department of Biostatistics</a><br>"),
                 HTML("<a href='https://www.linkedin.com/in/samuella-boadi-a87437219/'>Samuella Boadi, PhD Candidate, Department of Biostatistics</a>"),
                 p(),
                 HTML("<a href='https://www.public-health.uiowa.edu/people/daniel-sewell/'>Daniel K. Sewell, Associate Professor, Department of Biostatistics</a>")

             )
           )
  ),
  tabPanel("Glossary",
           fluidPage(
             div(class = "content",
                 tags$p(tags$b("Goodness of Fit (GOF)"),
                        paste0("
                               Quantification of the match between observed values
                               and those predicted by the model. In a nonparametric
                               regression setting, this is calculated by the average
                               absolute error between the posterior mean of each observation
                               and its observed value. Lower values indicate
                               the model predictions match the data closely, while higher
                               values indicate that they do not match.
                               "
                               )),
                 tags$p(tags$b("Bayesian P-Value (BPVal)"),
                        paste0("
                                A measure of goodness of fit evaluated through
                                the posterior predictive distribution. In particular,
                                in the regression setting, compares
                                a statistic calculated among the observed
                                data and the predicted data. The choice
                                of test statistic is highly flexible, but
                                in this case uses the Shaprio Wilk test statistic
                                for gaussian response regression and deviance
                                otherwise. The BPVal is the probability that the statistic
                                is larger among the observed data
                                relative to the predictive data.
                                Values of the BPVal that are close 0 or 1 indicate
                                inadequate fit from the model.
                                to 0 or 1 indicate inadequate fit
                               "
                        )),
                 tags$p(tags$b("Log Transformation"),
                        paste0("
                                Models that represent outcomes on the log scale
                                sometimes model the observed data than those that do not
                                transform. In a regression setting, this amounts to taking the log
                                of the outcome variable and modelling this as the outcome.
                               "
                        )),


                 HTML("<h1> <b> Study Designs </b> </h1>"),
                 HTML("<p> <b> Cross Sectional </b> A type of study design which captures
                      information at a single point in time. Individuals are sampled
                      without regard to disease or exposure status. Due to the lack
                      of a time component, these typically cannot establish causal effects between
                      covariates and outcomes. </p>"),
                 HTML("<b> Case Control </b> A type of study design where individuals
                      are selected according to their disease status. The outcome
                      of interest is often the odds ratio of disease in the exposed
                      population compared to disease in the nonexposed population")

             )
           )
  )




)

