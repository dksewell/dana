
library(shiny)
library(bslib)
library(DT)
library(shinycssloaders)
library(shinyBS)
library(bsicons)

ns_interpret <- shiny::NS("analysis_interpret")

ui <- navbarPage(
  title = "DANA",


  # About Page app
  tabPanel("About the app",
           # Introduction page
           div(class = "content",
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
               ))


  ),
  # Study Design Decision Tree Tab
  tabPanel("Decision Tree",
           fluidPage(
             # Add custom CSS for background
             tags$style(HTML("
               body {
                 background-image: url('https://prc.public-health.uiowa.edu/sites/prc.public-health.uiowa.edu/files/styles/ultrawide__1312_x_562/public/2022-05/PXL_20210909_001344683_2.jpg?h=6abcd482&itok=U2a29jwj'); /* Replace with the actual URL of the PRC UIowa logo */
                 background-size: cover; /* Ensure the image covers the entire background */
                 background-attachment: fixed; /* Fix the background image */
                 font-family: 'Arial', sans-serif;
                 color: #000000; /* UIowa black */
               }
               .navbar-default {
                 background-color: #FFD700; /* UIowa gold */
                 border-color: #FFD700;
               }
               .navbar-default .navbar-brand {
                 color: #000000; /* Black text for branding */
               }
               .navbar-default .navbar-nav > li > a {
                 color: #000000; /* Black text for navbar items */
               }
               .content {
                 background-color: rgba(255, 255, 255, 0.9); /* White background with slight transparency */
                 padding: 20px;
                 border-radius: 10px;
                 box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1);
                 color: #000000; /* Black text */
               }
               .btn {
                 background-color: #FFD700; /* UIowa gold */
                 color: #000000; /* Black text */
                 border-radius: 5px;
                 border: none;
               }
               .btn:hover {
                 background-color: #FFC107; /* Lighter gold */
                 color: #000000; /* Black text */
               }
               .panel {
                 background-color: rgba(255, 255, 255, 0.95); /* Slightly transparent white */
                 border-radius: 10px;
                 padding: 15px;
                 box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1);
               }
             ")),

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
                     # Add the regression output here
                     #h3("Regression Analysis Results"),  # Optional: Section Title
                     uiOutput("regression_output")
                 )
               )
             )
           )
  ),


  # Data Analysis Tab
  tabPanel("Data Analysis",
    page_fluid(
      fluidRow(
        column(width = 12,
      theme = bs_theme(version = 5),
      titlePanel("Data Analysis"),
                 class = "panel",
                 tabsetPanel(
                   tabPanel("Upload Data",
                            HTML("<h3> <b> Upload Data </b> </h3>"),
                            fileInput("file",
                                      label = tags$p(tags$b("Choose File:"),
                                                     tags$br("Accepts .csv .xlsx .rds")),
                                      accept = c(".csv", ".xlsx", ".rds")),
                            checkboxInput("select_rows", "Select custom data?"),
                            conditionalPanel(
                              condition = "input.select_rows",
                              numericInput("start_row", "Start Row (Row with Variable Names)", value = NULL),
                              numericInput("end_row", "End Row (row with Final Observation)", value = NULL)
                            ),
                            actionButton("upload_data", "Upload Data"),
                            conditionalPanel(
                              condition = "input.upload_data > 0",
                              uiUploadInfo(id = "uploadinfo"),
                            fluidRow(
                              column(width = 3,
                                     tags$h3(tags$b("Variable Specification")),

                                    selectInput("categorical_vars", label = tagList("Select Categorical Variables: ",
                                                                                    tooltip(
                                                                                      bs_icon("info-circle-fill", style = "color: #007bc0; cursor: pointer;"),
                                                                                      "These are variables that do not have inherent ordering. Examples can
                                                                                      include sex (M,F) or treatment status (treatment, placebo).",
                                                                                      placement = "right"
                                                                                    )
                                    ), selected = NULL, choices = NULL, multiple = TRUE),
                                    selectInput("continuous_vars", label = tagList("Select Continuous Variables: ",
                                                                                   tooltip(
                                                                                     bs_icon("info-circle-fill", style = "color: #007bc0; cursor: pointer;"),
                                                                                     "These are variables that are inherently ordered. Examples can include age or
                                                                                     weight.",
                                                                                     placement = "right"
                                                                                   )
                                    ), selected = NULL, choices = NULL, multiple = TRUE),
                                    conditionalPanel(
                                      condition = "input.upload_data > 0",
                                      uiOutput("ref_level_selection_ui")
                                    ),
                                    selectInput("ci_level", "Select Credible Interval Level: ",
                                                selected = 0.95,
                                                choices = c("99%" = 0.99, "95%" = 0.95, "90%" = 0.90, "Custom" = "custom"),
                                                multiple = FALSE),
                                    conditionalPanel(
                                      condition = "input.ci_level == 'custom'",
                                      numericInput("ci_custom", "Custom interval level: ",
                                                   value = 0.95,
                                                   min = 0.50,
                                                   max = 0.99,
                                                   step = 0.01)
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
                            conditionalPanel(
                              condition = "input.continuous_vars.length > 0",
                              uiContScrollPlot(id = "contscrollplot")),
                            conditionalPanel(
                              condition = "input.categorical_vars.length > 0",
                              uiCatScrollPlot(id = "catscrollplot")),
                            conditionalPanel(
                              condition = "input.categorical_vars.length > 0 || input.continuous_vars.length > 0",
                              shiny::includeCSS(system.file(package="table1", "table1_defaults_1.0/table1_defaults.css")),
                              uiFiveNum(id = "fivenum")),
                            ),

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
                              tags$h3(tags$b("Results")),
                              withSpinner(DTOutput(outputId = "analysis_info"), type= 4),
                            ),

                            tagList(tags$h3(tags$b("Interpretation"))),

                            conditionalPanel(
                              condition = "(input.study_design === 'Cross-sectional (Regression)' ||
                                            input.study_design === 'Retrospective Cohort (Regression)') &&
                                            input.run_analysis > 0",
                              selectInput(
                              inputId = ns_interpret("vars_interpret"),
                              label = "Select a variable to interpret",
                              choices = NULL,
                              selected = NULL,
                              multiple = TRUE),
                              actionButton(ns_interpret("select_all_vars_interpret"), "Select all"),
                              actionButton(ns_interpret("clear_all_vars_interpret"), "Clear selection"),
                              tags$br(),
                            ),


                            uiOutput("analysis_interpret"),
                            uiOutput("analysis_interpret_notes"),

                            checkboxInput("show_interpret_advanced", "Show advanced information", FALSE),
                            conditionalPanel(
                              condition = "input.show_interpret_advanced",
                              uiOutput("go_on"),
                                tags$h3(tags$b("Fit Info")),
                                conditionalPanel(
                                  condition = "input$scrollplot > 0",
                                  uiScrollPlot(id = "scrollplot")
                                ),
                              hr(),
                                DTOutput(outputId = "fit_info")
                              ),
                            uiOutput("interpret_advanced"),







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

  # Examples -------------------------------------------------------------------

  tabPanel("Examples",
           fluidPage(
             div(class = "content",
                 HTML("<h4> Below is an example data analysis in which we demonstrate how to use
                    DANA. </h4>"),
                 HTML("The goal of this example analysis is to understand
                 <b> What is the relationship between clinical characteristics and
                      probability of diabetes in Pima Indian women? </b>"),
                 HTML("The data contain 532 complete case records among women living near Phoenix, AZ
                      of Pima Indian heritage. A variety of clinical data were collected
                      by the US National Institute of
                      Diabetes and Digestive and Kidney Diseases.
                      Data are sourced from the <i> kmed </i> package in R."),
                 HTML("<h3> <b> Understanding Study Design </b> </h3>"),
                 HTML("It is important
                   to have an understanding of the type of study design involved, as
                   this determines the type of questions we may answer and the
                   types of analyses involved. DANA allows for the selection of a number
                   of different study designs, though two common ones include:"),
                 HTML("<h5> <b> Cross Sectional </b>  - This refers to a study performed
                   at a specific point in time. As such, there is no
                   time variable involved. These types of studies
                   are often observational, without researchers introducing
                   a treatment or intervention </h5>"),
                 HTML("<h5> <b> Retrospective Cohort </b> - This refers to a predefined
                      group of study units that are followed over time. Their
                      outcomes are tracked over time. </h5>"),
                 HTML("For the purposes of this analysis, we treat the study design
                      as cross-sectional. To upload data to DANA, click on the
                      ** Browse... ** button."),
                 HTML("<div style = 'text-align:center;'>
                      <img src = 'Images/01_Upload_Data.png', style = 'max-width:80%; height:auto;'>
                      </div>"),
                 HTML("After uploading data to DANA, you will see a data preview, which
                      shows a small sample of observations from the data. This
                      expects a table of observations (i.e. excel, csv) where
                      the first row includes the variable names, and each
                      subsequent row is an observation. The variables in our example analysis are as follows:"),
                 HTML("<p> <ul>
                        <li> <b> npreg </b> - Number of pregnancies </li>
                        <li> <b> glu </b> - Plasma glucose concentration </li>
                        <li> <b> bp </b> - Diastolic blood pressure (mmHg) </li>
                        <li> <b> skin </b> - Triceps skin fold thickness (mm) </li>
                        <li> <b> bmi </b> - Body mass index </li>
                        <li> <b> ped </b> - Diabetes pedigree function </li>
                        <li> <b> age </b> - Age (years) </li>
                        <li> <b> type </b> - Diabetic (0 = no, 1 = yes) </li>
                      </ul> </p>"),
                 HTML("<p>Then, we can
                      choose a type of study design from the drop down
                      menu on the left. </p>"),
                 HTML("<div style = 'text-align:center;'>
                      <img src = 'Images/02_Choose_Study_Design.png', style = 'max-width:80%; height:auto;'>
                      </div>"),
                 HTML("<h3> <b> Select Analysis and Variables </b> </h3>"),
                 HTML("The analysis type is selected next. <b> Univariate </b> analysis
                      involves getting summary statistics for each individual variable.
                      <b> Multivariate </b> analysis analyzes the relationship
                      between the outcome and predictors"),
                 HTML("<p> Variables to be used in the analysis must be designated by their type.
                      <ul>
                        <li> <b> Categorical Variable </b> - A variable that takes
                        on distinct levels, groups, or labels, without possible
                        in-between. Examples can place of residence such
                        as a city or county.
                          <ul>
                            <li> <b> Binary Variable </b> - A type of categorical
                            variable that only takes on two values. Examples
                            can include whether or not a person has
                            a history of a given comorbidity </li>
                          </ul>
                        <li> <b> Numeric Variable </b> - A variable that
                        takes on specific numeric values. Examples can include
                        diastolic blood pressure or height. </li>
                        <li> <b> Ordinal Variable </b> - A variable that
                        can take on ordered levels that do not correspond
                        to numeric values. This can include rating physical
                        activity on a scale of 1 to 5. A value of '1' may
                        be less than a value of '5', but does not correspond
                        to a specific measured level of activity. </li> </p>"),
                 HTML("<p> Variables can be selected using the prefill menu. In
                      our example analysis, the <b> output type </b> is binary -
                      either individuals are diabetic or not, which is coded as either
                      a 0 or 1. The <b> response variable </b> is the variable referring
                      to this binary outcome - in this case it is the variable called type, referring to
                      diabetic status. Finally, the other
                      <b> covariates </b> are listed to include them in the model. After
                      selecting variables, select the 'Run Analysis' button </p>"),
                 HTML("<div style = 'text-align:center;'>
                      <img src = 'Images/03_Select_Variables.png', style = 'max-width:80%; height:auto;'>
                      </div>"),
                 HTML("<h3> <b> Analyze Results </b> </h3>"),
                 HTML("<p> Select the 'Analysis Output' to view the results of the regression
                      model. This type of model is a Generalized Linear Model (GLM) (link here),
                      which assumes that each of the covariates has a linear effect on
                      the mean of the outcome. To interpret this, we say that
                      a unit increase in one covariate results in a certain multiplicative
                      effect on the odds ratio between having the outcome and
                      not having the outcome. For instance, <b> each additional
                      pregnancy is associated with multiplying the odds ratio of diabetes
                      by 1.11 </b>. The other effects are interpreted in a similar manner.
                      The CI_Lower and CI_Upper are the bounds
                      of a 95% confidence interval for each covariate. In this case,
                      confidence intervals that include a value
                      of 1 represent no statistically significant effect. Thus,
                      we can say that there is a statistically significant positive
                      association between blood pressure and diabetes </p>"),
                 HTML("<div style = 'text-align:center;'>
                      <img src = 'Images/04_Regression_Results.png', style = 'max-width:80%; height:auto;'>
                      </div>"),
                 HTML("<h3> <b> Caution </b> </h3>"),
                 HTML("<p> There are some important considerations in regression settings. </p>
                      <ul>
                        <li> Covariates are assumed to have linear effects on the outcome
                        or odds ratio. Other types of effects require more complicated models. </li>
                        <li> Correlations between observations are unaccounted for. Correlated
                        data requires other methods such as mixed effects, which take into account
                        how observations are related such as though
                        repeated measurements on the same individual. </li>
                        <li> Covariates are assumed to have independent effects. Therefore,
                        variables that are related will have inflated variance (larger
                        confidence intervals). Consider alternative approaches such
                        as adding interaction effects or removing variables that
                        are measuring the same thing on different scales. </li>
                      </ul>
                      <p> These assumptions should be assessed before reporting results. Consider
                      consulting a trained statistician or biostatistician to ensure valid
                      statistical conclusions. </p>"),










             )
           )
  ),
  tabPanel("Model Cascade",
           page_fluid(
             titlePanel("Model Cascade Information"),
             uiFlowchart("flowchart"),
             div(class = "content",
                 HTML("Prospective Cohort (i.e. mixed models)
                      <ul>
                      <li> Continuous Outcome </li>
                        <ul>
                        <li> Fit Bayesian Gaussian GLMER </li>
                          <ul>
                            <li> If Bayesian p-value for deviance is OK, use Bayesian Gaussian GLMER </li>
                            <li> If Bayesian p-value for deviance is high or low and y > 0, fit nonparametric Bayesian Gaussian GLM with log transform
                            and fit nonparametric Bayesian Gaussian GLM without transforming </li>
                              <ul>
                                <li> Compare expected absolute loss relative to median posterior predictions, and use the model that has lowest value
                              </ul>
                            <li> If Bayesian p-value for deviance is high or low and y < 0, fit nonparametric Bayesian Gaussian GLM </li>
                          </ul>
                        </ul>
                      <li> Binary Outcome </li>
                        <ul>
                          <li> Fit Bayesian Binomial GLMER </li>
                            <ul>
                          <li> If Bayesian p-value for deviance is high or low, use nonparametric Bayesian GLM with binary outcome </li>
                          <li> If Bayesian p-value for deviance is OK, use Bayesian GLMER with binary outcome </li>
                          </ul>
                        </ul>
                      <li> Count Outcome  </li>
                        <ul>
                       <li> Fit Bayesian Poisson GLM </li>
                        <ul>
                          <li> If Bayesian p-value for deviance is OK, use Bayesian poisson GLMER </li>
                          <li> If Bayesian p-value for deviance is high or low, fit Bayesian negative binomial GLMER </li>
                          <ul>
                            <li> If Bayesian p-value for deviance is OK, use Bayesian GLMER with negative binomial outcome </li>
                            <li> If Bayesian p-value for deviance is high or low, fit nonparametric poisson GLM </li>
                          </ul>
                          </ul>
                        </ul>
                      </ul>",
                      )
           ))),

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

