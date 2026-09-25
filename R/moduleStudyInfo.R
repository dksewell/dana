#' Study Information Server Logic
#'
#' Create server logic for study design information
#'
#' @param type A type of study
#' @param id shiny input/output id

#' @details
#'  Study information is the same as those supplied in the dendrogram
#'
#' @export
#'



serverStudyInfo <- function(id, type){
  shiny::moduleServer(
    id,
    function(input, output, session){
    switch(type,
      "Cross-sectional (Regression)" = {
        output$study_info <- shiny::renderUI({
          shiny::tagList(
          shiny::HTML("Study Design: Cross-sectional Observational Study. Learn more <a href='https://www.sciencedirect.com/science/article/pii/S0012369220304621'>here.</a>"),
          shiny::h4("Example of Cross-sectional Data:"),
          shiny::p("Researchers took well water samples from randomly selected wells across Iowa
                    and tested for several enteric pathogens using PCR.
                    The columns of the data consist of an anonymized well ID and the PCR Ct values for each of the pathogens tested."),
          shiny::p("This is a snapshot of data collected at a single time point.")
          )
        })
      },
      "Cross-sectional (No Regression)" = {
        output$study_info <- shiny::renderUI({
          shiny::tagList(
            shiny::HTML("Study Design: Cross-sectional Observational Study. Learn more <a href='https://www.sciencedirect.com/science/article/pii/S0012369220304621'>here.</a>"),
            shiny::h4("Example of Cross-sectional Data:"),
            shiny::p("Researchers took well water samples from randomly selected wells across Iowa
                    and tested for several enteric pathogens using PCR.
                    The columns of the data consist of an anonymized well ID and the PCR Ct values for each of the pathogens tested."),
            shiny::p("This is a snapshot of data collected at a single time point.")
          )
        })
      },

      "Retrospective Cohort (Regression)" = {
        output$study_info <- shiny::renderUI({
          shiny::tagList(
          shiny::HTML("Study Design: Retrospective Cohort Observational Study. Learn more <a href='https://www.sciencedirect.com/topics/medicine-and-dentistry/retrospective-cohort-study'>here.</a>"),
          h4("Example of Retrospective Cohort Data:"),
          p("Researchers analyzed medical records from patients who visited a hospital in Iowa between 2010 and 2020. They aimed to describe the prevalence of diabetes and related conditions such as hypertension, cardiovascular disease, and kidney disease. The columns of the data consist of anonymized patient ID, year of visit, age, gender, ethnicity, diabetes diagnoses, hypertension diagnoses, cardiovascular disease  diagnoses, and kidney disease diagnoses."),
          p("This data traces back from the outcome to previous exposures.")
          )
      })},
        output$study_info <- shiny::renderUI({
          shiny::p("Study Design not yet implemented.")
        })
    )
    }
  )
}

#' Study Information UI Logic
#'
#' Create UI logic study information
#'
#' @param id shiny input/output id
#' @details
#'  Study information is the same as those supplied in the dendrogram
#'
#' @export

uiStudyInfo <- function(id, label = "studyinfo"){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::htmlOutput(ns("study_info")),
  )
}
