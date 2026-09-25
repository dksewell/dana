#' moduleUploadInfo Server Logic
#'
#' Create server logic for generating list of information and warnings associated
#' with data upload
#'
#' @param id Shiny input/output id
#' @param data  Dataframe
#' @param cat_vars Categorical variables
#' @param cont_vars Continuous variables
#' @param ci_level Credible interval level
#'
#' @details
#' Paired with uiUploadInfo through the id argument.
#' This function is placed in an R shiny
#' server function with a user-specified parameters.
#'
#' Generates a list of warnings and information about the data upload.
#' This includes:
#'
#' * Number of observations
#' * Number of variables
#'
#' * Insufficient included observations
#' * Variables specified as both categorical and continuous
#' * Invalid CI levels
#'
#' @seealso
#'  \code{\link{uiUploadInfo}}
#'
#' @export

serverUploadInfo <- function(id, data, cat_vars, cont_vars, ci_level){
  shiny::moduleServer(
    id,
    function(input, output, session){

      req(data)

      n_obs <- nrow(data)
      n_col <- ncol(data)
      rslt <- list()

      # Data warnings
      if (n_obs < 30){
        rslt$negative <- c(rslt$negative, paste0("The specified data contains a
                                                 relatively small amount of
                                                 observations: ",
                                                 n_obs,
                                                 ". Ensure data are
                                                 specified correctly."))
      }

      if (anyDuplicated(c(cat_vars, cont_vars))){
        rslt$negative <- c(rslt$negative, paste0("One or more variables is
                                                  specified
                                                  as both a continuous
                                                  and a categorical variable"))
      }

      if (ci_level <= 0 || 1 <= ci_level){
        rslt$negative <- c(rslt$negative, paste0("CI levels must be between
                                                 0 and 1 (exclusive)"))
      }

      # Data info
      rslt$info <- c(rslt$info, paste0("Data upload contains ", n_obs, " observations"))
      rslt$info <- c(rslt$info, paste0("Data upload contains ", n_col, " variables: ",
                                       colnames(data)[1], " ... ", colnames(data)[n_col]))

      # Render as UI message
      output$message <- shiny::renderUI({
        tagList(
          lapply(rslt$negative, FUN = function(x){
            shiny::tags$p(shiny::tags$span(tags$b("Warning:", style = "color: red")), x)
          }),
          lapply(rslt$info, FUN = function(x){
            shiny::tags$li(shiny::tags$p(x))
          })
        )
      })
    }
)}

#' moduleUploadInfo UI Logic
#'
#' Create UI logic for generating list of information and warnings associated
#' with data upload
#'
#' @param id Shiny input/output id
#' @details
#' Paired with serverUploadInfo through the id argument.
#'
#'  \code{\link{serverUploadInfo}}
#'
#' @export

uiUploadInfo <- function(id, label = "uploadinfo"){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3(tags$b("Upload Info")),
    shiny::hr(),
    shiny::uiOutput(ns("message")),
    shiny::hr()
  )
}
