#' Detect Variable Types
#'
#' Basic logic for detecting (guessing) variable types based on data input
#'
#' @param data tibble input
#' @details
#' Returns character vectors corresponding to the column
#' names of each variable type. Uses basic logic to detect reasonable
#' types
#'
#' @export

detect_var_type <- function(data){

  # ----------------------------------------------------------------------------

  if (tibble::is_tibble(data) == FALSE){
    errorCondition("Object is not a tibble")
  }

  # ----------------------------------------------------------------------------

  unique_vals <- apply(data, 2, function(x) length(unique(x)))

  ## Define nominal by those with character entries
  categorical <- dplyr::select(data, dplyr::where(is.character)) |> names()

  ## Define continuous by having 5 or greater unique values and numeric entries
  continuous <- intersect(names(which(apply(data, 2, function(x) length(unique(x))) > 5)), data |>
                            dplyr::select(dplyr::where(is.numeric)) |> names())

  ## Define discrete by having less than 5 unique values
  discrete <- intersect(names(which(apply(data, 2, function(x) length(unique(x))) < 5)), data |>
                            dplyr::select(dplyr::where(is.numeric)) |> names())

  return(
    list(continuous = continuous,
        categorical = c(categorical, discrete),
        all = c(continuous, categorical, discrete))
  )

}
