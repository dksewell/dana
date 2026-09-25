#' Apply Reference Levels to a Dataframe
#'
#' Automatically converts specified columns within a dataframe to factors with specified reference levels
#'
#' @param data A data frame with containing the variables specified in the formula
#' @param cat_vars A string vector referring to the outcome variable
#' @param ref_levels A string vector referring to the reference levels
#'
#' @returns A dataframe with applied reference levels
#' @details Placeholder
#'
#'
#' @export
#'
#'


apply_refs <- function(data, cat_vars = NULL, ref_levels = NULL){

  # Early return if nothing is specified
  if (is.null(cat_vars) && is.null(ref_levels)){

    return(dplyr::tibble(data))

  }

  # Ensure both variables and references match
  if (length(cat_vars) != length(ref_levels)){

    stop(paste0("Unmatched reference levels: ", length(cat_vars), " and ", length(ref_levels)))

  }

  # Apply over dataframe
  ref_list <- lapply(setNames(ref_levels, cat_vars), function(x) x)

  data <- data |>
    dplyr::mutate(dplyr::across(dplyr::all_of(cat_vars), ~ {
      relevel(factor(as.character(.x)), ref = ref_list[[dplyr::cur_column()]])
    })) |>
    dplyr::tibble()

  return(data)

}
