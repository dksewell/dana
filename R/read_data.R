#' Read Data from File
#'
#' Reads data from a chosen fileInput
#'
#' @param path Path pointing to file to read
#' @param start_row Number indicating row with variable names
#' @param end_row Number indicating row with final observation
#' @returns Dataframe
#' @details
#' Designed to read 3 different file formats: csv, xlsx, and rds.
#' These are specifically chosen to be most common formats in public
#' health research.
#'
#' This function has two behaviors:
#'
#' - If start_row and end_row are NA, then use defaults to read in
#' data.
#' - If start_row and end_row are specified, then treats the start
#' row as row labels and reads to the specified data rows.
#'
#'
#'
#'
#' @export


read_data <- function(file, start_row, end_row){

  path = file$datapath
  ext <- tools::file_ext(path) # Denote extension type

  # Read data defaults if start_row and end_row are NA
  if (is.na(end_row)){
    if (ext == "csv") {
      rslt <- readr::read_csv(path)
    } else if (ext == "xlsx") {
      rslt <- readxl::read_excel(path)
    } else if (ext == "rds") {
      rslt <- readRDS(path)
    } else {
      warning("Unsupported file format")
    }
  }

  # Read data specified by start and end rows
  if (!is.na(end_row)) {
    if (ext == "csv") {
      rslt <- readr::read_csv(path,
               skip = start_row - 1,
               n_max = end_row - start_row)
    } else if (ext == "xlsx") {
      rslt <- readxl::read_excel(path,
                         skip = start_row - 1,
                         n_max = end_row - start_row)
    } else if (ext == "rds") {
      rslt <- readRDS(path,
              skip = start_row - 1,
              n_max = end_row - start_row)
    } else {
      warning("Unsupported file format")
    }
  }

  return(rslt)

}



