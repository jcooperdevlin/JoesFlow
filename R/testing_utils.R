# Testing utilities


#' save_flowdata
#' Save flow data
#'
#' This saves flow data and metadata to a .RData file for use in testing scripts
#' @param flow Path to flow data
#' @param meta Path to metadata
#' @param RData Path to output file
#'
#' @return Returns `TRUE` when successful.
#' @export
save_flowdata <- function(flow, meta, RData)
{
  sample_data <- utils::read.csv(flow)
  meta_data <- utils::read.csv(meta)

  save(sample_data, meta_data, file = RData)

  invisible(TRUE)
}


#' setup_testing_data
#' Set up testing data and return a list of files to test with
#'
#' This sets up testing data and returns a list of files to test with
#' @return Returns a character vector containing paths to files that need to be tested
#' @details To add other data not included as part of the repository, create a directory at root/testing/testData/ with the desired files. If that directory is missing, testing will only be performed on internal data (i.e. data included with the package).
#' @export
setup_testing_data <- function()
{
  # package repository root directory
  root <- system('git rev-parse --show-toplevel', intern = TRUE) %>%
    paste0('/')

  # check / set up internal package data
  test_data <- list('internal' = paste0(root, 'tests/test_data.RData'))
  if(!file.exists(test_data$internal))
    save_flowdata(paste0(root, 'tests/flow_test.csv'),
                  paste0(root, 'tests/metadata.csv'),
                  test_data$internal)

  # check / set up external testing data
  if(file.exists(paste0(root, 'tests/testData')))
  {
    # Issue 4: https://github.com/niaid/JoesFlow/issues/4
    test_data$issue4 <- paste0(root, 'tests/testData/2022_11_08/issue4.RData')
    if(!file.exists(test_data$issue4))
      save_flowdata(paste0(root, 'tests/testData/2022_11_08/Flourscent Intensity.csv'),
                    paste0(root, 'tests/testData/2022_11_08/Metadata.csv'),
                    test_data$issue4)
  }

  return(test_data)
}