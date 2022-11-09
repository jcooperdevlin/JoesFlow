# Testing utilities


#' save_flowdata
#' Save flow data
#'
#' This saves flow data and metadata to a .RData file for use in testing scripts
#' @param flow Path to flow data
#' @param meta Path to metadata
#' @param RData Path to output file
#' @param compress compression algorithm to use. Alternate compression algorithms may be recommended by `devtools::check()`
#'
#' @return Returns `TRUE` when successful.
#' @export
save_flowdata <- function(flow, meta, RData, compress = 'gzip')
{
  sample_data <- utils::read.csv(flow)
  meta_data <- utils::read.csv(meta)

  save(sample_data, meta_data, file = RData, compress = compress)

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
  # paths
  datadir <- system.file("data", package = 'JoesFlow')
  extdata <- system.file("extdata", package = 'JoesFlow')
  testdat <- system.file("testData", package = 'JoesFlow')

  # check / set up internal package data
  if(!file.exists(paste0(datadir, '/test_data.RData')))
    save_flowdata(paste0(extdata, '/flow_test.csv'),
                  paste0(extdata, '/metadata.csv'),
                  paste0(datadir, '/test_data.RData'),
                  compress = 'bzip2')

  # check / set up external testing data
  if(testdat != '')
  {
    # Issue 4: https://github.com/niaid/JoesFlow/issues/4
    if(!file.exists(paste0(datadir, '/issue4.RData')))
      save_flowdata(paste0(testdat, '/2022_11_08/Flourscent Intensity.csv'),
                    paste0(testdat, '/2022_11_08/Metadata.csv'),
                    paste0(datadir, '/issue4.RData'),
                    compress = 'xz')
  }
}
