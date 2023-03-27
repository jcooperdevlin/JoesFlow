# test Heatmap functionality

#########
# Setup #
#########

library(JoesFlow)
library(shiny)

# check which data sets we can test
extdata_dir  <- system.file( 'extdata', package = 'JoesFlow')
testData_dir <- system.file('testData', package = 'JoesFlow')

test_data <- tibble(lab = 'test',
                    flow = paste0(extdata_dir, '/flow.csv'),
                    meta = paste0(extdata_dir, '/metadata.csv'))

if(testData_dir != '')
{
  test_data <- tibble(lab = list.files(testData_dir),
                      flow = paste0(testData_dir, '/', lab, '/flow.csv'),
                      meta = paste0(testData_dir, '/', lab, '/metadata.csv')) %>%
    bind_rows(test_data)
}


#########
# Tests #
#########

test_that('Heatmap tests', {
  testServer(shinyApp(ui = app_ui(),
                      server = app_server),
  {
    # set up inputs
    session$setInputs(nav_bar                    = "Visualize",
                      main_output                = 'Markers',
                      file1                      = NULL,
                      file2                      = NULL,
                      subsample                  = 0.2,
                      seed                       = 247893,
                      meta_val                   = "ID",
                      clust_type                 = "Kmeans",
                      kmean                      = 5,
                      feat_dim                   = "PCA",
                      colpal                     = "Default",
                      show_hide_dimreduct_legend = "Show",
                      show_hide_cluster_legend   = "Show",
                      plot1_brush                = NULL,
                      download_width             = 15,
                      download_height            = 10)

    for(i in 1:nrow(test_data))
    {
      # set input files (test_data_paths is a `reactiveValues` object in the app)
      test_data_paths$flow <- test_data$flow[i]
      test_data_paths$meta <- test_data$meta[i]


      ### unit tests to run on all data sets ###

      # check heatmap plot
      expect_s3_class(vals$marker_heat, 'gTree')
    }
  })
})