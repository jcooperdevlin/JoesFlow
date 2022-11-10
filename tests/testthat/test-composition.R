# test Composition plot functionality

#########
# Setup #
#########

library(JoesFlow)
library(stringr)

# colors used in figures
library(ggsci)
library(RColorBrewer)

colors_clusters <- c(pal_d3("category10")(10), pal_d3("category20b")(20), pal_igv("default")(51))
colors_samples <- c(brewer.pal(5, "Set1"), brewer.pal(8, "Dark2"), pal_igv("default")(51))

# check which data sets we can test
setup_testing_data()
test_data <- data(package = 'JoesFlow')$results[,'Item']

# `data()` doesn't seem to give me the same results for `devtools::test()` and `devtools::check()`
# Catch that here
if(any(grepl('sample_data', test_data)))
{
  test_data <- test_data %>%        # format is `c("sample_data ({dataset_name})", "meta_data ({dataset_name})")` for all {dataset_name} in data/.
    str_split(fixed('(')) %>%       # strip out "sample_data (" and "meta_data ("
    sapply(`[`, 2) %>%
    str_replace(fixed(')'), '') %>% # remove trailing ")"
    unique()                        # keep unique
}


#########
# Tests #
#########

test_that('Composition plot tests', {

  ### unit tests to run on all data sets ###
  comp_tests <- quote({
    # from Composition section of `app_server.R`
    set.seed(23948)
    kmeans_groups <- tibble(ids = sample_data[,1],
                            grp = sample_data[,-1] %>%
                              kmeans(10) %$% cluster %>%
                              {paste0('C', .)}) # add a 'C' on the front of each group

    comp <- compositionJF(meta = meta_data,
                          grp = names(meta_data)[2],
                          kmeans_groups = kmeans_groups,
                          colors = colors_clusters)

    expect_s3_class(comp$plotter, 'tbl')
    expect_s3_class(comp$g1, "ggplot")
  })

  print(paste("Testing the following data sets:", paste(test_data, collapse = ', ')))

  ########## run tests ##########
  for(i in 1:length(test_data))
  {
    eval(parse(text = paste0('data(', test_data[i], ')')))

    eval(comp_tests)
  }
})