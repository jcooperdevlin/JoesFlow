# test PCA functionality

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

test_that('PCA tests', {

  ### unit tests to run on all data sets ###
  pca_tests <- quote({
    # from PCA Analysis section of `app_server.R` - make sure we can use any meta_data column
    pp <- sample_data[,-1] %>%            # strip ID column
      stats::prcomp(scale=T)              # run PCA

    pp1 <- clusterJF(pp,                  # render PCA plot
                     ids = sample_data[,1],
                     meta = meta_data,
                     grp = names(meta_data)[1],
                     colors = colors_samples)

    expect_s3_class(pp1, "ggplot")

    pp1 <- clusterJF(pp,                  # render PCA plot
                     ids = sample_data[,1],
                     meta = meta_data,
                     grp = names(meta_data)[2],
                     colors = colors_samples)

    expect_s3_class(pp1, "ggplot")

    # from Kmeans and PCA Analysis sections of `app_server.R` - make sure k-means figures work
    set.seed(23948)
    kmeans_groups <- tibble(ids = sample_data[,1],
                            grp = sample_data[,-1] %>%
                              kmeans(10) %$% cluster %>%
                              {paste0('C', .)}) # add a 'C' on the front of each group

    pp2 <- clusterJF(pp,
                     ids = sample_data[,1],
                     meta = kmeans_groups,
                     grp = 'grp',
                     colors = colors_clusters,
                     legend.name = "Cluster")

    expect_s3_class(pp2, "ggplot")

    # from sample-based PCA section of `app_server.R` - make sure sample-based PCA works
    groups_table <- table(kmeans_groups)

    pp <- apply(groups_table, 2, function(x) x / rowSums(groups_table)) %>%
      stats::prcomp()

    pp3 <- sb_clusterJF(pp,
                        ids = rownames(groups_table),
                        meta = meta_data,
                        grp = names(meta_data)[1],
                        colors1 = colors_samples,
                        colors2 = colors_clusters,
                        legend.name = names(meta_data)[1])

    expect_s3_class(pp3, "ggplot")
  })

  print(paste("Testing the following data sets:", paste(test_data, collapse = ', ')))

  ########## run tests ##########
  for(i in 1:length(test_data))
  {
    eval(parse(text = paste0('data(', test_data[i], ')')))

    eval(pca_tests)
  }
})