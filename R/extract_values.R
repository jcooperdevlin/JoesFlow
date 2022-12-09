# extract_values.R
# Functions for extracting values from PCA, UMAP, TSNE, ... for plotting and download

#' extract_values


#' extract_sb_values
#' Extract values from sample based PCA
#'
#' @param clustered_data Object containing clustered data (expects output from `prcomp`)
#' @param ids Character vector of ids for each row in `clustered_data$x`, corresponding to labels in `grps`
#' @param meta Data frame containing translation from id to group
#' @param grp Character value identifying the column of `meta` to use for group identifier
#' @value a data frame with values for SampleID, and Group, PC1, and PC2
#' @export
extract_sb_values <- function(clustered_data, ids, meta, grp)
{
  # pull principal components from sb_pca()
  tibble(SampleID = ids,
         PC1      = clustered_data$x[,'PC1'],
         PC2      = clustered_data$x[,'PC2']) %>%

    # add grouping information
    group_by(.data$SampleID) %>%
    mutate(Group = meta[,grp][meta[,1] == unique(.data$SampleID)] %>%
             as.character()) %>%
    ungroup() %>%

    dplyr::select(SampleID, Group, PC1, PC2)
}


#' extract_sb_loadings
#' Extract loadings from sample based PCA
#'
#' @param clustered_data Object containing clustered data (expects output from `prcomp`)
#' @value a data frame with loadings for PC1 and PC2 for each Cluster
#' @export
extract_sb_loadings <- function(clustered_data)
{
  # pull loadings from clustered_data
  tibble(Cluster = rownames(clustered_data$rotation),
         PC1     = clustered_data$rotation[,'PC1'],
         PC2     = clustered_data$rotation[,'PC2'])
}
