# extract_values.R
# Functions for extracting values from PCA, UMAP, TSNE, ... for plotting and download

#' extract_values


#' extract_sb_values
#' Extract values from sample based PCA results
#'
#' @param clustered_data Object containing clustered data (expects output from `prcomp`)
#' @param ids Character vector of ids for each row in `clustered_data$x`, corresponding to labels in `grps`
#' @param meta Data frame containing translation from id to group
#' @param grp Character value identifying the column of `meta` to use for group identifier
#' @value a data frame with values for PC1, PC2, SampleID, and Group
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

    select(SampleID, Group, PC1, PC2)
}


#' extract_sb_loadings
