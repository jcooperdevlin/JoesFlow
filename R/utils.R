
#' Plot a cluster for JoesFlow
#' 
#' @param clustered_data Object containing clustered data (expects output from `prcomp` or `uamp`)
#' @param ids Character vector of ids for each row in `clustered_data$x`, corresponding to SampleID in `meta`
#' @param meta Data frame containing metadata, with SampleID in the first column and Group label in the second column
#' @param colors Vector of colors for each group
#' @param legend.name Character string for the legend name (default is 'Group')
#' 
#' @return A ggplot object
#' @export
#' @import dplyr
#' @import ggplot2
#' @importFrom rlang .data 
clusterJF <- function(clustered_data, ids, meta, colors, legend.name = 'Group') {

  # format data for figure
  plotter <- tibble(PC1      = clustered_data$x[,'PC1'], 
                    PC2      = clustered_data$x[,'PC2'], 
                    SampleID = ids) %>%
    
    # get group labels
    group_by(.data$SampleID) %>%
    mutate(Group = meta[unique(.data$SampleID)]) %>%
    ungroup()

  # proportion of variance
  PoV <- with(clustered_data, sdev^2 / sum(sdev^2))

  # render figure    
  ggplot(plotter, aes(.data$PC1, .data$PC2, color=.data$Group)) +
    geom_point() + theme_bw() +
    scale_color_manual(values=colors, name = legend.name) +
    xlab(paste0("PC1 (Explained Variance ", round(PoV[1],4)*100, "%)")) +
    ylab(paste0("PC2 (Explained Variance ", round(PoV[2],4)*100, "%)")) +
    theme(axis.text=element_text(color='black', size=14),
          axis.title=element_text(color='black', size=16))
}
    

#' Plot a sample based cluster for JoesFlow
#' 
#' @param clustered_data Object containing clustered data (expects output from `prcomp` or `uamp`)
#' @param ids Character vector of ids for each row in `clustered_data$x`, corresponding to SampleID in `meta`
#' @param meta Data frame containing metadata, with SampleID in the first column and Group label in the second column
#' @param colors Vector of colors for each group
#' 
#' @return A ggplot object
#' @export
#' @import dplyr
#' @import ggplot2
#' @importFrom ggrepel geom_label_repel
#' @importFrom rlang .data 
sb_clusterJF <- function(clustered_data, ids, meta, colors) {
  
  # format data for figure
  plotter <- tibble(PC1      = clustered_data$x[,'PC1'], 
                    PC2      = clustered_data$x[,'PC2'], 
                    SampleID = ids) %>%
    
    # get group labels
    group_by(.data$SampleID) %>%
    mutate(Group = meta[unique(as.numeric(.data$SampleID))]) %>%
    ungroup()
  
  # proportion of variance
  PoV <- with(clustered_data, sdev^2 / sum(sdev^2))
  
  # render figure    
  ggplot(plotter, aes(.data$PC1, .data$PC2, color=.data$Group, label=.data$SampleID)) +
    geom_point() + theme_bw() +
    geom_label_repel(size = 6) +
    scale_color_manual(values=colors) +
    guides(color = guide_legend(override.aes = list(label = 'O', size = 3))) +
    xlab(paste0("PC1 (Explained Variance ", round(PoV[1],4)*100, "%)")) +
    ylab(paste0("PC2 (Explained Variance ", round(PoV[2],4)*100, "%)")) +
    theme(axis.text=element_text(color='black', size=14),
          axis.title=element_text(color='black', size=16))
}


#' Plot loadings for a sample based cluster for JoesFlow
#' 
#' @param clustered_data Object containing clustered data (expects output from `prcomp` or `uamp`)
#' @param colors Vector of colors for each group
#' 
#' @return A ggplot object
#' @export
#' @import dplyr
#' @import ggplot2
#' @importFrom ggrepel geom_label_repel
#' @importFrom rlang .data 
sb_loadingsJF <- function(clustered_data, colors) {
  
  # format data for figure
  plotter <- tibble(PC1   = clustered_data$rotation[,'PC1'], 
                    PC2   = clustered_data$rotation[,'PC2'], 
                    Label = rownames(clustered_data$rotation))
  
  # proportion of variance
  PoV <- with(clustered_data, sdev^2 / sum(sdev^2))
  
  # render figure    
  ggplot(plotter, aes(.data$PC1, .data$PC2, color=.data$Label, label=.data$Label)) +
    geom_point() + theme_bw() +
    geom_label_repel(size = 6) +
    scale_color_manual(values=colors) +
    guides(color = guide_legend(override.aes = list(label = 'O', size = 3))) +
    theme(axis.text=element_text(color='black', size=14),
          axis.title=element_text(color='black', size=16))
}
