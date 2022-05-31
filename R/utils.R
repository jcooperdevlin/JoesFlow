
#' Plot a cluster for JoesFlow
#' 
#' @param clustered_data Object containing clustered data (expects output from `prcomp` or `uamp`)
#' @param ids Character vector of ids for each row in `clustered_data$x`, corresponding to SampleID in `meta`
#' @param meta Data frame containing metadata, with SampleID in the first column and Group label in the second column
#' @param colors Vector of colors for each group
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param legend.name Character string for the legend name (default is 'Group')
#' @param ... Other objects passed along to functions within clusterJF
#' 
#' @return A ggplot object
#' @export
#' @import dplyr
#' @import ggplot2
#' @importFrom rlang .data 
clusterJF <- function(clustered_data, ...) {
  UseMethod('clusterJF')
}

# method for principal components object
#' @rdname clusterJF
#' @method clusterJF prcomp
#' @export
clusterJF.prcomp <- function(clustered_data, ids, meta, colors, legend.name = 'Group', ...) {

  # format data for figure
  plotter <- tibble(X1       = clustered_data$x[,'PC1'], 
                    X2       = clustered_data$x[,'PC2'], 
                    SampleID = ids) %>%
    
    # get group labels
    group_by(.data$SampleID) %>%
    mutate(Group = meta[unique(.data$SampleID)]) %>%
    ungroup()

  # proportion of variance
  PoV <- with(clustered_data, sdev^2 / sum(sdev^2))
  
  # render the figure
  clusterJF(plotter, colors,
            xlab = paste0("PC1 (Explained Variance ", round(PoV[1],4)*100, "%)"),
            ylab = paste0("PC2 (Explained Variance ", round(PoV[2],4)*100, "%)"),
            legend.name, ...)
}

# method for matrix (probably coming from `umap`)
#' @rdname clusterJF
#' @method clusterJF matrix
#' @export
clusterJF.matrix <- function(clustered_data, ids, meta, colors, legend.name = 'Group', ...){
  
  # format data for figure
  plotter <- tibble(X1       = clustered_data[,1],
                    X2       = clustered_data[,2],
                    SampleID = ids) %>%
    
    # get group labels
    group_by(.data$SampleID) %>%
    mutate(Group = meta[unique(.data$SampleID)]) %>%
    ungroup()
  
  # render figure
  clusterJF(plotter, colors, xlab = "UMAP_1", ylab = "UMAP_2", legend.name, ...)
}

# method for tibble object (should normally be called from clusterJF.prcomp or clusterJF.matrix)
#' @rdname clusterJF
#' @method clusterJF tbl
#' @export
clusterJF.tbl <- function(clustered_data, colors, xlab, ylab, legend.name, ...){
  
  # render figure    
  ggplot(clustered_data, aes(.data$X1, .data$X2, color=.data$Group)) +
    geom_point() + theme_bw() +
    scale_color_manual(values=colors, name = legend.name) +
    xlab(xlab) +
    ylab(ylab) +
    theme(axis.text=element_text(color='black', size=14),
          axis.title=element_text(color='black', size=16))
}
    


#' Sample-based PCA for JoesFlow
#' 
#' @param clustered_data Object containing clustered data (expects output from `prcomp` or `umap`)
#' @param ids Character vector of ids for each row in `clustered_data$x`, corresponding to SampleID in `meta`
#' @param meta Data frame containing metadata, with SampleID in the first column and Group label in the second column
#' @param colors1 Vector of colors for samples
#' @param colors2 Vector of colors for clusters
#' 
#' @return A ggplot object
#' @export
#' @import dplyr
#' @import ggplot2
#' @importFrom ggrepel geom_label_repel
#' @importFrom rlang .data 
sb_clusterJF <- function(clustered_data, ids, meta, colors1, colors2) {
  
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
  
  # render figures
  pp1 <- ggplot(plotter, aes(.data$PC1, .data$PC2, color=.data$Group, label=.data$SampleID)) +
    geom_point() + theme_bw() +
    geom_label_repel(size = 6) +
    scale_color_manual(values=colors1) +
    guides(color = guide_legend(override.aes = list(label = 'O', size = 3))) +
    xlab(paste0("PC1 (Explained Variance ", round(PoV[1],4)*100, "%)")) +
    ylab(paste0("PC2 (Explained Variance ", round(PoV[2],4)*100, "%)")) +
    theme(axis.text=element_text(color='black', size=14),
          axis.title=element_text(color='black', size=16))
  
  # format data for figure
  plotter <- tibble(PC1   = clustered_data$rotation[,'PC1'], 
                    PC2   = clustered_data$rotation[,'PC2'], 
                    Label = rownames(clustered_data$rotation))
  
  # proportion of variance
  PoV <- with(clustered_data, sdev^2 / sum(sdev^2))
  
  # render figure    
  pp2 <- ggplot(plotter, aes(.data$PC1, .data$PC2, color=.data$Label, label=.data$Label)) +
    geom_point() + theme_bw() +
    geom_label_repel(size = 6) +
    scale_color_manual(values=colors2) +
    guides(color = guide_legend(override.aes = list(label = 'O', size = 3))) +
    theme(axis.text=element_text(color='black', size=14),
          axis.title=element_text(color='black', size=16))
  
  pp1 + pp2
}
