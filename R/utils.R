
#' Plot a cluster for JoesFlow
#'
#' @param clustered_data Object containing clustered data (expects output from `prcomp` or `uamp`)
#' @param ids Character vector of ids for each row in `clustered_data$x`, corresponding to labels in `meta`
#' @param meta Data frame containing metadata - first column corresponds to ids
#' @param grp Column of meta to use for grouping of data
#' @param colors Vector of colors for each group
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param axis_prefix x-and y-axis prefix (e.g. 'UMAP_')
#' @param legend.name Character string for the legend name (default is 'Group')
#' @param show_legend Logical value used to turn off the legend
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
clusterJF.prcomp <- function(clustered_data, ids, meta, grp, colors, legend.name = 'Group', show.legend = TRUE, ...) {

  # proportion of variance
  PoV <- with(clustered_data, sdev^2 / sum(sdev^2))

  # format data for figure
  extract_values(clustered_data, ids, meta, grp) %>%

    clusterJF(meta = meta, grp = grp, colors = colors, legend.name = legend.name,
              xlab = paste0("PC1 (Explained Variance ", round(PoV[1],4)*100, "%)"),
              ylab = paste0("PC2 (Explained Variance ", round(PoV[2],4)*100, "%)"),
              show.legend, ...)
}

# method for matrix (probably coming from `umap`)
#' @rdname clusterJF
#' @method clusterJF matrix
#' @export
clusterJF.matrix <- function(clustered_data, axis_prefix = 'axis', ids, meta, grp, colors, legend.name = 'Group', show.legend = TRUE, ...){

  # format data for figure
  extract_values(clustered_data, ids, meta, grp) %>%

    clusterJF(meta = meta, grp = grp, colors = colors, legend.name = legend.name,
              xlab = paste(axis_prefix, 1, sep = '_'),
              ylab = paste(axis_prefix, 2, sep = '_'),
              show.legend, ...)
}

# method for tibble object (should normally be called from clusterJF.prcomp or clusterJF.matrix)
#' @rdname clusterJF
#' @method clusterJF tbl
#' @export
clusterJF.tbl <- function(clustered_data, meta, grp, colors, xlab, ylab, legend.name, show.legend = TRUE, ...){

  # render figure
  retval <- ggplot(clustered_data, aes(.data$X1, .data$X2, color=.data$Group)) +
    geom_point() + theme_bw() +
    scale_color_manual(values=colors, name = legend.name) +
    xlab(xlab) +
    ylab(ylab) +
    theme(axis.text=element_text(color='black', size=14),
          axis.title=element_text(color='black', size=16))

  if(!show.legend)
    retval <- retval + guides(color = FALSE)

  retval
}



#' Sample-based PCA for JoesFlow
#'
#' @param clustered_data Object containing clustered data (expects output from `prcomp` or `umap`)
#' @param ids Character vector of ids for each row in `clustered_data$x`, corresponding to labels in `meta`
#' @param meta Data frame containing metadata - first column corresponds to ids
#' @param grp Column of meta to use for grouping of data
#' @param colors1 Vector of colors for samples
#' @param colors2 Vector of colors for clusters
#' @param legend.name Character string for the legend name (default is 'Group')
#' @param show_grp_legend Logical value used to turn off the legend for groups
#' @param show_clust_legend Logical value used to turn off the legend for clusters
#'
#' @return A ggplot object
#' @export
#' @import dplyr
#' @import ggplot2
#' @importFrom ggrepel geom_label_repel
#' @importFrom rlang .data
sb_clusterJF <- function(clustered_data, ids, meta, grp, colors1, colors2, legend.name = 'Group', show_grp_legend = TRUE, show_clust_legend = TRUE) {

  # extract data for figure
  plotter <- extract_sb_values(clustered_data, ids, meta, grp)

  # proportion of variance
  PoV <- with(clustered_data, sdev^2 / sum(sdev^2))

  # render figures
  pp1 <- ggplot(plotter, aes(.data$PC1, .data$PC2, color=.data$Group, label=.data$SampleID)) +
    geom_point() + theme_bw() +
    geom_label_repel(size = 6) +
    scale_color_manual(values=colors1, name = legend.name) +
    guides(color = guide_legend(override.aes = list(label = 'O', size = 3))) +
    xlab(paste0("PC1 (Explained Variance ", round(PoV[1],4)*100, "%)")) +
    ylab(paste0("PC2 (Explained Variance ", round(PoV[2],4)*100, "%)")) +
    theme(axis.text=element_text(color='black', size=14),
          axis.title=element_text(color='black', size=16))

  if(!show_grp_legend)
    pp1 <- pp1 + guides(color = FALSE)

  # format data for figure
  plotter <- tibble(PC1   = clustered_data$rotation[,'PC1'],
                    PC2   = clustered_data$rotation[,'PC2'],
                    Label = rownames(clustered_data$rotation))

  # proportion of variance
  PoV <- with(clustered_data, sdev^2 / sum(sdev^2))

  # render figure
  pp2 <- extract_sb_loadings(clustered_data) %>%
    ggplot(aes(.data$PC1, .data$PC2, color=.data$Cluster, label=.data$Cluster)) +
    geom_point() + theme_bw() +
    geom_label_repel(size = 6) +
    scale_color_manual(values=colors2) +
    guides(color = guide_legend(override.aes = list(label = 'O', size = 3))) +
    theme(axis.text=element_text(color='black', size=14),
          axis.title=element_text(color='black', size=16))

  if(!show_clust_legend)
    pp2 <- pp2 + guides(color = FALSE)

  pp1 + pp2
}


#' Marker heat plot for Joes Flow
#'
#' @param sample_data Data frame or numeric matrix containing sample data
#' @param ids Character vector of ids for each row in `sample_data`, corresponding to the labels in `meta`
#' @param meta Data frame containing metadata - first column corresponds to ids (when ids are sorted using `order`)
#' @param grp Column of meta to use for grouping of data
#' @param kmeans_groups Vector containing Kmeans group label for each row in `sample_data`
#' @param colors Vector of colors for samples
#' @param sample_size An integer indicating how many samples to use in the generation of the figure
#'
#' @return A grid graphics object
#' @export
#' @importFrom ComplexHeatmap Heatmap
#' @importFrom ComplexHeatmap columnAnnotation
#' @importFrom circlize colorRamp2
#' @importFrom rlang .data
marker_heatJF <- function(sample_data, ids, meta, grp, kmeans_groups, colors, sample_size)
{
  # format data for figure
  plotter <- tibble(SampleID = factor(ids),             # want ids to run from 1:length(unique(ids))
                    Group = meta[as.numeric(.data$SampleID)],
                    Group_fact = factor(.data$Group),
                    Kmeans = kmeans_groups) %>%

    cbind(sample_data)

  # subset data
  plotter_sub <- plotter[sample(1:nrow(plotter), size = min(nrow(plotter), sample_size)),]

  # if we have enough colors, plot group color
  if(length(levels(plotter$Group_fact)) <= length(levels(plotter$SampleID)))
  {
    names(colors)[1:length(unique(meta))] <- unique(meta)
    ha <- ComplexHeatmap::columnAnnotation(Group=plotter_sub$Group, col=list(Group=colors[plotter_sub$Group_fact]))
  }else{
    ha <- NULL
  }

  # create heatmap
  select(plotter_sub, -.data$SampleID, -.data$Group, -.data$Group_fact, -.data$Kmeans) %>%
    t() %>%
    scale() %>%
    ComplexHeatmap::Heatmap(show_row_names = T, show_column_names = F,
                            top_annotation = ha,
                            heatmap_legend_param = list(title = "Scaled Value"),
                            cluster_rows = T, cluster_columns = T, row_names_side = 'left',
                            column_names_gp = grid::gpar(fontsize=7),
                            row_names_gp = grid::gpar(fontsize=10),
                            row_title_gp = grid::gpar(fontsize = 10),
                            row_names_max_width = unit(10,'cm'),
                            use_raster = T,
                            column_split=plotter_sub$Kmeans,
                            cluster_column_slices=F,
                            col = circlize::colorRamp2(c(-2, 0, 2), c("blue", "white", "red")))
}


#' Composition plot for Joes Flow
#'
#' @param meta Data frame containing metadata labels. Names of each element of the vector should correspond to sample IDs and include all IDs in `kmeans_groups`.
#' @param grp Column of meta to use for grouping of data
#' @param kmeans_groups Named character vector containing Kmeans group label for each sample. Names of each element of the vector should correspond to sample IDs and include all IDs in `meta`.
#' @param colors Vector of colors for clusters
#'
#' @return A ggplot object
#' @export
#' @importFrom rlang .data
compositionJF <- function(meta, grp, kmeans_groups, colors)
{
  # grouping labels
  meta_grps <- tibble(id = meta[,1] %>% unlist(),
                      grp = meta[,grp] %>% unlist())

  # data frame for plotting
  plotter <- tibble(SampleID = kmeans_groups$ids,
                    cluster = kmeans_groups$grp)

  # get group labels
  if(nrow(meta_grps) == nrow(plotter))  # Sometimes we get a list of groups for each row of plotter
  {
    # double check that these are sorted properly
    if( any(meta_grps$id != plotter$SampleID))
      stop("Group and sample IDs are not sorted properly")

    plotter$Group <- meta_grps$grp
  }else{                                       # other times we get a look up table with one row per sample ID
    plotter <- plotter %>%
      group_by(.data$SampleID) %>%
      mutate(Group = meta_grps$grp[meta_grps$id == unique(.data$SampleID)] %>%
               as.character()) %>%
      ungroup()
  }


  plotter <- plotter %>%
    # count up totals for each cluster by group and sample ID
    group_by(.data$SampleID, .data$cluster, .data$Group) %>%
    dplyr::summarize(n = length(.data$Group)) %>%
    ungroup() %>%

    # count up totals for each sample ID for each group
    group_by(.data$SampleID, .data$Group) %>%
    mutate(N = sum(.data$n)) %>%
    ungroup() %>%

    # calculate percent of each cluster for each sample ID
    mutate(pct = 100 * .data$n / .data$N) %>%

    # get rid of these unneeded columns
    dplyr::select(-"N", -"n")


  g1 <- ggplot(plotter, aes(.data$SampleID, .data$pct, fill=.data$cluster)) +
    geom_col()+ scale_fill_manual(values=colors) + theme_bw() +
    guides(fill = guide_legend("Cluster")) +
    ylab("Cluster Percentage %") +
    xlab("Sample IDs") +
    theme(axis.title=element_text(size=16)) +
    facet_wrap(~.data$Group, ncol=4, scales="free_x")

  # return plotter and g1
  list(plotter = plotter,
       g1 = g1)
}


#' Plot a dimension reduction figure for JoesFlow
#'
#' @param x Object containing clustered data (expects output from `prcomp`, `uamp`, or `Rtsne`)
#' @param sample_data sample data
#' @param features named vector, features in names(sample_data), names(features) = unique(kmeans_groups) -- names should be ordered
#' @param ... Other objects passed along to functions within clusterJF
#'
#' @return A ggplot object
#' @export
#' @import dplyr
#' @import ggplot2
#' @importFrom rlang .data
dimreductJF <- function(x, sample_data, features, ...) {
  UseMethod('dimreductJF')
}

# method for principal components object
#' @rdname dimreductJF
#' @method dimreductJF prcomp
#' @export
dimreductJF.prcomp <- function(x, sample_data, features, ...) {
  dimreductJF(x$x, sample_data, features, ...)
}

# method for matrix object (output from `umap` or `Rtsne`)
#' @rdname dimreductJF
#' @method dimreductJF matrix
#' @export
dimreductJF.matrix <- function(x, sample_data, features, ...) {
  # set up tibble for dimension reduction coordinates
  plotter <- tibble(dim1 = x[,1],
                    dim2 = x[,2])

  # create list of figures
  glister <- list()
  for(i in 1:length(features))
  {
    glister[[i]] <- mutate(plotter,
                           feat = scale(sample_data[[features[i]]])) %>%
      ggplot(aes(.data$dim1, .data$dim2, z = .data$feat)) +
      stat_summary_hex() +

      theme_void() +
      ggtitle(paste0('Cluster ', names(features)[i], '; ', features[i])) +
      scale_fill_gradient(low = 'grey85', high = 'red3') +
      guides(color = guide_legend(features[i])) +
      theme(legend.position = 'right')
  }

  return(glister)
}