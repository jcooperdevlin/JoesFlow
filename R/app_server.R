#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @export
#' @importFrom shiny column
#' @importFrom shiny downloadHandler
#' @importFrom shiny observe
#' @importFrom shiny fluidRow
#' @importFrom shiny plotOutput
#' @importFrom shiny reactive
#' @importFrom shiny reactiveValues
#' @importFrom shiny renderPlot
#' @importFrom shiny renderUI
#' @importFrom shiny selectInput
#' @importFrom shiny sliderInput
#' @importFrom shiny tagList
#' @importFrom shiny withProgress
#'
#' @import dplyr
#' @importFrom reshape2 dcast
#' @importFrom reshape2 melt
#' @importFrom tidyr pivot_wider
#'
#' @import ggplot2
#' @import patchwork
#' @import hexbin
#' @importFrom cowplot plot_grid
#' @importFrom DT renderDT
#' @importFrom DT renderDataTable
#' @importFrom ggsci pal_d3
#' @importFrom ggsci pal_igv
#' @importFrom RColorBrewer brewer.pal
#' @importFrom RColorBrewer brewer.pal.info
#'
#' @importFrom ComplexHeatmap draw
#' @importFrom fastcluster hclust
#' @importFrom uwot umap
#' @importFrom Rtsne Rtsne
#'
#' @importFrom stringi stri_read_raw
#' @importFrom stringi stri_enc_detect
app_server <- function(input, output, session) {

  options(shiny.maxRequestSize=1000*1024^2)

  vals <- reactiveValues()

  ##### Options #####

  # Upload::choose flow file
  data_mat <- reactive({
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    set.seed(input$seed)

    tt=utils::read.csv(inFile$datapath, header = T, sep=',')
    if(input$subsample<1){
      tt=tt[sample(rownames(tt), nrow(tt)*input$subsample),]
    }

    tt
  })

  # Upload::choose metadata file
  meta_mat <- reactive({
    inFile <- input$file2

    if (is.null(inFile))
      return(NULL)

    # check file encoding for odd characters (don't do this for flow files, as they tend to be very big and pretty much all numeric)
    enc <- stringi::stri_read_raw(inFile$datapath) %>%
      stringi::stri_enc_detect()

    if(enc[[1]]$Encoding[1] == 'UTF-8')
    {
      tt <- utils::read.csv(inFile$datapath)
    }else{
      tt <- utils::read.csv(inFile$datapath, encoding = 'latin1')
    }

    tt
  })

  # Visualize::colors
  output$col_pal <- renderUI({
    col_pals=c("Default", rownames(RColorBrewer::brewer.pal.info)[RColorBrewer::brewer.pal.info$colorblind &
                                                                  RColorBrewer::brewer.pal.info$category == 'qual'])

    selectInput("colpal", "Select Color Palette",
                choices=col_pals, selected = "Default")
  })

  colors_clusters <- reactive({
    if(input$colpal=="Default")
    {
      colors_sel <- c(ggsci::pal_d3("category10")(10), ggsci::pal_d3("category20b")(20), ggsci::pal_igv("default")(51))
    }else{
      # total number of colors to choose from for this pallet
      n_unique_colors <- RColorBrewer::brewer.pal(n = RColorBrewer::brewer.pal.info[input$colpal,'maxcolors'],
                                                name = input$colpal)

      # pull colors from brewer.pal
      colors_sel = RColorBrewer::brewer.pal(n = n_unique_colors, name = input$colpal)
    }

    # make sure we have enough colors (repeat if necessary)
    rep(colors_sel, length.out = input$kmean)
  })

  colors_samples <- reactive({
    colors_sel <- c(RColorBrewer::brewer.pal(5, "Set1"), RColorBrewer::brewer.pal(8, "Dark2"), ggsci::pal_igv("default")(51))

    # make sure we have enough colors (repeat if necessary)
    rep(colors_sel, length.out = length(unique(meta_mat()[,input$meta_val])))
  })

  # Visualize::meta-data group variable
  output$meta_sel <- renderUI({
    msel=colnames(meta_mat())

    selectInput("meta_val", "Group variable",
                choices=msel)
  })

  # Visualize::select cluster?
  output$select_k <- renderUI({
    sels=unique(as.character(kmeaner()))

    selectInput("k_val", "Select cluster",
                choices=sels)
  })

  # Visualize::number of clusters
  output$cluster_setting<-renderUI({
    numericInput("kmean",
                "Number of clusters:",
                value = 5,
                min = 2)
  })


  ##### Data input tables #####

  output$metadata <- DT::renderDT({
    meta_mat()
  })

  output$contents <- DT::renderDT({
    data_mat()
  })


  ##### Features Figures #####

  output$feats_plot = renderPlot({
    data_mat2=data_mat()[,-1]
    data_mat2=data.matrix(data_mat2)

    rvars=data.frame(Feature=colnames(data_mat2),
                     Variance=apply(data_mat2, 2, stats::var))
    rvars=rvars[order(rvars$Variance, decreasing=T),]

    rvars$Feature=factor(rvars$Feature, levels=rev(as.character(rvars$Feature)))
    gg=ggplot(rvars[1:15,], aes(.data$Feature, .data$Variance)) + geom_col(fill='navy') +
      coord_flip() + theme_bw() + ggtitle("Cellular Variance") +
      theme(axis.text=element_text(color='black', size=14),
            axis.title=element_text(color='black', size=16),
            plot.title = element_text(size=16))

    vals$feat_gg <- gg

    print(gg)

  })


  output$sample_var = renderPlot({
    data_mat2=data_mat()[,-1]
    ids=data_mat()[,1]
    data_mat2=data.matrix(data_mat2)

    plotter=data.frame(SampleID=ids)

    if(length(input$meta_val)>0){
      grouper=meta_mat()[,input$meta_val]

      plotter$Group=as.character(plotter$SampleID)
      samps=as.character(unique(plotter$SampleID))
      for(jj in 1:length(samps)){
        grouper=dplyr::filter(meta_mat(), .data$ID==samps[jj])
        grouper=as.character(grouper[,input$meta_val][1])

        plotter$Group[plotter$SampleID==samps[jj]]<-grouper
      }
    } else {
      plotter$Group=plotter$SampleID
    }

    h_agg=stats::aggregate(data_mat2, by=list(plotter$Group), "mean")
    h_agg1=data.matrix(h_agg[,-1])

    rvars=data.frame(Feature=colnames(h_agg1),
                     Variance=apply(h_agg1, 2, stats::var))
    rvars=rvars[order(rvars$Variance, decreasing=T),]

    rvars$Feature=factor(rvars$Feature, levels=rev(as.character(rvars$Feature)))
    gg=ggplot(rvars[1:15,], aes(.data$Feature, .data$Variance)) + geom_col(fill='navy') +
      coord_flip() + theme_bw() + ggtitle("Sample Variance") +
      theme(axis.text=element_text(color='black', size=14),
            axis.title=element_text(color='black', size=16),
            plot.title = element_text(size=16))

    vals$samp_gg<-gg

    print(gg)

  })


  output$feat_download = downloadHandler(
    filename = 'FeaturePlot.pdf',
    content = function(file) {
      ggsave(file,
             plot = {vals$feat_gg + vals$samp_gg},
             width=input$download_width,
             height=input$download_height,
             units = 'in')
    })

  ##### Kmeans #####
  kmeaner<-reactive({
    withProgress({
      data_mat2=data_mat()[,-1]
      ids=data_mat()[,1]
      data_mat2=data.matrix(data_mat2)

      set.seed(input$seed)

      if(input$clust_type=="Kmeans"){
        kmeaner=kmeans(data_mat2, input$kmean)
        kk=paste0("C", kmeaner$cluster)
      } else {
        hc <- fastcluster::hclust(stats::dist(data_mat2)^2, "cen")
        memb <- stats::cutree(hc, k = input$kmean)
        kk=paste0("C", as.character(memb))
      }

      tibble(ids = ids,
             grp = kk)
    }, message = "Calculating clusters")
  })


  ##### PCA analysis #####
  pca_coords<-reactive({
    withProgress({

      data_mat()[,-1] %>% # strip ID column
        stats::prcomp(scale=T)   # run PCA

    }, message="Calculating PCA")
  })

  output$pca_plot = renderPlot({

    gg <- pca_coords() %>%
      clusterJF(ids = data_mat()[,1],
                meta = meta_mat(),
                grp = input$meta_val,
                colors = colors_samples(),
                legend.name = input$meta_val)

    vals$pca_samps<-gg

    print(gg)

  })


  output$pca_k_plot = renderPlot({

    gg <- pca_coords() %>%
      clusterJF(ids = data_mat()[,1],
                meta = kmeaner(),
                grp = 'grp',
                colors = colors_clusters(),
                legend.name = 'Cluster')

    vals$pca_kmeans<-gg

    print(gg)

  })


  ##### sample-based PCA #####
  # run the PCA
  sb_pca <- reactive({

    groups_table <- table(kmeaner())

    pp <- apply(groups_table, 2, function(x) x / rowSums(groups_table)) %>%
      stats::prcomp()

    list(pp = pp, groups_table = groups_table)
  })

  # generate the figures
  samp_pca <- reactive({

    sb_clusterJF(sb_pca()$pp,
                 ids = rownames(sb_pca()$groups_table),
                 meta = meta_mat(),
                 grp = input$meta_val,
                 colors1 = colors_samples(),
                 colors2 = colors_clusters(),
                 legend.name = input$meta_val)

  })
  output$samp_p_pca <- renderPlot({
    vals$pca_clusters <- samp_pca()
    print(samp_pca())
  })
  output$samp_pca <- renderPlot({
    vals$umap_clusters <- samp_pca()
    print(samp_pca())
  })
  output$samp_t_pca <- renderPlot({
    vals$tsne_clusters <- samp_pca()
    print(samp_pca())
  })

  ##### UMAP #####
  umap_coords<-reactive({
    withProgress({

      set.seed(input$seed)

      data_mat()[,-1] %>%
        uwot::umap(pca = min(15, ncol(data_mat())-1), fast_sgd = TRUE)

    }, message="Calculating UMAP")
  })


  output$umap_plot = renderPlot({

    gg <- umap_coords() %>%
      clusterJF(axis_prefix = 'UMAP',
                ids = data_mat()[,1],
                meta = meta_mat(),
                grp = input$meta_val,
                colors = colors_samples(),
                legend.name = input$meta_val)

    vals$umap_samps<-gg

    print(gg)

  })

  output$umap_k_plot = renderPlot({

    gg <- umap_coords() %>%
      clusterJF(axis_prefix = 'UMAP',
                ids = data_mat()[,1],
                meta = kmeaner(),
                grp = 'grp',
                colors = colors_clusters(),
                legend.name = 'Cluster')

    vals$umap_kmeans<-gg

    print(gg)
  })

  ##### TSNE #####

  tsne_coords<-reactive({
    withProgress({

      set.seed(input$seed)

      mat <- data_mat()[,-1] %>%
        Rtsne::Rtsne(initial_dims=15, pca=TRUE, theta=1)
      mat <- mat[['Y']]

      colnames(mat)=c("tSNE_1", "tSNE_2")

      mat

    }, message="Calculating tSNE")
  })

  output$tsne_plot = renderPlot({

    gg <- tsne_coords() %>%
      clusterJF(axis_prefix = 'tSNE',
                ids = data_mat()[,1],
                meta = meta_mat(),
                grp = input$meta_val,
                colors = colors_samples(),
                legend.name = input$meta_val)

    vals$tsne_samps<-gg

    print(gg)

  })

  output$tsne_k_plot = renderPlot({

    gg <- tsne_coords() %>%
      clusterJF(axis_prefix = 'tSNE',
                ids = data_mat()[,1],
                meta = kmeaner(),
                grp = 'grp',
                colors = colors_clusters(),
                legend.name = 'Cluster')

    vals$tsne_kmeans<-gg

    print(gg)

  })


  ##### Composition #####

  composition_plot <- reactive({
    compositionJF(meta = meta_mat(),
                  grp = input$meta_val,
                  kmeans_groups = kmeaner(),
                  colors = colors_clusters())
  })

  plotter_melt <- reactive({
    composition_plot()$plotter
  })

  output$composition_ui <- renderPlot({
    vals$comp_plot<-composition_plot()$g1

    composition_plot()$g1

  })

  output$click_info <- DT::renderDataTable(server = FALSE, {
    # input$plot1_brush is defined in app_ui
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.

    # make the dataframe wider before sending to renderDataTable
    plotter=plotter_melt() %>%
      dplyr::mutate(pct = signif(.data$pct, digits = 3)) %>%
      tidyr::pivot_wider(id_cols = c('SampleID', 'Group'), names_from = 'cluster', values_from = 'pct')

    # if we have a selection
    if(!is.null(input$plot1_brush)){
      # if there is mroe than one panel, subset to the selected panel
      if(!is.null(input$plot1_brush$panelvar1))
        plotter <- dplyr::filter(plotter, .data$Group==input$plot1_brush$panelvar1)

      # verify that it is sorted by SampleID (should be already)
      plotter <- dplyr::arrange(plotter, .data$SampleID)

      # grab the correct columns of data basaed on the selected area of the graph
      click_tab=plotter[round(input$plot1_brush$xmin,0):round(input$plot1_brush$xmax,0),]
    } else {
      # otherwise display nothing
      click_tab=data.frame()
    }

    click_tab
  }, extensions = 'Buttons',

  options = list(
    paging = TRUE,
    searching = TRUE,
    fixedColumns = TRUE,
    autoWidth = TRUE,
    ordering = TRUE,
    dom = 'lfrtipB',
    buttons = c('copy', 'csv', 'excel')
  ),

  class = "display"
  )

  # generate figures for `Select Dimension Reduction`
  dimreduct <- reactive({
    if(input$feat_dim %in% c("PCA", "UMAP", "tSNE"))
    {
      # get coordinates from the correct dimension reduction method
      if(input$feat_dim=="PCA"){
        x <- pca_coords()$x
      }
      if(input$feat_dim=="UMAP"){
        x <- umap_coords()
      }
      if(input$feat_dim=="tSNE"){
        x <- tsne_coords()
      }

      # selected features
      features <- sapply(paste0('k', 1:input$kmean), function(x) input[[x]])

      # cluster selected for each feature
      names(features) <- paste0('C', 1:input$kmean)

      # generate figures
      glister <- dimreductJF(x, data_mat()[,-1], features)
    }else{
      glister <- list()
    }

    glister
  })

  # this is for the `Select Dimension Reduction` UI
  output$comp_ui <- renderUI({
    data_mat2=data_mat()[,-1]
    kmeans=as.character(kmeaner()$grp)

    msel=colnames(data_mat2)
    howmanyrows=ceiling(input$kmean/3)

    which2select<-character(input$kmean)
    for(jj in 1:input$kmean){
      # calculate column means for cluster jj and all others
      clusterjj    <- which(kmeans==paste0('C', jj))
      clusterOther <- which(kmeans!=paste0('C', jj))

      orderer <- tibble(meansjj    = colMeans(data_mat2[   clusterjj,]),
                        meansOther = colMeans(data_mat2[clusterOther,]),
                        diff = .data$meansjj - .data$meansOther,
                        Feature=colnames(data_mat2)) %>%

        # sort such that the feature with the largest difference is at the top
        arrange(desc(.data$diff))

      # if the largest difference is smaller than everything else, pick the feature with the largest mean value
      if(orderer$diff[1]<0)
        orderer <- arrange(orderer, desc(.data$meansjj))

      # this is the top feature of interest for cluster jj
      which2select[jj] <- orderer$Feature[1]
    }

    # generate UI elements for each cluster/feature (3 columns by howmanyrows rows)
    plot_output_list<-lapply(1:howmanyrows, function(m) {

      # cluster/figure numbers (don't add extra plots on the last row)
      plts <- (3*m - 2):min(3*m, input$kmean)

      # code for fluidRow entry
      ui_code <-
        paste("fluidRow(",
        paste0("  column(4,\n",
               "         selectInput(inputId = 'k", plts, "',\n",
               "                     label = 'Feature Variable C", plts, "',\n",
               "                     choices = msel,\n",
               "                     selected = which2select[", plts, "]),\n",
               "         plotOutput('kk", plts, "'))",
               collapse = ',\n'),
        ")", sep = '\n')

      eval(parse(text = ui_code))
    })
    do.call(tagList, plot_output_list)
  })

  # this populates the figures for `Select Dimension Reduction`
  observe({
    # if a dimension reduction method has been selected
    if(input$feat_dim %in% c("PCA", "UMAP", "tSNE")){

      # add generated figures to UI
      lapply(1:input$kmean, function(i) {
        output[[paste0('kk', i)]] <- renderPlot(dimreduct()[[i]])
      })
    }
  })


  ##### Markers #####
  output$marker_heat <- renderPlot({
    withProgress({
      set.seed(input$seed)
      h1 <- marker_heatJF(  sample_data = data_mat()[,-1],
                                    ids = data_mat()[,1],
                                   meta = meta_mat()[,input$meta_val],
                          kmeans_groups = kmeaner(),
                                 colors = colors_samples(),
                            sample_size = 500)

      vals$marker_heat <-h1 %>%
        ComplexHeatmap::draw() %>%
        grid::grid.grabExpr()

      h1
    }, message="Generating heatmap")
  })

  ##### Downloads #####

  ## PCA ##
  output$pca_download = downloadHandler(
    filename = 'PCA_plots.png',
    content = function(file) {
      ggsave(file,
             plot = {(vals$pca_samps + vals$pca_kmeans) / vals$pca_clusters},
             width = input$download_width,
             height = input$download_height,
             units = "in")
    })

  output$pca_coord_download = downloadHandler(
    filename = 'PCA_coords.txt',
    content = function(file) {

      extract_values(clustered_data = pca_coords(),
                     ids            = data_mat()[,1],
                     meta           = meta_mat(),
                     grp            = input$meta_val) %>%

        rename(PC1 = X1, PC2 = X2) %>%

        utils::write.table(file, sep='\t', quote=FALSE, row.names=FALSE)
    })

  ## Sample-based PCA ##
  sb_vals <- reactive({
    extract_sb_values(clustered_data = sb_pca()$pp,
                      ids            = rownames(sb_pca()$groups_table),
                      meta           = meta_mat(),
                      grp            = input$meta_val)
  })

  output$pca_download_vals = downloadHandler(
    filename = 'sample_PCA_values.txt',
    content = function(file) {
      utils::write.table(sb_vals, file=file, row.names=FALSE, quote=FALSE, sep='\t')
    })

  output$pca_download_loading = downloadHandler(
    filename = 'sample_PCA_loadings.txt',
    content = function(file) {
      extract_sb_loadings(sb_pca()$pp) %>%
        utils::write.table(file=file, row.names=FALSE, quote=FALSE, sep='\t')
    })

  ## UMAP ##
  output$umap_download = downloadHandler(
    filename = 'UMAP_plots.png',
    content = function(file) {
      ggsave(file,
             plot = {(vals$umap_samps + vals$umap_kmeans) / vals$pca_clusters},
             width = input$download_width,
             height = input$download_height,
             units = "in")
    })

  output$umap_coord_download = downloadHandler(
    filename = 'UMAP_coords.txt',
    content = function(file) {
      extract_values(clustered_data = umap_coords(),
                     ids            = data_mat()[,1],
                     meta           = meta_mat(),
                     grp            = input$meta_val) %>%

        rename(UMAP_1 = X1, UMAP_2 = X2) %>%

        utils::write.table(file, sep='\t', quote=FALSE, row.names=FALSE)
    })

  output$umap_download_vals = downloadHandler(
    filename = 'sample_PCA_values.txt',
    content = function(file) {
      utils::write.table(sb_vals, file=file, row.names=FALSE, quote=FALSE, sep='\t')
    })

  output$umap_download_loading = downloadHandler(
    filename = 'sample_PCA_loadings.txt',
    content = function(file) {
      extract_sb_loadings(sb_pca()$pp) %>%
        utils::write.table(file=file, row.names=FALSE, quote=FALSE, sep='\t')
    })

  ## TSNE ##
  output$tsne_download = downloadHandler(
    filename = 'TSNE_plots.png',
    content = function(file) {
      ggsave(file,
             plot = {(vals$tsne_samps + vals$tsne_kmeans) / vals$pca_clusters},
             width = input$download_width,
             height = input$download_height,
             units = "in")
    })

  output$tsne_coord_download = downloadHandler(
    filename = 'TSNE_coords.txt',
    content = function(file) {
      extract_values(clustered_data = tsne_coords(),
                     ids            = data_mat()[,1],
                     meta           = meta_mat(),
                     grp            = input$meta_val) %>%

        rename(tSNE_1 = X1, tSNE_2 = X2) %>%

        utils::write.table(file, sep='\t', quote=FALSE, row.names=FALSE)
    })

  output$tsne_download_vals = downloadHandler(
    filename = 'sample_PCA_values.txt',
    content = function(file) {
      utils::write.table(sb_vals, file=file, row.names=FALSE, quote=FALSE, sep='\t')
    })

  output$tsne_download_loading = downloadHandler(
    filename = 'sample_PCA_loadings.txt',
    content = function(file) {
      extract_sb_loadings(sb_pca()$pp) %>%
        utils::write.table(file=file, row.names=FALSE, quote=FALSE, sep='\t')
    })

  ## Composition ##
  output$comp_download = downloadHandler(
    filename = 'Composition_plot.pdf',
    content = function(file) {
      ggsave(file,
             plot = vals$comp_plot,
             width = input$download_width,
             height = input$download_height,
             units = "in")
    })

  output$comp_feat_download = downloadHandler(
    filename = 'Composition_plot_features.pdf',
    content = function(file) {
      grDevices::pdf(file, width=input$download_width, height=input$download_height)
      print(cowplot::plot_grid(plotlist=dimreduct(), ncol=3))
      grDevices::dev.off()
    })

  output$comp_download_table = downloadHandler(
    filename = 'Composition_table.txt',
    content = function(file) {
      plotter=plotter_melt()
      plotter_melt=reshape2::melt(data=plotter, id.vars=c("SampleID", "Group"))
      utils::write.table(plotter_melt, file=file, row.names=F, sep='\t', quote=F)
    })

  ## Markers ##
  output$heat_download = downloadHandler(
    filename = 'marker_heatmap.pdf',
    content = function(file) {
      ggsave(file,
             plot = vals$marker_heat,
             width = input$download_width,
             height = input$download_height,
             units = "in")
    })
}
