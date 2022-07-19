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
#' @importFrom matrixStats colVars
#' @importFrom reshape2 dcast
#' @importFrom reshape2 melt
#' @importFrom tidyr pivot_wider
#' 
#' @import ggplot2
#' @import patchwork
#' @importFrom cowplot plot_grid
#' @importFrom DT renderDT
#' @importFrom DT renderDataTable
#' @importFrom ggsci pal_d3
#' @importFrom ggsci pal_igv
#' @importFrom gridExtra grid.arrange
#' @importFrom gridExtra arrangeGrob
#' @importFrom RColorBrewer brewer.pal
#' 
#' @import ComplexHeatmap
#' @import fastcluster
#' @import Rtsne
#' @import uwot
app_server <- function(input, output, session) {
  
  colors_clusters_og = c(ggsci::pal_d3("category10")(10), ggsci::pal_d3("category20b")(20), ggsci::pal_igv("default")(51))
  colors_samples = c(RColorBrewer::brewer.pal(5, "Set1"), RColorBrewer::brewer.pal(8, "Dark2"), ggsci::pal_igv("default")(51))
  
  options(shiny.maxRequestSize=1000*1024^2)
  
  vals <- reactiveValues()
  
  ##### Options #####
  
  # Upload::choose flow file
  data_mat <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
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
    
    tt=utils::read.csv(inFile$datapath, header = T, sep=',')
    tt
    
  })
  
  meta_vec <- reactive({
    inFile <- input$file2
    
    if (is.null(inFile))
      return(NULL)
    
    tt=utils::read.csv(inFile$datapath, header = T, sep=',')
    
    # first column: IDs
    # second column: meta data
    retval <- tt[,2]
    names(retval) <- tt[,1]
    
    retval
  })
  
  # Visualize::colors
  output$col_pal <- renderUI({
    col_pals=c("Default",
               "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", 
               "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", 
               "YlGn", "YlGnBu", "YlOrBr", "YlOrRd", "Accent", "Dark2", "Paired", 
               "Pastel1", "Pastel2", "Set1", "Set2", "Set3", "BrBG", "PiYG", 
               "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral"
    )
    
    selectInput("colpal", "Select Color Palette",
                choices=col_pals, selected = "Default")
  })
  
  colors_clusters <- reactive({
    if(input$colpal=="Default"){
      colors_sel=colors_clusters_og
    } else {
      if(length(unique(kmeaner()))<8){
        colors_sel=RColorBrewer::brewer.pal(n = length(unique(kmeaner())), name = input$colpal)
      } else {
        col1=RColorBrewer::brewer.pal(n = 8, name = input$colpal)
        col2=sample(colors_clusters_og, (length(unique(kmeaner()))-8))
        colors_sel=c(col1,col2)
      }
    }
    colors_sel
  })
  
  # Visualize::meta-data group variable
  output$meta_sel <- renderUI({
    msel=colnames(meta_mat())
    
    selectInput("meta_val", "Group variable",
                choices=msel)
  })
  
  # Visualize::number of clusters
  output$select_k <- renderUI({
    sels=unique(as.character(kmeaner()))
    
    selectInput("k_val", "Select cluster",
                choices=sels)
  })
  
  # Visualize::clustering method
  output$cluster_setting<-renderUI({
    sliderInput("kmean",
                "Number of clusters:",
                value = 5,
                min = 1,
                max = 20)
  })
  
  # Visualize::Download height
  # Visualize::Download width
  
  
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
                     Variance=matrixStats::colVars(data_mat2))
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
                     Variance=matrixStats::colVars(h_agg1))
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
      grDevices::pdf(file, width=input$download_width, height=input$download_height)
      print(
        gridExtra::grid.arrange(vals$feat_gg, vals$samp_gg, nrow=1)
      )
      grDevices::dev.off()
    })

  ##### Kmeans #####
  kmeaner<-reactive({
    withProgress({
      data_mat2=data_mat()[,-1]
      ids=data_mat()[,1]
      data_mat2=data.matrix(data_mat2)
      if(input$clust_type=="Kmeans"){
        kmeaner=kmeans(data_mat2, input$kmean)
        kk=paste0("C", kmeaner$cluster)
      } else {
        hc <- fastcluster::hclust(stats::dist(data_mat2)^2, "cen")
        memb <- stats::cutree(hc, k = input$kmean)
        kk=paste0("C", as.character(memb))
      }

      names(kk) <- ids
      kk
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
                meta = meta_mat()[,2],
                colors = colors_samples)
    
    vals$pca_samps<-gg
    
    print(gg)
    
  })
  
  
  output$pca_k_plot = renderPlot({
    
    gg <- pca_coords() %>%
      clusterJF(ids = 1:nrow(data_mat()),
                meta = kmeaner(),
                colors = colors_clusters(),
                legend.name = 'Kmeans')
    
    vals$pca_kmeans<-gg
    
    print(gg)
    
  })
  
  
  ##### sample-based PCA #####
  # run the PCA
  sb_pca <- reactive({
    
    groups_table <- table(data_mat()[,1], kmeaner())
    
    pp <- apply(groups_table, 2, function(x) x / rowSums(groups_table)) %>%
      stats::prcomp()
    
    list(pp = pp, groups_table = groups_table)
  })
  
  # generate the figures
  samp_pca <- reactive({
    
    sb_clusterJF(sb_pca()$pp,
                 ids = rownames(sb_pca()$groups_table),
                 meta = as.character(meta_mat()[,input$meta_val]),
                 colors1 = colors_samples,
                 colors2 = colors_clusters())
    
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
      
      data_mat()[,-1] %>%
        uwot::umap(pca = min(15, ncol(data_mat())-1), fast_sgd = TRUE)
      
    }, message="Calculating UMAP")
  })
  
  
  output$umap_plot = renderPlot({
    
    gg <- umap_coords() %>%
      clusterJF(ids = data_mat()[,1],
                meta = meta_mat()[,2],
                colors = colors_samples)
    
    vals$umap_samps<-gg
    
    print(gg)
    
  })
  
  output$umap_k_plot = renderPlot({
    
    gg <- umap_coords() %>%
      clusterJF(ids = 1:nrow(data_mat()),
                meta = kmeaner(),
                colors = colors_clusters(),
                legend.name = 'Kmeans')
    
    vals$umap_kmeans<-gg
    
    print(gg)
  })
  
  ##### TSNE #####
  
  tsne_coords<-reactive({
    withProgress({
      data_mat2=data_mat()[,-1]
      ids=data_mat()[,1]
      data_mat2=data.matrix(data_mat2)
      mat = Rtsne::Rtsne(data_mat2, initial_dims=15, pca=TRUE, theta=1)$Y
      colnames(mat)=c("tSNE_1", "tSNE_2")
      mat
      
    }, message="Calculating tSNE")
  })
  
  output$tsne_plot = renderPlot({
    data_mat2=data_mat()[,-1]
    ids=data_mat()[,1]
    data_mat2=data.matrix(data_mat2)
    
    tsne_df=tsne_coords()
    plotter=data.frame(tSNE_1=tsne_df[,1], tSNE_2=tsne_df[,2], SampleID=ids)
    
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
    
    colors_samples2=colors_samples
    
    gg=ggplot(sample(plotter), aes(.data$tSNE_1, .data$tSNE_2, color=.data$Group)) +
      geom_point() + theme_bw() +
      scale_color_manual(values=colors_samples2) +
      theme(axis.text=element_text(color='black', size=14),
            axis.title=element_text(color='black', size=16))
    
    vals$tsne_samps<-gg
    
    print(gg)
    
  })
  
  output$tsne_k_plot = renderPlot({
    data_mat2=data_mat()[,-1]
    ids=data_mat()[,1]
    data_mat2=data.matrix(data_mat2)
    
    tsne_df=tsne_coords()
    plotter=data.frame(tSNE_1=tsne_df[,1], tSNE_2=tsne_df[,2], SampleID=ids)
    
    plotter$Kmeans=as.character(kmeaner())
    
    gg=ggplot(sample(plotter), aes(.data$tSNE_1, .data$tSNE_2, color=.data$Kmeans)) +
      geom_point() + theme_bw() +
      scale_color_manual(values=colors_clusters()) +
      theme(axis.text=element_text(color='black', size=14),
            axis.title=element_text(color='black', size=16))
    
    vals$tsne_kmeans<-gg
    
    print(gg)
    
  })
  
  
  ##### Composition #####
  
  composition_plot <- reactive({
    compositionJF(meta_vec(), kmeaner(), colors_clusters())
  })
  
  plotter_melt <- reactive({
    composition_plot()$plotter
  })
  
  output$composition_ui <- renderPlot({
    vals$comp_plot<-composition_plot()$g1
    
    composition_plot()$g1
    
  })
  
  output$click_info <- DT::renderDataTable(server = FALSE, {
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    
    plotter=plotter_melt()
    
    if(!is.null(input$plot1_brush)){
      
      if(is.null(input$plot1_brush$panelvar1)){
        plotter=plotter[order(plotter$SampleID, decreasing=F),]
        click_tab=plotter[round(input$plot1_brush$xmin,0):round(input$plot1_brush$xmax,0),]
      } else {
        plotter_sub=dplyr::filter(plotter, .data$Group==input$plot1_brush$panelvar1)
        plotter_sub=plotter_sub[order(plotter_sub$SampleID, decreasing=F),]
        click_tab=plotter_sub[round(input$plot1_brush$xmin,0):round(input$plot1_brush$xmax,0),]
      }
      
      click_tab=data.frame(SampleID=click_tab$SampleID, 
                           Group=click_tab$Group, 
                           signif(click_tab[,1:(ncol(click_tab)-2)], digits=3)
      )
      
    } else {
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
  
  observe({
    if(input$feat_dim=="PCA" | input$feat_dim=="UMAP" | input$feat_dim=="tSNE"){
      print(input$feat_dim)
      data_mat2=data_mat()[,-1]
      ids=data_mat()[,1]
      data_mat2=data.matrix(data_mat2)
      kmeans=as.character(kmeaner())
      kuniq=unique(kmeans)
      
      msel=colnames(data_mat())[-1]
      
      if(input$feat_dim=="PCA"){
        umap_df=pca_coords()
        plotter=data.frame(UMAP_1=umap_df$x[,1], UMAP_2=umap_df$x[,2], SampleID=ids)
      }
      if(input$feat_dim=="UMAP"){
        umap_df=umap_coords()
        plotter=data.frame(UMAP_1=umap_df[,1], UMAP_2=umap_df[,2], SampleID=ids)
      }
      if(input$feat_dim=="tSNE"){
        umap_df=tsne_coords()
        plotter=data.frame(UMAP_1=umap_df[,1], UMAP_2=umap_df[,2], SampleID=ids)
      }
      
      lapply(1:length(kuniq), function(i) {
        plotter$Feature=data_mat2[,input[[paste0('k', i)]]]
        plotter$Feature=scale(plotter$Feature)
        
        output[[paste0('kk', i)]] <- renderPlot({
          g1=ggplot(sample(plotter), aes(.data$UMAP_1, .data$UMAP_2, z=.data$Feature)) +
            stat_summary_hex() + theme_void() + 
            ggtitle(paste0("Cluster C", i, "; ", input[[paste0('k', i)]])) +
            scale_fill_gradient(low='grey85', high="red3") +
            guides(color = guide_legend(input[[paste0('k', i)]])) +
            theme(legend.position='none')
          g1
        })
      })
    }
  })
  
  
  glist<-reactive({
    glister=list()
    if(input$feat_dim=="PCA" | input$feat_dim=="UMAP" | input$feat_dim=="tSNE"){
      print(input$feat_dim)
      data_mat2=data_mat()[,-1]
      ids=data_mat()[,1]
      data_mat2=data.matrix(data_mat2)
      kmeans=as.character(kmeaner())
      kuniq=unique(kmeans)
      
      msel=colnames(data_mat())[-1]
      
      if(input$feat_dim=="PCA"){
        umap_df=pca_coords()
        plotter=data.frame(UMAP_1=umap_df$x[,1], UMAP_2=umap_df$x[,2], SampleID=ids)
      }
      if(input$feat_dim=="UMAP"){
        umap_df=umap_coords()
        plotter=data.frame(UMAP_1=umap_df[,1], UMAP_2=umap_df[,2], SampleID=ids)
      }
      if(input$feat_dim=="tSNE"){
        umap_df=tsne_coords()
        plotter=data.frame(UMAP_1=umap_df[,1], UMAP_2=umap_df[,2], SampleID=ids)
      }
      #lapply(1:length(kuniq), function(i) 
      for(i in 1:length(kuniq)){
        plotter$Feature=data_mat2[,input[[paste0('k', i)]]]
        plotter$Feature=scale(plotter$Feature)
        glister[[i]]<-
          ggplot(sample(plotter), aes(.data$UMAP_1, .data$UMAP_2, z=.data$Feature)) +
          stat_summary_hex() + theme_void() + 
          ggtitle(paste0("Cluster C", i, "; ", input[[paste0('k', i)]])) +
          scale_fill_gradient(low='grey85', high="red3") +
          guides(color = guide_legend(input[[paste0('k', i)]])) +
          theme(legend.position='right')
      }
    } else {glister=list()}
    glister
  })
  
  output$comp_ui <- renderUI({
    data_mat2=data_mat()[,-1]
    ids=data_mat()[,1]
    data_mat2=data.matrix(data_mat2)
    kmeans=as.character(kmeaner())
    kuniq=unique(kmeans)
    
    msel=colnames(data_mat())[-1]
    howmanyrows=ceiling(length(kuniq)/3)
    
    data.use=t(data_mat2)
    which2select<-c()
    for(jj in 1:length(kuniq)){
      
      cells.1=which(kmeans==kuniq[jj])
      cells.2=which(kmeans!=kuniq[jj])
      c1=rowMeans(data.use[,cells.1])
      c2=rowMeans(data.use[,cells.2])
      orderer=data.frame(c1,c2,diff=(c1-c2),Feature=rownames(data.use))
      orderer=orderer[order(orderer$diff, decreasing=T),]
      if(orderer$diff[1]<0){
        orderer=orderer[order(orderer$c1, decreasing=T),]
      }
      which2select=c(which2select, as.character(orderer$Feature[1]))
    }

    plot_output_list<-lapply(1:howmanyrows, function(m) {
      mult1=(3*m)-2
      mult2=(3*m)-1
      mult3=(3*m)
      
      fluidRow(column(4,
                      selectInput(paste0('k', mult1), paste0('Feature Variable C', mult1),
                                  choices = msel, selected=which2select[mult1]),
                      plotOutput(paste0('kk', mult1))),
               column(4,
                      selectInput(paste0('k', mult2), paste0('Feature Variable C', mult2),
                                  choices = msel, selected=which2select[mult2]),
                      plotOutput(paste0('kk', mult2))),
               column(4,
                      selectInput(paste0('k', mult3), paste0('Feature Variable C', mult3),
                                  choices = msel, selected=which2select[mult3]),
                      plotOutput(paste0('kk', mult3)))
      )
    })
    do.call(tagList, plot_output_list)
  })
  
    
  ##### Markers #####
  output$marker_heat <- renderPlot({
    withProgress({
      h1 <- marker_heatJF(  sample_data = data_mat()[,-1],
                                    ids = data_mat()[,1],
                                   meta = meta_mat()[,input$meta_val],
                          kmeans_groups = kmeaner(),
                                 colors = colors_samples,
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
      ggsave(file, plot = {(vals$pca_samps + vals$pca_kmeans) / vals$pca_clusters},
             width = input$download_width, height = input$download_height, units = "in")
    })
  
  output$pca_coord_download = downloadHandler(
    filename = 'PCA_coords.txt',
    content = function(file) {
      pp=pca_coords()
      ids=data_mat()[,1]
      plotter=data.frame(PC1=pp$x[,1], PC2=pp$x[,2], SampleID=ids)
      plotter$Kmeans=as.character(kmeaner())
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
      utils::write.table(plotter, file, sep='\t', quote=F, row.names=F)
    })
  
  ## Sample-based PCA ##
  sb_pca_download <- reactive({
    
  })
  
  output$pca_download_vals = downloadHandler(
    filename = 'sample_PCA_values.txt',
    content = function(file) {
      data_mat2=data_mat()[,-1]
      ids=data_mat()[,1]
      data_mat2=data.matrix(data_mat2)
      kmeans=as.character(kmeaner())
      
      totaler=data.frame(table(ids))
      k_df = data.frame(table(kmeans, ids))
      k_mat = reshape2::dcast(k_df, ids ~ kmeans)
      
      k_mat=k_mat[,-1]
      k_add=apply(k_mat, 2, function(x){(x/totaler$Freq)*100})
      pp=stats::prcomp(k_add)
      plotter=data.frame(pp$x)
      colnames(plotter)=paste0("PC", 1:ncol(plotter))
      plotter$SampleID=totaler$ids
      
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
      utils::write.table(plotter, file=file, row.names=F, quote=F, sep='\t')
    })
  
  output$pca_download_loading = downloadHandler(
    filename = 'sample_PCA_loadings.txt',
    content = function(file) {
      data_mat2=data_mat()[,-1]
      ids=data_mat()[,1]
      data_mat2=data.matrix(data_mat2)
      kmeans=as.character(kmeaner())
      
      totaler=data.frame(table(ids))
      k_df = data.frame(table(kmeans, ids))
      k_mat = reshape2::dcast(k_df, ids ~ kmeans)
      
      k_mat=k_mat[,-1]
      k_add=apply(k_mat, 2, function(x){(x/totaler$Freq)*100})
      
      pp=stats::prcomp(k_add)
      plotter=data.frame(pp$rotation)
      
      utils::write.table(plotter, file=file, row.names=T, quote=F, sep='\t')
    })
  
  ## UMAP ##
  output$umap_download = downloadHandler(
    filename = 'UMAP_plots.png',
    content = function(file) {
      grDevices::png(file, width=input$download_width, height=input$download_height, units="in", res=200)
      print(
        gridExtra::grid.arrange(
          gridExtra::arrangeGrob(vals$umap_samps, vals$umap_kmeans, nrow=1),
          vals$umap_clusters, nrow=2)
      )
      grDevices::dev.off()
    })
  
  output$umap_coord_download = downloadHandler(
    filename = 'UMAP_coords.txt',
    content = function(file) {
      pp=umap_coords()
      ids=data_mat()[,1]
      plotter=data.frame(pp, SampleID=ids)
      plotter$Kmeans=as.character(kmeaner())
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
      utils::write.table(umap_coords(), file, sep='\t', quote=F, row.names=F)
    })
  
  output$umap_download_vals = downloadHandler(
    filename = 'sample_PCA_values.txt',
    content = function(file) {
      data_mat2=data_mat()[,-1]
      ids=data_mat()[,1]
      data_mat2=data.matrix(data_mat2)
      kmeans=as.character(kmeaner())
      
      totaler=data.frame(table(ids))
      k_df = data.frame(table(kmeans, ids))
      k_mat = reshape2::dcast(k_df, ids ~ kmeans)
      
      k_mat=k_mat[,-1]
      k_add=apply(k_mat, 2, function(x){(x/totaler$Freq)*100})
      
      pp=stats::prcomp(k_add)
      plotter=data.frame(pp$x)
      colnames(plotter)=paste0("PC", 1:ncol(plotter))
      plotter$SampleID=totaler$ids
      
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
      utils::write.table(plotter, file=file, row.names=F, quote=F, sep='\t')
    })
  
  output$umap_download_loading = downloadHandler(
    filename = 'sample_PCA_loadings.txt',
    content = function(file) {
      data_mat2=data_mat()[,-1]
      ids=data_mat()[,1]
      data_mat2=data.matrix(data_mat2)
      kmeans=as.character(kmeaner())
      
      totaler=data.frame(table(ids))
      k_df = data.frame(table(kmeans, ids))
      k_mat = reshape2::dcast(k_df, ids ~ kmeans)
      
      k_mat=k_mat[,-1]
      k_add=apply(k_mat, 2, function(x){(x/totaler$Freq)*100})
      
      pp=stats::prcomp(k_add)
      plotter=data.frame(pp$rotation)
      
      utils::write.table(plotter, file=file, row.names=T, quote=F, sep='\t')
    })
  
  
  ## TSNE ##
  output$tsne_download = downloadHandler(
    filename = 'TSNE_plots.png',
    content = function(file) {
      grDevices::png(file, width=input$download_width, height=input$download_height, units="in", res=200)
      print(
        gridExtra::grid.arrange(
          gridExtra::arrangeGrob(vals$tsne_samps, vals$tsne_kmeans, nrow=1),
          vals$tsne_clusters, nrow=2)
      )
      grDevices::dev.off()
    })
  
  output$tsne_download_vals = downloadHandler(
    filename = 'sample_PCA_values.txt',
    content = function(file) {
      data_mat2=data_mat()[,-1]
      ids=data_mat()[,1]
      data_mat2=data.matrix(data_mat2)
      kmeans=as.character(kmeaner())
      
      totaler=data.frame(table(ids))
      k_df = data.frame(table(kmeans, ids))
      k_mat = reshape2::dcast(k_df, ids ~ kmeans)
      
      k_mat=k_mat[,-1]
      k_add=apply(k_mat, 2, function(x){(x/totaler$Freq)*100})
      
      pp=stats::prcomp(k_add)
      plotter=data.frame(pp$x)
      colnames(plotter)=paste0("PC", 1:ncol(plotter))
      plotter$SampleID=totaler$ids
      
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
      utils::write.table(plotter, file=file, row.names=F, quote=F, sep='\t')
    })
  
  output$tsne_download_loading = downloadHandler(
    filename = 'sample_PCA_loadings.txt',
    content = function(file) {
      data_mat2=data_mat()[,-1]
      ids=data_mat()[,1]
      data_mat2=data.matrix(data_mat2)
      kmeans=as.character(kmeaner())
      
      totaler=data.frame(table(ids))
      k_df = data.frame(table(kmeans, ids))
      k_mat = reshape2::dcast(k_df, ids ~ kmeans)
      
      k_mat=k_mat[,-1]
      k_add=apply(k_mat, 2, function(x){(x/totaler$Freq)*100})
      
      pp=stats::prcomp(k_add)
      plotter=data.frame(pp$rotation)
      
      utils::write.table(plotter, file=file, row.names=T, quote=F, sep='\t')
    })

  output$tsne_coord_download = downloadHandler(
    filename = 'TSNE_coords.txt',
    content = function(file) {
      pp=tsne_coords()
      ids=data_mat()[,1]
      plotter=data.frame(pp, SampleID=ids)
      plotter$Kmeans=as.character(kmeaner())
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
      utils::write.table(tsne_coords(), file, sep='\t', quote=F, row.names=F)
    })

  ## Composition ##
  output$comp_download = downloadHandler(
    filename = 'Composition_plot.pdf',
    content = function(file) {
      grDevices::pdf(file, width=input$download_width, height=input$download_height)
      print(vals$comp_plot)
      grDevices::dev.off()
    })
  
  output$comp_feat_download = downloadHandler(
    filename = 'Composition_plot_features.pdf',
    content = function(file) {
      grDevices::pdf(file, width=input$download_width, height=input$download_height)
      print(cowplot::plot_grid(plotlist=glist(), ncol=3))
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
      grDevices::pdf(file, width=input$download_width, height=input$download_height)
      print(gridExtra::grid.arrange(vals$marker_heat))
      grDevices::dev.off()
    })

}
