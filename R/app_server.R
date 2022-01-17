#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  library(DT)
  library(ggplot2)
  library(matrixStats)
  library(uwot)
  library(RColorBrewer)
  library(ggsci)
  library(reshape2)
  library(gridExtra)
  library(ggrepel)
  library(Rtsne)
  library(ComplexHeatmap)
  library(circlize)
  library(hexbin)
  library(dplyr)
  library(periscope)
  library(cowplot)
  library(fastcluster)
  
  #library(RANN)
  #library(igraph)
  #library(mstknnclust)
  
  colors_clusters_og = c(pal_d3("category10")(10), pal_d3("category20b")(20), pal_igv("default")(51))
  colors_samples = c(brewer.pal(5, "Set1"), brewer.pal(8, "Dark2"), pal_igv("default")(51))
  
  options(shiny.maxRequestSize=1000*1024^2)
  
  vals <- reactiveValues()
  
  #colors
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
        colors_sel=brewer.pal(n = length(unique(kmeaner())), name = input$colpal)
      } else {
        col1=brewer.pal(n = 8, name = input$colpal)
        col2=sample(colors_clusters_og, (length(unique(kmeaner()))-8))
        colors_sel=c(col1,col2)
      }
    }
    colors_sel
  })
  
  data_mat <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    tt=read.csv(inFile$datapath, header = T, sep=',')
    if(input$subsample<1){
      tt=tt[sample(rownames(tt), nrow(tt)*input$subsample),]
    }
    
    tt
    
  })
  
  meta_mat <- reactive({
    inFile <- input$file2
    
    if (is.null(inFile))
      return(NULL)
    
    tt=read.csv(inFile$datapath, header = T, sep=',')
    tt
    
  })
  
  output$metadata <- renderDT({
    meta_mat()
  })
  
  output$contents <- renderDT({
    data_mat()
  })
  
  output$feats_distr <- renderPlot({
    data_mat2=data_mat()[,-1]
    data_mat2=data.matrix(data_mat2)
    
    vars=colVars(data_mat2)
    sds=colSds(data_mat2)
    means=colSums(data_mat2)
    
    plotter=data.frame(Feature=colnames(data_mat2), vars, sds, means)
    
    labels=data.frame(Feature=colnames(data_mat2), xPos=max(means), yPos=max((density(means))$y))
    g1=ggplot(plotter, aes(means)) + geom_density(color='dodgerblue2') + theme_minimal() + 
      xlab("Feature Mean") + ylab("Density") +
      geom_vline(xintercept = input$mean_cutoff, color="black", size=3)
    g2=ggplot(plotter, aes(sds)) + geom_density(color='red3') + theme_minimal() + 
      xlab("Feature SD") + ylab("Density") +
      geom_vline(xintercept = input$SD_cutoff, color="black", size=3)
    g3=ggplot(plotter, aes(vars)) + geom_density(color='darkorange1') + theme_minimal() + 
      xlab("Feature Variance") + ylab("Density") +
      geom_vline(xintercept = input$var_cutoff, color="black", size=3)
    
    grid.arrange(g1,g2,g3)
    
  }, height=500)
  
  output$feats_plot = renderPlot({
    data_mat2=data_mat()[,-1]
    data_mat2=data.matrix(data_mat2)
    
    rvars=data.frame(Feature=colnames(data_mat2),
                     Variance=colVars(data_mat2))
    rvars=rvars[order(rvars$Variance, decreasing=T),]
    
    rvars$Feature=factor(rvars$Feature, levels=rev(as.character(rvars$Feature)))
    gg=ggplot(rvars[1:15,], aes(Feature, Variance)) + geom_col(fill='navy') +
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
        grouper=subset(meta_mat(), ID==samps[jj])
        grouper=as.character(grouper[,input$meta_val][1])
        
        plotter$Group[plotter$SampleID==samps[jj]]<-grouper
      }
    } else {
      plotter$Group=plotter$SampleID
    }
    
    h_agg=aggregate(data_mat2, by=list(plotter$Group), "mean")
    h_agg1=data.matrix(h_agg[,-1])
    
    rvars=data.frame(Feature=colnames(h_agg1),
                     Variance=colVars(h_agg1))
    rvars=rvars[order(rvars$Variance, decreasing=T),]
    
    rvars$Feature=factor(rvars$Feature, levels=rev(as.character(rvars$Feature)))
    gg=ggplot(rvars[1:15,], aes(Feature, Variance)) + geom_col(fill='navy') +
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
      pdf(file, width=input$download_width, height=input$download_height)
      print(
        grid.arrange(vals$feat_gg, vals$samp_gg, nrow=1)
      )
      dev.off()
    })
  
  
  output$meta_sel <- renderUI({
    msel=colnames(meta_mat())
    
    selectInput("meta_val", "Group variable",
                choices=msel)
  })
  
  
  output$select_k <- renderUI({
    sels=unique(as.character(kmeaner()))
    
    selectInput("k_val", "Select cluster",
                choices=sels)
  })
  
  output$select_p <- renderUI({
    numericInput("p_val",
                 "Select p-value cutoff",
                 value = 0.05,
                 min = 0, step=0.05,
                 max = 1)
  })
  
  output$select_l <- renderUI({
    numericInput("log_val",
                 "Select log2 fold change cutoff",
                 value = 1, step=0.5,
                 min = -10,
                 max = 100)
  })
  
  
  pca_coords<-reactive({
    withProgress({
      data_mat2=data_mat()[,-1]
      ids=data_mat()[,1]
      data_mat2=data.matrix(data_mat2)
      
      pp=prcomp(data_mat2, scale=T)
      pp
      
    }, message="Calculating PCA")
  })
  
  output$pca_plot = renderPlot({
    data_mat2=data_mat()[,-1]
    ids=data_mat()[,1]
    data_mat2=data.matrix(data_mat2)
    
    pp=pca_coords()
    plotter=data.frame(PC1=pp$x[,1], PC2=pp$x[,2], SampleID=ids)
    
    if(length(input$meta_val)>0){
      grouper=meta_mat()[,input$meta_val]
      
      plotter$Group=as.character(plotter$SampleID)
      samps=as.character(unique(plotter$SampleID))
      for(jj in 1:length(samps)){
        grouper=subset(meta_mat(), ID==samps[jj])
        grouper=as.character(grouper[,input$meta_val][1])
        
        plotter$Group[plotter$SampleID==samps[jj]]<-grouper
      }
    } else {
      plotter$Group=plotter$SampleID
    }
    
    
    PoV <- pp$sdev^2/sum(pp$sdev^2)
    
    colors_samples2=colors_samples
    #if(min(table(plotter$Group))<2){
    #  colors_samples2=c(rep("black", length(unique(plotter$Group))))
    #} 
    
    gg=ggplot(plotter, aes(PC1, PC2, color=Group)) +
      geom_point() + theme_bw() +
      scale_color_manual(values=colors_samples2) +
      xlab(paste0("PC1 (Explained Variance ", round(PoV[1],4)*100, "%)")) +
      ylab(paste0("PC2 (Explained Variance ", round(PoV[2],4)*100, "%)")) +
      theme(axis.text=element_text(color='black', size=14),
            axis.title=element_text(color='black', size=16))
    
    vals$pca_samps<-gg
    
    print(gg)
    
  })
  
  
  output$pca_k_plot = renderPlot({
    data_mat2=data_mat()[,-1]
    ids=data_mat()[,1]
    data_mat2=data.matrix(data_mat2)
    
    pp=pca_coords()
    plotter=data.frame(PC1=pp$x[,1], PC2=pp$x[,2], SampleID=ids)
    
    plotter$Kmeans=as.character(kmeaner())
    
    PoV <- pp$sdev^2/sum(pp$sdev^2)
    
    gg=ggplot(sample(plotter), aes(PC1, PC2, color=Kmeans)) +
      geom_point() + theme_bw() +
      scale_color_manual(values=colors_clusters()) +
      xlab(paste0("PC1 (Explained Variance ", round(PoV[1],4)*100, "%)")) +
      ylab(paste0("PC2 (Explained Variance ", round(PoV[2],4)*100, "%)")) +
      theme(axis.text=element_text(color='black', size=14),
            axis.title=element_text(color='black', size=16))
    
    vals$pca_kmeans<-gg
    
    print(gg)
    
  })
  
  output$samp_p_pca <- renderPlot({
    data_mat2=data_mat()[,-1]
    ids=data_mat()[,1]
    data_mat2=data.matrix(data_mat2)
    kmeans=as.character(kmeaner())
    
    totaler=data.frame(table(ids))
    k_df = data.frame(table(kmeans, ids))
    k_mat = dcast(k_df, ids ~ kmeans)
    
    k_mat=k_mat[,-1]
    k_add=apply(k_mat, 2, function(x){(x/totaler$Freq)*100})
    
    pp=prcomp(k_add)
    plotter=data.frame(PC1=pp$x[,1], PC2=pp$x[,2], SampleID=totaler$ids)
    
    if(length(input$meta_val)>0){
      grouper=meta_mat()[,input$meta_val]
      
      plotter$Group=as.character(plotter$SampleID)
      samps=as.character(unique(plotter$SampleID))
      for(jj in 1:length(samps)){
        grouper=subset(meta_mat(), ID==samps[jj])
        grouper=as.character(grouper[,input$meta_val][1])
        
        plotter$Group[plotter$SampleID==samps[jj]]<-grouper
      }
    } else {
      plotter$Group=plotter$SampleID
    }
    
    PoV <- pp$sdev^2/sum(pp$sdev^2)
    
    colors_samples2=colors_samples
    if(min(table(plotter$Group))<2){
      colors_samples2=c(rep("black", length(unique(plotter$Group))))
    } 
    
    pp1=ggplot(plotter, aes(PC1, PC2, color=Group, label=SampleID)) +
      geom_point(size=4) + theme_bw() +
      geom_label_repel(size=6) +
      scale_color_manual(values=colors_samples2) +
      xlab(paste0("PC1 (Explained Variance ", round(PoV[1],4)*100, "%)")) +
      ylab(paste0("PC2 (Explained Variance ", round(PoV[2],4)*100, "%)")) +
      theme(axis.text=element_text(color='black', size=14),
            axis.title=element_text(color='black', size=16))
    
    if(min(table(plotter$Group))<2){
      pp1=pp1+theme(legend.position='none')
    } 
    
    pp_load=pp$rotation
    plotter2=data.frame(PC1=pp_load[,1], PC2=pp_load[,2], Label=rownames(pp_load))
    pp2=ggplot(plotter2, aes(PC1, PC2, color=Label, label=Label)) +
      theme_bw() +
      geom_label_repel(size=6) +
      scale_color_manual(values=colors_clusters()) +
      #xlab(paste0("PC1 (Explained Variance ", round(PoV[1],4)*100, "%)")) +
      #ylab(paste0("PC2 (Explained Variance ", round(PoV[2],4)*100, "%)")) +
      theme(axis.text=element_text(color='black', size=14),
            axis.title=element_text(color='black', size=16))
    
    vals$pca_clusters<-arrangeGrob(pp1,pp2, nrow=1)
    grid.arrange(pp1,pp2, nrow=1)
    
  })
  
  
  output$pca_download = downloadHandler(
    filename = 'PCA_plots.png',
    content = function(file) {
      png(file, width=input$download_width, height=input$download_height, units="in", res=200)
      print(
        grid.arrange(
          arrangeGrob(vals$pca_samps, vals$pca_kmeans, nrow=1),
          vals$pca_clusters, nrow=2)
      )
      dev.off()
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
      k_mat = dcast(k_df, ids ~ kmeans)
      
      k_mat=k_mat[,-1]
      k_add=apply(k_mat, 2, function(x){(x/totaler$Freq)*100})
      pp=prcomp(k_add)
      plotter=data.frame(pp$x)
      colnames(plotter)=paste0("PC", 1:ncol(plotter))
      plotter$SampleID=totaler$ids
      
      if(length(input$meta_val)>0){
        grouper=meta_mat()[,input$meta_val]
        
        plotter$Group=as.character(plotter$SampleID)
        samps=as.character(unique(plotter$SampleID))
        for(jj in 1:length(samps)){
          grouper=subset(meta_mat(), ID==samps[jj])
          grouper=as.character(grouper[,input$meta_val][1])
          
          plotter$Group[plotter$SampleID==samps[jj]]<-grouper
        }
      } else {
        plotter$Group=plotter$SampleID
      }
      write.table(plotter, file=file, row.names=F, quote=F, sep='\t')
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
      k_mat = dcast(k_df, ids ~ kmeans)
      
      k_mat=k_mat[,-1]
      k_add=apply(k_mat, 2, function(x){(x/totaler$Freq)*100})
      
      pp=prcomp(k_add)
      plotter=data.frame(pp$rotation)
      
      write.table(plotter, file=file, row.names=T, quote=F, sep='\t')
    })
  
  
  umap_coords<-reactive({
    withProgress({
      data_mat2=data_mat()[,-1]
      ids=data_mat()[,1]
      data_mat2=data.matrix(data_mat2)
      if(ncol(data_mat2)>=15){
        mnist_umap <- umap(data_mat2, pca = 15, fast_sgd = TRUE)
      } else {
        mnist_umap <- umap(data_mat2, pca = ncol(data_mat2), fast_sgd = TRUE)
      }
      colnames(mnist_umap)=c("UMAP_1", "UMAP_2")
      mnist_umap
      
    }, message="Calculating UMAP")
  })
  
  
  output$umap_plot = renderPlot({
    data_mat2=data_mat()[,-1]
    ids=data_mat()[,1]
    data_mat2=data.matrix(data_mat2)
    
    umap_df=umap_coords()
    plotter=data.frame(UMAP_1=umap_df[,1], UMAP_2=umap_df[,2], SampleID=ids)
    
    if(length(input$meta_val)>0){
      grouper=meta_mat()[,input$meta_val]
      
      plotter$Group=as.character(plotter$SampleID)
      samps=as.character(unique(plotter$SampleID))
      for(jj in 1:length(samps)){
        grouper=subset(meta_mat(), ID==samps[jj])
        grouper=as.character(grouper[,input$meta_val][1])
        
        plotter$Group[plotter$SampleID==samps[jj]]<-grouper
      }
    } else {
      plotter$Group=plotter$SampleID
    }
    
    colors_samples2=colors_samples
    #if(min(table(plotter$Group))<2){
    #  colors_samples2=c(rep("black", length(unique(plotter$Group))))
    #} 
    
    gg=ggplot(sample(plotter), aes(UMAP_1, UMAP_2, color=Group)) +
      geom_point() + theme_bw() +
      scale_color_manual(values=colors_samples2) +
      theme(axis.text=element_text(color='black', size=14),
            axis.title=element_text(color='black', size=16))
    
    vals$umap_samps<-gg
    
    print(gg)
    
  })
  
  output$cluster_setting<-renderUI({
    #if(input$clust_type=="Kmeans"){
    sliderInput("kmean",
                "Number of clusters:",
                value = 5,
                min = 1,
                max = 20)
    #} else {
    #  sliderInput("mean_shift",
    #              "Number of neighbors:",
    #              value = round(0.75*nrow(data_mat())),
    #              min = 100,
    #              max = nrow(data_mat()))
    #}
    #graph based is too slow
    #else {
    #  sliderInput("graph_k",
    #              "Number of neighbors:",
    #              value = 25,
    #              min = 5,
    #              max = 100)
    #}
    
  })
  
  kmeaner<-reactive({
    withProgress({
      data_mat2=data_mat()[,-1]
      ids=data_mat()[,1]
      data_mat2=data.matrix(data_mat2)
      if(input$clust_type=="Kmeans"){
        kmeaner=kmeans(data_mat2, input$kmean)
        kk=paste0("C", kmeaner$cluster)
      } else {
        hc <- hclust(dist(data_mat2)^2, "cen")
        memb <- cutree(hc, k = input$kmean)
        kk=paste0("C", as.character(memb))
        
        #print(dim(data_mat2))
        #res=msClustering( t(data_mat2), h=input$mean_shift)
        #kk=paste0("C", as.character(res$labels))
        
        #classification <- meanShift(data_mat2, alpha=1, iterations=5,
        #                            algorithm="KDTREE", 
        #                            nNeighbor=input$mean_shift)
        #kk=paste0("C", as.character(classification$assignment))
        
      }
      
      #else {
      #  
      #  knn.info <- RANN::nn2(data_mat2, k=input$graph_k)
      #
      #  mat=t(data_mat2)
      #  ## convert to adjacancy matrix
      #  knn <- knn.info$nn.idx
      #  adj <- matrix(0, ncol(mat), ncol(mat))
      #  rownames(adj) <- colnames(adj) <- colnames(mat)
      #  for(i in seq_len(ncol(mat))) {
      #    adj[i,colnames(mat)[knn[i,]]] <- 1
      #  }
      #
      #  ## convert to graph
      #  g <- igraph::graph.adjacency(adj, mode="undirected")
      #  g <- simplify(g) ## remove self loops
      
      #  ## identify communities
      #  km <- igraph::cluster_louvain(g)
      #  kk=paste0("C", km$membership)
      #}
      kk
    }, message = "Calculating clusters")
  })
  
  output$umap_k_plot = renderPlot({
    data_mat2=data_mat()[,-1]
    ids=data_mat()[,1]
    data_mat2=data.matrix(data_mat2)
    
    umap_df=umap_coords()
    plotter=data.frame(UMAP_1=umap_df[,1], UMAP_2=umap_df[,2], SampleID=ids)
    
    plotter$Kmeans=as.character(kmeaner())
    
    gg=ggplot(sample(plotter), aes(UMAP_1, UMAP_2, color=Kmeans)) +
      geom_point() + theme_bw() +
      scale_color_manual(values=colors_clusters()) +
      theme(axis.text=element_text(color='black', size=14),
            axis.title=element_text(color='black', size=16))
    
    vals$umap_kmeans<-gg
    
    print(gg)
  })
  
  output$samp_pca <- renderPlot({
    data_mat2=data_mat()[,-1]
    ids=data_mat()[,1]
    data_mat2=data.matrix(data_mat2)
    kmeans=as.character(kmeaner())
    
    totaler=data.frame(table(ids))
    k_df = data.frame(table(kmeans, ids))
    k_mat = dcast(k_df, ids ~ kmeans)
    
    k_mat=k_mat[,-1]
    k_add=apply(k_mat, 2, function(x){(x/totaler$Freq)*100})
    
    pp=prcomp(k_add)
    plotter=data.frame(PC1=pp$x[,1], PC2=pp$x[,2], SampleID=totaler$ids)
    
    if(length(input$meta_val)>0){
      grouper=meta_mat()[,input$meta_val]
      
      plotter$Group=as.character(plotter$SampleID)
      samps=as.character(unique(plotter$SampleID))
      for(jj in 1:length(samps)){
        grouper=subset(meta_mat(), ID==samps[jj])
        grouper=as.character(grouper[,input$meta_val][1])
        
        plotter$Group[plotter$SampleID==samps[jj]]<-grouper
      }
    } else {
      plotter$Group=plotter$SampleID
    }
    
    PoV <- pp$sdev^2/sum(pp$sdev^2)
    
    colors_samples2=colors_samples
    if(min(table(plotter$Group))<2){
      colors_samples2=c(rep("black", length(unique(plotter$Group))))
    } 
    
    pp1=ggplot(plotter, aes(PC1, PC2, color=Group, label=SampleID)) +
      geom_point(size=4) + theme_bw() +
      geom_label_repel(size=6) +
      scale_color_manual(values=colors_samples2) +
      xlab(paste0("PC1 (Explained Variance ", round(PoV[1],4)*100, "%)")) +
      ylab(paste0("PC2 (Explained Variance ", round(PoV[2],4)*100, "%)")) +
      theme(axis.text=element_text(color='black', size=14),
            axis.title=element_text(color='black', size=16))
    
    if(min(table(plotter$Group))<2){
      pp1=pp1+theme(legend.position='none')
    } 
    
    pp_load=pp$rotation
    plotter2=data.frame(PC1=pp_load[,1], PC2=pp_load[,2], Label=rownames(pp_load))
    pp2=ggplot(plotter2, aes(PC1, PC2, color=Label, label=Label)) +
      theme_bw() +
      geom_label_repel(size=6) +
      scale_color_manual(values=colors_clusters()) +
      #xlab(paste0("PC1 (Explained Variance ", round(PoV[1],4)*100, "%)")) +
      #ylab(paste0("PC2 (Explained Variance ", round(PoV[2],4)*100, "%)")) +
      theme(axis.text=element_text(color='black', size=14),
            axis.title=element_text(color='black', size=16))
    
    vals$umap_clusters<-arrangeGrob(pp1,pp2, nrow=1)
    grid.arrange(pp1,pp2, nrow=1)
    
    
  })
  
  
  output$umap_download = downloadHandler(
    filename = 'UMAP_plots.png',
    content = function(file) {
      png(file, width=input$download_width, height=input$download_height, units="in", res=200)
      print(
        grid.arrange(
          arrangeGrob(vals$umap_samps, vals$umap_kmeans, nrow=1),
          vals$umap_clusters, nrow=2)
      )
      dev.off()
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
      k_mat = dcast(k_df, ids ~ kmeans)
      
      k_mat=k_mat[,-1]
      k_add=apply(k_mat, 2, function(x){(x/totaler$Freq)*100})
      
      pp=prcomp(k_add)
      plotter=data.frame(pp$x)
      colnames(plotter)=paste0("PC", 1:ncol(plotter))
      plotter$SampleID=totaler$ids
      
      if(length(input$meta_val)>0){
        grouper=meta_mat()[,input$meta_val]
        
        plotter$Group=as.character(plotter$SampleID)
        samps=as.character(unique(plotter$SampleID))
        for(jj in 1:length(samps)){
          grouper=subset(meta_mat(), ID==samps[jj])
          grouper=as.character(grouper[,input$meta_val][1])
          
          plotter$Group[plotter$SampleID==samps[jj]]<-grouper
        }
      } else {
        plotter$Group=plotter$SampleID
      }
      write.table(plotter, file=file, row.names=F, quote=F, sep='\t')
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
      k_mat = dcast(k_df, ids ~ kmeans)
      
      k_mat=k_mat[,-1]
      k_add=apply(k_mat, 2, function(x){(x/totaler$Freq)*100})
      
      pp=prcomp(k_add)
      plotter=data.frame(pp$rotation)
      
      write.table(plotter, file=file, row.names=T, quote=F, sep='\t')
    })
  
  
  ###tsne
  
  tsne_coords<-reactive({
    withProgress({
      data_mat2=data_mat()[,-1]
      ids=data_mat()[,1]
      data_mat2=data.matrix(data_mat2)
      mat = Rtsne(data_mat2, initial_dims=15, pca=TRUE, theta=1)$Y
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
        grouper=subset(meta_mat(), ID==samps[jj])
        grouper=as.character(grouper[,input$meta_val][1])
        
        plotter$Group[plotter$SampleID==samps[jj]]<-grouper
      }
    } else {
      plotter$Group=plotter$SampleID
    }
    
    colors_samples2=colors_samples
    #if(min(table(plotter$Group))<2){
    #  colors_samples2=c(rep("black", length(unique(plotter$Group))))
    #} 
    
    gg=ggplot(sample(plotter), aes(tSNE_1, tSNE_2, color=Group)) +
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
    
    gg=ggplot(sample(plotter), aes(tSNE_1, tSNE_2, color=Kmeans)) +
      geom_point() + theme_bw() +
      scale_color_manual(values=colors_clusters()) +
      theme(axis.text=element_text(color='black', size=14),
            axis.title=element_text(color='black', size=16))
    
    vals$tsne_kmeans<-gg
    
    print(gg)
    
  })
  
  output$samp_t_pca <- renderPlot({
    data_mat2=data_mat()[,-1]
    ids=data_mat()[,1]
    data_mat2=data.matrix(data_mat2)
    kmeans=as.character(kmeaner())
    
    totaler=data.frame(table(ids))
    k_df = data.frame(table(kmeans, ids))
    k_mat = dcast(k_df, ids ~ kmeans)
    
    k_mat=k_mat[,-1]
    k_add=apply(k_mat, 2, function(x){(x/totaler$Freq)*100})
    
    pp=prcomp(k_add)
    plotter=data.frame(PC1=pp$x[,1], PC2=pp$x[,2], SampleID=totaler$ids)
    
    if(length(input$meta_val)>0){
      grouper=meta_mat()[,input$meta_val]
      
      plotter$Group=as.character(plotter$SampleID)
      samps=as.character(unique(plotter$SampleID))
      for(jj in 1:length(samps)){
        grouper=subset(meta_mat(), ID==samps[jj])
        grouper=as.character(grouper[,input$meta_val][1])
        
        plotter$Group[plotter$SampleID==samps[jj]]<-grouper
      }
    } else {
      plotter$Group=plotter$SampleID
    }
    
    PoV <- pp$sdev^2/sum(pp$sdev^2)
    
    colors_samples2=colors_samples
    if(min(table(plotter$Group))<2){
      colors_samples2=c(rep("black", length(unique(plotter$Group))))
    } 
    
    pp1=ggplot(plotter, aes(PC1, PC2, color=Group, label=SampleID)) +
      geom_point(size=4) + theme_bw() +
      geom_label_repel(size=6) +
      scale_color_manual(values=colors_samples2) +
      xlab(paste0("PC1 (Explained Variance ", round(PoV[1],4)*100, "%)")) +
      ylab(paste0("PC2 (Explained Variance ", round(PoV[2],4)*100, "%)")) +
      theme(axis.text=element_text(color='black', size=14),
            axis.title=element_text(color='black', size=16))
    
    if(min(table(plotter$Group))<2){
      pp1=pp1+theme(legend.position='none')
    } 
    
    pp_load=pp$rotation
    plotter2=data.frame(PC1=pp_load[,1], PC2=pp_load[,2], Label=rownames(pp_load))
    pp2=ggplot(plotter2, aes(PC1, PC2, color=Label, label=Label)) +
      theme_bw() +
      geom_label_repel(size=6) +
      scale_color_manual(values=colors_clusters()) +
      #xlab(paste0("PC1 (Explained Variance ", round(PoV[1],4)*100, "%)")) +
      #ylab(paste0("PC2 (Explained Variance ", round(PoV[2],4)*100, "%)")) +
      theme(axis.text=element_text(color='black', size=14),
            axis.title=element_text(color='black', size=16))
    
    vals$tsne_clusters<-arrangeGrob(pp1,pp2, nrow=1)
    grid.arrange(pp1,pp2, nrow=1)
    
    
  })
  
  output$tsne_download = downloadHandler(
    filename = 'TSNE_plots.png',
    content = function(file) {
      png(file, width=input$download_width, height=input$download_height, units="in", res=200)
      print(
        grid.arrange(
          arrangeGrob(vals$tsne_samps, vals$tsne_kmeans, nrow=1),
          vals$tsne_clusters, nrow=2)
      )
      dev.off()
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
      k_mat = dcast(k_df, ids ~ kmeans)
      
      k_mat=k_mat[,-1]
      k_add=apply(k_mat, 2, function(x){(x/totaler$Freq)*100})
      
      pp=prcomp(k_add)
      plotter=data.frame(pp$x)
      colnames(plotter)=paste0("PC", 1:ncol(plotter))
      plotter$SampleID=totaler$ids
      
      if(length(input$meta_val)>0){
        grouper=meta_mat()[,input$meta_val]
        
        plotter$Group=as.character(plotter$SampleID)
        samps=as.character(unique(plotter$SampleID))
        for(jj in 1:length(samps)){
          grouper=subset(meta_mat(), ID==samps[jj])
          grouper=as.character(grouper[,input$meta_val][1])
          
          plotter$Group[plotter$SampleID==samps[jj]]<-grouper
        }
      } else {
        plotter$Group=plotter$SampleID
      }
      write.table(plotter, file=file, row.names=F, quote=F, sep='\t')
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
      k_mat = dcast(k_df, ids ~ kmeans)
      
      k_mat=k_mat[,-1]
      k_add=apply(k_mat, 2, function(x){(x/totaler$Freq)*100})
      
      pp=prcomp(k_add)
      plotter=data.frame(pp$rotation)
      
      write.table(plotter, file=file, row.names=T, quote=F, sep='\t')
    })
  
  #
  #
  #
  
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
          grouper=subset(meta_mat(), ID==samps[jj])
          grouper=as.character(grouper[,input$meta_val][1])
          
          plotter$Group[plotter$SampleID==samps[jj]]<-grouper
        }
      } else {
        plotter$Group=plotter$SampleID
      }
      write.table(plotter, file, sep='\t', quote=F, row.names=F)
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
          grouper=subset(meta_mat(), ID==samps[jj])
          grouper=as.character(grouper[,input$meta_val][1])
          
          plotter$Group[plotter$SampleID==samps[jj]]<-grouper
        }
      } else {
        plotter$Group=plotter$SampleID
      }
      write.table(umap_coords(), file, sep='\t', quote=F, row.names=F)
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
          grouper=subset(meta_mat(), ID==samps[jj])
          grouper=as.character(grouper[,input$meta_val][1])
          
          plotter$Group[plotter$SampleID==samps[jj]]<-grouper
        }
      } else {
        plotter$Group=plotter$SampleID
      }
      write.table(tsne_coords(), file, sep='\t', quote=F, row.names=F)
    })
  
  
  
  ##composition
  
  plotter_melt <- reactive({
    data_mat2=data_mat()[,-1]
    ids=data_mat()[,1]
    data_mat2=data.matrix(data_mat2)
    kmeans=as.character(kmeaner())
    
    totaler=data.frame(table(ids))
    k_df = data.frame(table(kmeans, ids))
    k_mat = dcast(k_df, ids ~ kmeans)
    
    k_mat=k_mat[,-1]
    k_add=apply(k_mat, 2, function(x){(x/totaler$Freq)*100})
    colnames(k_add)=colnames(k_mat)
    
    plotter=data.frame(k_add, SampleID=totaler$ids)
    
    if(length(input$meta_val)>0){
      grouper=meta_mat()[,input$meta_val]
      
      plotter$Group=as.character(plotter$SampleID)
      samps=as.character(unique(plotter$SampleID))
      for(jj in 1:length(samps)){
        grouper=subset(meta_mat(), ID==samps[jj])
        grouper=as.character(grouper[,input$meta_val][1])
        
        plotter$Group[plotter$SampleID==samps[jj]]<-grouper
      }
    } else {
      plotter$Group=plotter$SampleID
    }
    
    #plotter_melt=melt(data=plotter, id.vars=c("SampleID", "Group"))
    plotter
  })
  
  output$composition_ui <- renderPlot({
    plotter=plotter_melt()
    plotter_melt=melt(data=plotter, id.vars=c("SampleID", "Group"))
    if(length(unique(plotter$Group))<nrow(plotter)){
      
      k_cols=length(unique(plotter_melt$Group))
      colors_use = c(colors_clusters()[1:k_cols], colors_samples)
      
      g1=ggplot(plotter_melt, aes(SampleID, value, fill=variable)) + 
        #geom_tile(aes(x=SampleID,y=105,fill=Group, height=5), show.legend = F) +
        geom_col()+ scale_fill_manual(values=colors_clusters()) + theme_bw() +
        guides(fill = guide_legend("Cluster")) + ylab("Cluster Percentage %") +
        theme(axis.text=element_text(color='black', size=14),
              axis.title=element_text(color='black', size=16)) +
        facet_wrap(~Group, ncol=k_cols, scales="free")
    } else {
      
      g1=ggplot(plotter_melt, aes(SampleID, value, fill=variable)) + 
        geom_col()+ scale_fill_manual(values=colors_clusters()) + theme_bw() +
        guides(fill = guide_legend("Cluster")) + ylab("Cluster Percentage %") +
        theme(axis.text=element_text(color='black', size=14),
              axis.title=element_text(color='black', size=16))
    }
    vals$comp_plot<-g1
    
    g1
    
  })
  
  output$comp_download = downloadHandler(
    filename = 'Composition_plot.pdf',
    content = function(file) {
      pdf(file, width=input$download_width, height=input$download_height)
      print(vals$comp_plot)
      dev.off()
    })
  
  output$comp_download_table = downloadHandler(
    filename = 'Composition_table.txt',
    content = function(file) {
      plotter=plotter_melt()
      plotter_melt=melt(data=plotter, id.vars=c("SampleID", "Group"))
      write.table(plotter_melt, file=file, row.names=F, sep='\t', quote=F)
    })
  
  output$click_info <- DT::renderDataTable(server = FALSE, {
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    
    plotter=plotter_melt()
    
    # if(!is.null(input$plot1_click)){
    # 
    #   if(is.null(input$plot1_click$panelvar1)){
    #     plotter=plotter[order(plotter$SampleID, decreasing=F),]
    #     click_tab=plotter[round(input$plot1_click$x,0),]
    #   } else {
    #     plotter_sub=subset(plotter, Group==input$plot1_click$panelvar1)
    #     plotter_sub=plotter_sub[order(plotter_sub$SampleID, decreasing=F),]
    #     click_tab=plotter_sub[round(input$plot1_click$x,0),]
    #   }
    #   
    #   click_tab %>% 
    #     mutate_if(is.numeric, signif, digits=2)
    #   click_tab=data.frame(SampleID=click_tab$SampleID, Group=click_tab$Group, click_tab[,1:(ncol(click_tab)-2)])
    # } 
    
    if(!is.null(input$plot1_brush)){
      
      if(is.null(input$plot1_brush$panelvar1)){
        plotter=plotter[order(plotter$SampleID, decreasing=F),]
        click_tab=plotter[round(input$plot1_brush$xmin,0):round(input$plot1_brush$xmax,0),]
      } else {
        plotter_sub=subset(plotter, Group==input$plot1_brush$panelvar1)
        plotter_sub=plotter_sub[order(plotter_sub$SampleID, decreasing=F),]
        click_tab=plotter_sub[round(input$plot1_brush$xmin,0):round(input$plot1_brush$xmax,0),]
      }
      
      #click_tab %>% 
      #  mutate_if(is.numeric, signif, digits=2)
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
  
  #output$brush_info <- renderPrint({
  #  plotter=plotter_melt()
  #  plotter_melt=melt(data=plotter, id.vars=c("SampleID", "Group"))
  #  brushedPoints(plotter_melt, input$plot1_brush)
  #})
  
  
  output$marker_heat <- renderPlot({
    withProgress({
      data_mat2=data_mat()[,-1]
      ids=data_mat()[,1]
      data_mat2=data.matrix(data_mat2)
      
      plotter=data.frame(SampleID=ids)
      
      if(length(input$meta_val)>0){
        grouper=meta_mat()[,input$meta_val]
        
        plotter$Group=as.character(plotter$SampleID)
        samps=as.character(unique(plotter$SampleID))
        for(jj in 1:length(samps)){
          grouper=subset(meta_mat(), ID==samps[jj])
          grouper=as.character(grouper[,input$meta_val][1])
          
          plotter$Group[plotter$SampleID==samps[jj]]<-grouper
        }
      } else {
        plotter$Group=plotter$SampleID
      }
      
      plotter$Kmeans=as.character(kmeaner())
      
      samp_samp=sample(1:nrow(plotter), 500)
      
      plotter_sub=plotter[samp_samp,]
      data_mat2_sub=data_mat2[samp_samp,]
      
      print(dim(plotter_sub))
      
      if(length(unique(plotter$Group))<length(unique(plotter$SampleID))){
        colorer2=c()
        for(j in 1:length(plotter_sub$Group)){
          colorer2[j]=colors_samples[j]
          names(colorer2)[j]=plotter_sub$Group[j]
        }
        ha=columnAnnotation(Group=plotter_sub$Group, col=list(Group=colorer2))
        
        h1=grid.grabExpr(draw(
          Heatmap(scale(t(data_mat2_sub)), show_row_names = T, show_column_names = F,
                  top_annotation = ha,
                  heatmap_legend_param = list(title = "Scaled Value"),
                  cluster_rows = T, cluster_columns = T, row_names_side = 'left',
                  column_names_gp = gpar(fontsize=7),
                  row_names_gp = gpar(fontsize=10),
                  row_title_gp = gpar(fontsize = 10),
                  row_names_max_width = unit(10,'cm'),
                  use_raster = T,
                  column_split=plotter_sub$Kmeans,
                  cluster_column_slices=F,
                  #column_split = splitter,
                  #left_annotation = ha,
                  col = colorRamp2(c(-2, 0, 2), c("blue", "white", "red")))
        ))
        
      } else {
        
        h1=grid.grabExpr(draw(
          Heatmap(scale(t(data_mat2_sub)), show_row_names = T, show_column_names = F,
                  #top_annotation = ha,
                  heatmap_legend_param = list(title = "Scaled Value"),
                  cluster_rows = T, cluster_columns = T, row_names_side = 'left',
                  column_names_gp = gpar(fontsize=7),
                  row_names_gp = gpar(fontsize=10),
                  row_title_gp = gpar(fontsize = 10),
                  row_names_max_width = unit(10,'cm'),
                  use_raster = T,
                  column_split=plotter_sub$Kmeans,
                  cluster_column_slices=F,
                  #column_split = splitter,
                  #left_annotation = ha,
                  col = colorRamp2(c(-2, 0, 2), c("blue", "white", "red")))
        ))
      }
      vals$marker_heat<-h1
      
      if(length(unique(plotter$Group))<length(unique(plotter$SampleID))){
        colorer2=c()
        for(j in 1:length(plotter_sub$Group)){
          colorer2[j]=colors_samples[j]
          names(colorer2)[j]=plotter_sub$Group[j]
        }
        ha=columnAnnotation(Group=plotter_sub$Group, col=list(Group=colorer2))
        
        Heatmap(scale(t(data_mat2_sub)), show_row_names = T, show_column_names = F,
                top_annotation = ha,
                heatmap_legend_param = list(title = "Scaled Value"),
                cluster_rows = T, cluster_columns = T, row_names_side = 'left',
                column_names_gp = gpar(fontsize=7),
                row_names_gp = gpar(fontsize=10),
                row_title_gp = gpar(fontsize = 10),
                row_names_max_width = unit(10,'cm'),
                use_raster = T,
                column_split=plotter_sub$Kmeans,
                cluster_column_slices=F,
                #column_split = splitter,
                #left_annotation = ha,
                col = colorRamp2(c(-2, 0, 2), c("blue", "white", "red")))
      } else {
        Heatmap(scale(t(data_mat2_sub)), show_row_names = T, show_column_names = F,
                #top_annotation = ha,
                heatmap_legend_param = list(title = "Scaled Value"),
                cluster_rows = T, cluster_columns = T, row_names_side = 'left',
                column_names_gp = gpar(fontsize=7),
                row_names_gp = gpar(fontsize=10),
                row_title_gp = gpar(fontsize = 10),
                row_names_max_width = unit(10,'cm'),
                use_raster = T,
                column_split=plotter_sub$Kmeans,
                cluster_column_slices=F,
                #column_split = splitter,
                #left_annotation = ha,
                col = colorRamp2(c(-2, 0, 2), c("blue", "white", "red")))
      }
    }, message="Generating heatmap")
  })
  
  output$heat_download = downloadHandler(
    filename = 'marker_heatmap.pdf',
    content = function(file) {
      pdf(file, width=input$download_width, height=input$download_height)
      print(grid.arrange(vals$marker_heat))
      dev.off()
    })
  
  
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
          g1=ggplot(sample(plotter), aes(UMAP_1, UMAP_2, z=Feature)) +
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
          ggplot(sample(plotter), aes(UMAP_1, UMAP_2, z=Feature)) +
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
  
  output$comp_feat_download = downloadHandler(
    filename = 'Composition_plot_features.pdf',
    content = function(file) {
      pdf(file, width=input$download_width, height=input$download_height)
      print(plot_grid(plotlist=glist(), ncol=3))
      dev.off()
    })
  
  
  de_stats <- reactive({
    withProgress({
      data_mat2=data_mat()[,-1]
      ids=data_mat()[,1]
      data_mat2=data.matrix(data_mat2)
      kmeans=as.character(kmeaner())
      
      full_res=data.frame(Feature=NA, avgLogFC=NA, p_val=NA, adj_p_val=NA, Kmean=NA)
      ks=unique(kmeans)
      for(jj in 1:length(ks)){
        cells.1=which(kmeans==ks[jj])
        cells.2=which(kmeans!=ks[jj])
        data.use=t(data_mat2)
        res=WilcoxDETest2(data.use, cells.1, cells.2)
        res$Kmean=ks[jj]
        full_res=rbind(full_res, res)
      }
      full_res=full_res[-1,]
      full_res=full_res[order(full_res$avgLogFC, decreasing=T),]
      full_res=full_res[order(full_res$adj_p_val, decreasing=F),]
      full_res
    }, message="Calculating DE stats")
  })
  
  output$de_feats <- renderDT({
    de_stats()
  })
  
  output$diff_volc <- renderPlot({
    feat_df = subset(de_stats(), Kmean==input$k_val)
    good_feats=subset(feat_df, avgLogFC > input$log_val & adj_p_val < input$p_val)
    okay_feats=subset(feat_df, avgLogFC < input$log_val & adj_p_val > input$p_val)
    
    data_mat2=data_mat()[,-1]
    ids=data_mat()[,1]
    data_mat2=data.matrix(data_mat2)
    
    ggplot(feat_df, aes(avgLogFC, log10(adj_p_val+1))) + 
      geom_point(color='grey85', size=3) +
      scale_y_reverse() + ylab("-log10 adjusted p-value") +
      geom_label_repel(data=good_feats, aes(avgLogFC, log10(adj_p_val+1), label=Feature)) +
      geom_point(data=good_feats, aes(avgLogFC, log10(adj_p_val+1)), color='darkorange1', size=3) +
      geom_vline(xintercept = input$log_val, color="darkorange1") +
      geom_hline(yintercept = log10(input$p_val+1), color="darkorange1") +
      theme_bw() + 
      theme(axis.text=element_text(color='black', size=14),
            axis.title=element_text(color='black', size=16))
  })
  
  output$diff_heat <- renderPlot({
    feat_df = subset(de_stats(), Kmean==input$k_val)
    good_feats=subset(feat_df, avgLogFC > input$log_val & adj_p_val < input$p_val)
    okay_feats=subset(feat_df, avgLogFC < input$log_val & adj_p_val > input$p_val)
    
    data_mat2=data_mat()[,-1]
    ids=data_mat()[,1]
    data_mat2=data.matrix(data_mat2)
    
    heat_data=data_mat2[,as.character(good_feats$Feature)]
    
    plotter=data.frame(SampleID=ids, Kmeans=as.character(kmeaner()))
    
    if(length(input$meta_val)>0){
      grouper=meta_mat()[,input$meta_val]
      
      plotter$Group=as.character(plotter$SampleID)
      samps=as.character(unique(plotter$SampleID))
      for(jj in 1:length(samps)){
        grouper=subset(meta_mat(), ID==samps[jj])
        grouper=as.character(grouper[,input$meta_val][1])
        
        plotter$Group[plotter$SampleID==samps[jj]]<-grouper
      }
    } else {
      plotter$Group=plotter$SampleID
    }
    
    
    
    
    h_agg=aggregate(heat_data, by=list(plotter$Group, plotter$Kmeans), "mean")
    colnames(h_agg)[1:2] <- c("Group", "Kmeans")
    h_mat = data.matrix(h_agg[,3:ncol(h_agg)])
    h_mat=t(scale(t(h_mat)))
    h_mat[is.na(h_mat)]<-0
    
    if(input$group_sel=="Group"){
      colorer=c()
      for(k in 1:length(h_agg[,"Kmeans"])){
        colorer[k]=colors_clusters()[k]
        names(colorer)[k]=h_agg[,"Kmeans"][k]
      }
      ha=columnAnnotation(Kmeans=h_agg[,"Kmeans"],
                          col=list(Kmeans=colorer))
      splitter=h_agg$Group
    } else {
      colorer2=c()
      for(j in 1:length(h_agg[,"Group"])){
        colorer2[j]=colors_samples[j]
        names(colorer2)[j]=h_agg[,"Group"][j]
      }
      ha=columnAnnotation(Group=h_agg[,"Group"],
                          col=list(Group=colorer2))
      splitter=h_agg$Kmeans
    }
    
    
    Heatmap(t(h_mat), show_row_names = T, show_column_names = F,
            top_annotation = ha,
            heatmap_legend_param = list(title = "Scaled Value"),
            cluster_rows = T, cluster_columns = T, #row_names_side = 'left',
            column_names_gp = gpar(fontsize=12),
            row_title_gp = gpar(fontsize = 10),
            row_names_max_width = unit(10,'cm'),
            use_raster = T,
            #cluster_row_slices=F,
            column_split = splitter,
            #left_annotation = ha,
            col = colorRamp2(c(-2, 0, 2), c("blue", "white", "red")))
    
    
  })
  
  WilcoxDETest2 <- function(
    data.use,
    cells.1,
    cells.2,
    verbose = TRUE,
    ...
  ) {
    data.use <- data.use[, c(cells.1, cells.2), drop = FALSE]
    g=c(rep(0,length(cells.1)), rep(1, length(cells.2)))
    
    p_val <- sapply(
      X = 1:nrow(x = data.use),
      FUN = function(x) {
        #return(min(2 * min(limma::rankSumTestWithCorrelation(index = j, statistics = data.use[x, ])), 1))
        return(wilcox.test(data.use[x,cells.1], data.use[x,cells.2])$p.value)
      }
    )
    
    foldB=rowMeans(data.use[,cells.2])
    foldA=rowMeans(data.use[,cells.1])
    avgLogFC=(foldB-foldA)/(foldA)
    adj_p_val=p.adjust(p_val, method="BH")
    return(data.frame(Feature=rownames(x = data.use), avgLogFC, p_val, adj_p_val))
    
  }
  
  
}

