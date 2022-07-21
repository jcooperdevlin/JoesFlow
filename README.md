The goal of JoesFlow is to analyze high-dimensional single cell data
from flow cytometry, scRNA-seq, CITE-seq and any kind of single cell
matrix data. JoesFlow utilizes novel scRNA-seq dimension reduction
techniques to generate interpretable and informative visualizations that
incorporate all aspects of a dataset in an unbiased manner.

## JoesFlow Installation

### Installation via Docker

The recommended method for running JoesFlow is using Docker, because the
images provided have all system dependencies and R packages.

-   [Install Docker on your
    system.](https://docs.docker.com/get-docker/) This will require
    admin-level access. Using the WSL 2 backend is recommended for
    Windows computers.
-   Open Docker on your computer.
-   Mac OS
    -   Open Terminal.app
-   Windows
    -   Open the Windows PowerShell
-   Start JoesFlow with the following command:
    `docker run --user shiny --rm -ti -p 3838:3838 johnsonra/joes-flow`
    -   The first time you run this will take several minutes, as it
        will have to download the image to your computer. The next time
        you start it up should only take a few seconds.
    -   Update your local image (e.g. to update to a new version) with
        the following command: `docker pull johnsonra/joes-flow`
-   Once JoesFlow is running, open this page in your favorite browser:
    <http://localhost:3838/JoesFlow>
-   To shut down the Docker image when finished, you can enter the
    command `control-c` in the Terminal/PowerShell window and close it.

### Installation via RStudio

JoesFlow has a lot of dependencies, so in order to install locally in
RStudio, you may need to install additional system tools and R packages
for full functionality.

    remotes::install_github("NIAID/JoesFlow")

Once installed, the Shiny app can be started up as follows:

    library(JoesFlow)
    run_app()
    #> Loading required package: shiny
    #> 
    #> Listening on http://127.0.0.1:4322

![Joe’s Flow Screenshot](README_files/setup-1.png)

## JoesFlow Functions

    sample_data=read.csv("tests/flow_test.csv", sep=',', header=T)
    meta_data=read.csv("tests/metadata.csv", header=T, sep=',')

    datatable(sample_data[1:100,1:20])

![sample\_data table](README_files/test_data-1.png)

    datatable(meta_data)

![meta\_data table](README_files/test_data-2.png)

Convert `meta_data` to named vector as expected by JoesFlow. This is
taken care of automatically when loading meta data through the shiny
app, assuming the first column contains IDs, and the second column
contains Group designations.

    meta <- meta_data[,2]
    names(meta) <- meta_data[,1]

### PCA figures


    # PCA cluster
    pp <- select(sample_data, -SampleID) %>%
      prcomp(scale=T)
      
    pc1 <- clusterJF(pp, ids = sample_data$SampleID, 
                meta = meta_data$Group, 
                colors = colors_samples)
     
    # Kmeans cluster
    set.seed(23948)
    kmeans_groups <- select(sample_data, -SampleID) %>%
      kmeans(10) %$% cluster %>%
      {paste0('C', .)} # add a 'C' on the front of each group
    names(kmeans_groups) <- sample_data$SampleID
      
    pc2 <- clusterJF(pp, ids = 1:nrow(sample_data),
                     meta = kmeans_groups,
                     colors = colors_clusters,
                     legend.name = 'Kmeans')

    grid.arrange(pc1, pc2, nrow=1)

<img src="README_files/figure-markdown_strict/pca-1.png" style="display: block; margin: auto;" />

### UMAP figures


    # Umap
    mnist_umap <- select(sample_data, -SampleID) %>%
      umap(pca = 15, fast_sgd = TRUE)

    um1 <- clusterJF(mnist_umap, ids = sample_data$SampleID,
                     meta = meta_data$Group,
                     colors = colors_samples)

    um2 <- clusterJF(mnist_umap, ids = 1:nrow(sample_data),
                     meta = kmeans_groups,
                     colors = colors_clusters,
                     legend.name = 'Kmeans')

    grid.arrange(um1, um2, nrow=1)

<img src="README_files/figure-markdown_strict/umap-1.png" style="display: block; margin: auto;" />

### Sample-based PCA

    # sample-based pca
    groups_table <- with(sample_data, table(SampleID, kmeans_groups))

    pp <- apply(groups_table, 2, function(x) x / rowSums(groups_table)) %>%
      prcomp()

    gg3 <- sb_clusterJF(pp, ids = rownames(groups_table),
                        meta = meta_data$Group,
                        colors1 = colors_samples,
                        colors2 = colors_clusters)

    gg3

<img src="README_files/figure-markdown_strict/sample based pca-1.png" style="display: block; margin: auto;" />

### Composition plot

    compositionJF(meta, kmeans_groups, colors_clusters)$g1

<img src="README_files/figure-markdown_strict/comp_plot-1.png" style="display: block; margin: auto;" />

![data table](README_files/comp_plot-2.png)

### Heat plot

    # marker heat plot
    select(sample_data, -SampleID) %>%
      marker_heatJF(ids = sample_data$SampleID,
                    meta = meta_data$Group,
                    kmeans_groups = kmeans_groups,
                    colors = colors_samples,
                    sample_size = 500)

<img src="README_files/figure-markdown_strict/heat_plot-1.png" style="display: block; margin: auto;" />
