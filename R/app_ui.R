#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 
#' 

loadingLogo <- function(href, src, loadingsrc, height = NULL, width = NULL, alt = NULL) {
  tagList(
    tags$head(
      tags$script(
        "setInterval(function(){
        if ($('html').attr('class')=='shiny-busy') {
        $('div.busy').show();
        $('div.notbusy').hide();
        } else {
        $('div.busy').hide();
        $('div.notbusy').show();
        }
},100)")
    ),
    tags$a(href=href,
           div(class = "busy",
               img(src=loadingsrc,height = height, width = width, alt = alt)),
           div(class = 'notbusy',
               img(src = src, height = height, width = width, alt = alt))
    )
  )
}

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    navbarPage(title = "Joe's Flow", 
               tabPanel("Upload",
                        sidebarPanel(
                          fileInput("file1", "Choose Flow File",
                                    accept = c(
                                      "text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")
                          ),
                          fileInput("file2", "Choose Metadata File",
                                    accept = c(
                                      "text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")
                                    
                                    
                          ),
                          sliderInput("subsample",
                                      "How much subsampling?",
                                      value = 0.2,
                                      min = 0,
                                      max = 1)
                        ),
                        mainPanel(
                          #tags$head(tags$style(
                          #  "#contents{color: #df691a; font-size: 18px}",
                          #  "#DataTables_Table_0_filter{color: #df691a; font-size: 18px}",
                          #  "#DataTables_Table_0_info{color: #df691a; font-size: 18px}",
                          #  "#DataTables_Table_0_length{color: #df691a; font-size: 18px}",
                          #  "#DataTables_Table_0_paginate{color: #df691a; font-size: 18px}")),
                          DT::DTOutput('contents'),
                          DT::DTOutput('metadata')
                        )
               ),
               tabPanel("Visualize",
                        sidebarPanel(
                          uiOutput("meta_sel"),
                          
                          #fluidRow(column(4,
                          #                numericInput("mean_cutoff", label = "Mean Cutoff",
                          #                             min=0,max=Inf, step = 1000, value = 0),
                          #                numericInput("SD_cutoff", label = "SD Cutoff",
                          #                             min=0,max=Inf, step = 50, value = 0),
                          #                numericInput("var_cutoff", label = "Variance Cutoff",
                          #                             min=0,max=Inf, step = 1000, value = 0)
                          #                )),
                          
                          
                          br(),
                          
                          # Input: Slider for the number of observations to generate ----
                          selectInput("clust_type", label = "Select Clustering Method",
                                      choices=c("Kmeans", "Hierarchical"), selected="Kmeans"),
                          uiOutput("cluster_setting"),
                          uiOutput("col_pal"),
                          
                          fluidRow(downloadButton('pca_coord_download', label = "PCA coords")),
                          fluidRow(downloadButton('umap_coord_download', label = "UMAP coords")),
                          fluidRow(downloadButton('tsne_coord_download', label = "tSNE coords")),
                          fluidRow(column(6, numericInput("download_height", "Download Height", 
                                                          min=1, max=50, step=1, value=10)),
                                   column(6, numericInput("download_width", "Download Width", 
                                                          min=1, max=50, step=1, value=15)))
                          
                        ),
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel("Features", 
                                               fluidRow(column(6,
                                                               plotOutput("feats_plot")),
                                                        column(6, 
                                                               plotOutput("sample_var"))),
                                               fluidRow(column(6, downloadButton('feat_download')))#,
                                               #plotOutput("feats_distr")
                                      ),
                                      tabPanel("PCA",
                                               fluidRow(column(6,
                                                               plotOutput("pca_plot")),
                                                        column(6,
                                                               plotOutput("pca_k_plot"))),
                                               fluidRow(column(12,
                                                               plotOutput("samp_p_pca"))),
                                               fluidRow(column(6,
                                                               downloadButton("pca_download", label='Download Plot'),
                                                               downloadButton("pca_download_vals", label='Download Values')),
                                                        column(6, downloadButton("pca_download_loading", label='Download Loadings'))
                                               )
                                      ),
                                      tabPanel("UMAP", 
                                               fluidRow(column(6,
                                                               plotOutput("umap_plot")),
                                                        column(6,
                                                               plotOutput("umap_k_plot"))),
                                               fluidRow(column(12,
                                                               plotOutput("samp_pca")
                                               )),
                                               fluidRow(column(6,
                                                               downloadButton("umap_download", label='Download Plot'),
                                                               downloadButton("umap_download_vals", label='Download Values')),
                                                        column(6, downloadButton("umap_download_loading", label='Download Loadings'))
                                               )
                                      ),
                                      tabPanel("TSNE",
                                               fluidRow(column(6,
                                                               plotOutput("tsne_plot")),
                                                        column(6,
                                                               plotOutput("tsne_k_plot"))),
                                               fluidRow(column(12,
                                                               plotOutput("samp_t_pca"))),
                                               fluidRow(column(6,
                                                               downloadButton("tsne_download", label='Download Plot'),
                                                               downloadButton("tsne_download_vals", label='Download Values')),
                                                        column(6, downloadButton("tsne_download_loading", label='Download Loadings'))
                                               )
                                      ),
                                      tabPanel("Composition",
                                               fluidRow(
                                                 column(width = 12,
                                                        h4("Click & Drag for plot details"),
                                                        DT::dataTableOutput("click_info")
                                                 )#,
                                                 #column(width = 0,
                                                 #        h4("Brushed points"),
                                                 #        verbatimTextOutput("brush_info")
                                                 # )
                                               ),
                                               fluidRow(column(12,
                                                               plotOutput("composition_ui",
                                                                          click = "plot1_click",
                                                                          brush = brushOpts(id = "plot1_brush")))),
                                               fluidRow(
                                                 column(6, downloadButton('comp_download', label="Download Plot")),
                                                 column(6, downloadButton('comp_download_table', label="Download Table"))
                                               ),
                                               fluidRow(column(12, selectizeInput("feat_dim", "Select Dimension Reduction",
                                                                                  choices=c(" ", "PCA", "UMAP", "tSNE"), selected=" "))),
                                               fluidRow(column(12, uiOutput("comp_ui"))),
                                               fluidRow(column(6, downloadButton('comp_feat_download')))
                                               #fluidRow(column(6, uiOutput("comp_ui")),
                                               #          column(6, uiOutput(("comp_plots"))))
                                      ),
                                      tabPanel("Markers",
                                               fluidRow(column(12,
                                                               plotOutput("marker_heat", height = "700px"))
                                               ),
                                               fluidRow(column(6, downloadButton('heat_download')))
                                      )
                                      #tabPanel("Markers",
                                      #         fluidRow(column(12,
                                      #                         DTOutput("de_feats"))),
                                      #         fluidRow(
                                      #           column(width=6,
                                      #                  fluidRow(column(6, uiOutput("select_k")),
                                      #                           column(6, uiOutput("select_p"))),
                                      #                  fluidRow(column(6, selectInput('group_sel', "Split Columns",
                                      #                                                 choices=c("Kmeans", "Group"),
                                      #                                                 selected="Kmeans")),
                                      #                           column(6, uiOutput("select_l")))
                                      #                  ),
                                      #           column(width = 6,
                                      #                  fluidRow(column(12, plotOutput("diff_volc")))
                                      #           )),
                                      #         fluidRow(column(12, plotOutput("diff_heat", height = "700px")))
                                      #)
                          )
                        )
               ),
               tabPanel(HTML(" </a></li><li><a href=\"https://www.niaid.nih.gov/research/png-loke-phd\">Type 2 Immunity Section, Laboratory of Parasitic Diseases, NIAID")),
               #tags$script(HTML("var header = $('.navbar> .container-fluid');
               #             header.append('<a <div style=\"float:right;color:#df691a\"><h3>https://www.niaid.nih.gov/research/png-loke-phd</h3><div></a>');
               #                  console.log(header)")),
               tags$style(HTML(".navbar-default .navbar-brand {color: #df691a; font-size:28}",
                               ".navbar-default .navbar-brand:hover {color: #df691a;}",
                               '.navbar-header {font-family: "Brush Script MT"}',
                               '.navbar-brand { font-size: 48px}',
                               '.navbar-nav > li > a, .navbar-brand {
                              padding-top:30px !important; 
                            padding-bottom:0 !important;
                            height: 80px;
                           }',
                               "
                           .navbar-nav {
                           float: none !important;
                           }
                           .navbar-nav > li:nth-child(4) {
                           float: right;
                           right: 150px;
                           }.",
                               'h3, .h3 { font-size: 12px}',
                               '.navbar {min-height:80px !important;}',
                               ".navbar { background-color: #2c3e4f;}",
                               ".navbar-default .navbar-nav > li > a {color:#df691a;}",
                               ".navbar-default .navbar-nav > .active > a,",
                               ".navbar-default .navbar-nav > .active > a:focus,",
                               ".navbar-default .navbar-nav > .active > a:hover {color: white;font-size:18px;background-color:#2c3e4f}",
                               ".navbar-default .navbar-nav > li > a:hover {color: white;background-color:#2c3e4f;text-decoration:underline;}",
                               ".navbar-default .navbar-nav > li > a[data-value='t1'] {color: red;background-color: pink;}",
                               ".navbar-default .navbar-nav > li > a[data-value='t2'] {color: blue;background-color: lightblue;}",
                               ".navbar-default .navbar-nav > li > a[data-value='t3'] {color: green;background-color: lightgreen;}"
               ))
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'JoesFlow'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

