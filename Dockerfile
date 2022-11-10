# JoesFlow Docker file
# https://github.com/IDSS-NIAID/JoesFlow

# start with shiny server and R
FROM rocker/shiny:4.2.1

# install system dependencies
RUN apt-get update && apt-get install -y \
  libcurl4-gnutls-dev \
  libssl-dev \
  libxml2-dev \
  libfontconfig1-dev \
  libcairo2-dev \
  libmagick++-dev \
  libharfbuzz-dev \
  libfribidi-dev

# install R packge dependencies
RUN R -e 'install.packages(\
  c("BiocManager",\
    "devtools",\
    "magick",\
    "RSpectra",\
    "shinythemes"),\
  repos="https://packagemanager.rstudio.com/all/2022-11-08+Y3JhbiwyOjQ1MjYyMTU7NDcyMTMxRQ")'

RUN R -e 'BiocManager::install("ComplexHeatmap", update=FALSE)'

# shouldn't *need* these, but it helps to cache the installs, rather than need to install them every time we use `devtools::install` below
RUN R -e 'install.packages(\
  c("patchwork",\
    "hexbin",\
    "circlize",\
    "cowplot",\
    "DT",\
    "fastcluster",\
    "ggrepel",\
    "ggsci",\
    "gridExtra",\
    "RColorBrewer",\
    "reshape2",\
    "Rtsne",\
    "stringi",\
    "tidyr",\
    "uwot"),\
  repos="https://packagemanager.rstudio.com/all/2022-11-08+Y3JhbiwyOjQ1MjYyMTU7NDcyMTMxRQ")'

# copy R package to image
RUN mkdir JoesFlow JoesFlow/man JoesFlow/R
COPY DESCRIPTION LICENSE NAMESPACE JoesFlow/.
COPY R/* JoesFlow/R/.
COPY man/* JoesFlow/man/.

# install JoesFlow
RUN mkdir /srv/shiny-server/JoesFlow
COPY shiny/* /srv/shiny-server/JoesFlow/.

RUN R -e 'devtools::install("JoesFlow", dependencies = FALSE)'

# run app
CMD ["/usr/bin/shiny-server"]
