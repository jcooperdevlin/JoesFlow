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
  libmagick++-dev

# install R packge dependencies
RUN R -e 'install.packages(\
  c("BiocManager",\
    "devtools",\
    "magick",\
    "RSpectra",\
    "shinythemes"),\
  repos="https://packagemanager.rstudio.com/all/2022-07-14+Y3JhbiwyOjQ1MjYyMTU7QzczRDEwMEE")'
  
RUN R -e 'BiocManager::install("ComplexHeatmap", update=FALSE)'

# install JoesFlow
RUN R -e 'devtools::install_github("IDSS-NIAID/JoesFlow", upgrade="never", dependences = TRUE)'
RUN wget https://raw.githubusercontent.com/NIAID/JoesFlow/docker/shiny/server.R \ 
         https://raw.githubusercontent.com/NIAID/JoesFlow/docker/shiny/ui.R
RUN mkdir /srv/shiny-server/JoesFlow
RUN mv *.R /srv/shiny-server/JoesFlow/.

# run app
CMD ["/usr/bin/shiny-server"]
