# JoesFlow Docker file
# https://github.com/IDSS-NIAID/JoesFlow

# start with shiny server and R
FROM rocker/shiny:4.1.2

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
    "shinythemes"))'

RUN R -e 'BiocManager::install("ComplexHeatmap", update=FALSE)'

# install JoesFlow
#RUN R -e 'devtools::install_github("IDSS-NIAID/JoesFlow", upgrade="never", dependences = TRUE)'
RUN wget https://raw.githubusercontent.com/IDSS-NIAID/JoesFlow/main/R/app_server.R \ 
         https://raw.githubusercontent.com/IDSS-NIAID/JoesFlow/main/R/app_ui.R
RUN mkdir /srv/shiny-server/JoesFlow
RUN mv app_server.R /srv/shiny-server/JoesFlow/server.R
RUN mv app_ui.R /srv/shiny-server/JoesFlow/ui.R

# run app
CMD ["/usr/bin/shiny-server"]
