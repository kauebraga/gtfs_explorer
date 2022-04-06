FROM rocker/shiny:latest
# Install system requirements for index.R as needed
RUN apt-get update && apt-get install -y \
    --no-install-recommends \
    git-core \
    libssl-dev \
    libcurl4-gnutls-dev \
    curl \
    libsodium-dev \
    libxml2-dev \
    libicu-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libglpk40 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*
# ENV _R_SHLIB_STRIP_=true
# COPY Rprofile.site /etc/R
RUN install2.r --error --skipinstalled \
    -r 'http://cran.rstudio.com' \
    gtfstools \
    leaflet \
    leafgl \
    shinydashboard \
    data.table \
    waiter \
    highcharter \
    shinyWidgets \
    shiny \
    shinyBS \
    ggplot2 \
    htmltools \
    lwgeom \
    sf 
COPY ./app/. /srv/shiny-server/
# USER shiny
EXPOSE 3838


# upload: https://www.section.io/engineering-education/docker-push-for-publishing-images-to-docker-hub/