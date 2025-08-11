# Use the official R base image
FROM rocker/shiny:latest

# Install additional system dependencies if needed
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev

# Install R packages required for your Shiny app
RUN R -e "install.packages(c('shiny', 'bsicons', 'bslib', 'bsplus', 'caret', 'cluster', 'colorspace', 'corrplot', 'DBI', 'devtools', 'dplyr', 'future', 'ggdist', 'gghalves', 'ggplot2', 'ggpmisc', 'ggtext', 'glmnet', 'glmnetUtils', 'grid', 'gridExtra', 'hash', 'Hmisc', 'hrbrthemes', 'htmltools', 'ipc', 'isotree', 'leaflet', 'lime', 'lubridate', 'magrittr', 'Metrics', 'missForest', 'Nmisc', 'openxlsx', 'permimp', 'pdp', 'plotly', 'plyr', 'png', 'promises', 'pso', 'ragg', 'RColorBrewer', 'RSQLite', 'reshape2', 'reactable', 'readxl', 'rsample', 'SHAPforxgboost', 'shiny', 'shinybusy', 'shinydashboard', 'shinydashboardPlus', 'shinyjs', 'shinythemes', 'stats', 'tidymodels', 'tidyr', 'tidyverse', 'units', 'xgboost', 'DT'), repos='https://cran.rstudio.com/')"

# Copy your Shiny app to the Docker image
COPY ./app /srv/shiny-server/

# Expose the Shiny app port
EXPOSE 3838

# Start the Shiny server
CMD ["/usr/bin/shiny-server"]