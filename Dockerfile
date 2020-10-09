FROM rocker/shiny-verse

MAINTAINER Thomas Hansen "tbh@mbg.au.dk"

RUN R -e "install.packages(c('plotly', 'shinydashboard', 'data.table', 'minpack.lm', 'DT', 'RCurl'), repos='http://cran.rstudio.com/')"

COPY corona_dk /srv/shiny-server/corona_dk
COPY corona_ww /srv/shiny-server/corona_ww

RUN chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838