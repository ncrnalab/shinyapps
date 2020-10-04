FROM r-base:latest

MAINTAINER Thomas Hansen "tbh@mbg.au.dk"

RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libxt-dev \
    libssl-dev \
    libxml2 \
    libxml2-dev

# Download and install shiny server
RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb

RUN R -e "install.packages(c('Rcpp', 'shiny', 'rmarkdown', 'plotly', 'tidyverse', 'shinydashboard'), repos='http://cran.rstudio.com/')"


COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf
COPY /corona_dk /srv/shiny-server/
#COPY /corona_ww /srv/shiny-server/

EXPOSE 80

COPY shiny-server.sh /usr/bin/shiny-server.sh

RUN chmod +x /usr/bin/shiny-server.sh 

CMD ["/usr/bin/shiny-server.sh"]