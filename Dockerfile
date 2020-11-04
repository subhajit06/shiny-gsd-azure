# Use rocker/r-ver as the base image
# This will inherit everyhing that was installed into base image already
# Documented at https://hub.docker.com/r/rocker/r-ver/~/dockerfile/
FROM rocker/shiny:latest

# Install any dependencies required for the R packages
RUN  apt-get update \
  && apt-get install -y --no-install-recommends \
  libxml2-dev \
  libcurl4-openssl-dev \
  libssl-dev \
  libv8-dev \
  xdg-utils	

# Install the R Packages from CRAN
RUN Rscript -e 'install.packages(c("jsonlite", "AzureQstor", "ggplot2", "gsDesign"))'
RUN Rscript -e 'install.packages(c("jsonvalidate","shinydashboard","readr"))'

#RUN mkdir /home/test_shiny_gsDesign
#RUN mkdir /home/test_shiny_gsDesign/json

#COPY queueLib.R /home/test_shiny_gsDesign/queueLib.R
#COPY UI_gsDesign.R /home/test_shiny_gsDesign/UI_gsDesign.R 

#WORKDIR /home/test_shiny_gsDesign/

#EXPOSE 29579
RUN mkdir /usr/local/src/app/
RUN mkdir /usr/local/src/app/json

COPY . /usr/local/src/app
COPY ./json  /usr/local/src/app/json
#set working directory to the app
WORKDIR /usr/local/src/app

CMD ["Rscript", "UI_gsDesign.R"] 

#ENTRYPOINT ["R"]
