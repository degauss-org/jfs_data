FROM rocker/r-ver:4.1.2

EXPOSE 3838

WORKDIR /app

RUN R -e "install.packages('shiny')"
RUN R -e "install.packages('tidyr')"
RUN R -e "install.packages('readr')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('rclipboard')"
RUN R -e "install.packages('glue')"
RUN R -e "install.packages('stringr')"
RUN R -e "install.packages('purrr')"


COPY app.R .

ENTRYPOINT ["R", "-e", "shiny::runApp(port = 3838, host = '0.0.0.0')"]