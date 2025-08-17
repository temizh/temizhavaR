FROM rocker/r-ver:4.3.1

RUN R -e "install.packages(c('websocket', 'httpuv'))"

COPY . /app
WORKDIR /app

EXPOSE 9000

CMD ["Rscript", "websocket_server.R"]
