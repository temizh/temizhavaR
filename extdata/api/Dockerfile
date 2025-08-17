FROM rocker/r-ver:4.3.1

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev

RUN R -e "install.packages(c('RestRserve'))"

COPY . /app
WORKDIR /app

EXPOSE 8000

CMD ["Rscript", "main.R"]