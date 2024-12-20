FROM rocker/geospatial:latest

WORKDIR /code

RUN install2.r --error \
    shiny \
    dplyr \
    ggplot2 \
    readr \
    ggExtra \
    mapgl \
    duckdbfs

COPY . .

CMD ["R", "--quiet", "-e", "shiny::runApp(host='0.0.0.0', port=7860)"]
