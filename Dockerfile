FROM rocker/geospatial:latest

WORKDIR /code

RUN install2.r --error \
    bslib \
    shiny \
    shinychat \
    tidyverse \
    duckdbfs \
    markdown

RUN installGithub.r cboettig/mapgl tidyverse/ellmer

COPY . .

CMD ["R", "--quiet", "-e", "shiny::runApp(host='0.0.0.0', port=7860)"]
