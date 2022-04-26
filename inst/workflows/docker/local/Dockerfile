FROM divadnojnarg/shinyvalidator-docker:latest
RUN apt-get update && \
    apt-get install -y \
    # apache server 
    apache2 apache2-utils && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*
# Audit app
ADD . /app/
