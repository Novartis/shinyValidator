FROM r-base
RUN apt-get update && \
    apt-get install -y \
    # apache server 
    apache2 \
    apache2-utils \
    #ca-certificates \
    lsb-release \
    gnupg \
    git \
    git-lfs \
    gconf-service \
    libasound2 \
    libatk1.0-0 \
    libcairo2 \
    libcups2 \
    libfontconfig1-dev \
    libfontconfig1 \
    libgbm1 \
    libgdk-pixbuf2.0-0 \
    libgtk-3-0 \
    libnspr4 \
    libpango-1.0-0 \
    pandoc \
    libxss1 fonts-liberation libappindicator1 libnss3 lsb-release xdg-utils net-tools \
    libxml2-dev libgit2-dev libssl-dev build-essential cmake ed jags ccache distcc texinfo qpdf \
    libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev default-jre-headless && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# For headless testing with shinytest2, crrri, crrry, ... (all chrome based stuff)
# shinyloadtest (requires shinycannon).
RUN wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb \
         http://security.ubuntu.com/ubuntu/pool/main/i/icu/libicu66_66.1-2ubuntu2_amd64.deb \ 
         https://github.com/rstudio/shinycannon/releases/download/v1.1.3/shinycannon_1.1.3-dd43f6b_amd64.deb && \
         dpkg -i ./*.deb

ENV HEADLESS_CHROME=/usr/bin/google-chrome
ENV CHROMOTE_CHROME=/usr/bin/google-chrome

# Audit app
ADD . /app/
