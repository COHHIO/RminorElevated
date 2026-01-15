# Dockerfile for Shinyapps.io Deployment Runner
FROM r-base:4.4.2

# Install system dependencies with better error handling
RUN apt-get clean && \
    rm -rf /var/lib/apt/lists/* && \
    apt-get update -y && \
    apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libcairo2-dev \
    libxt-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff-dev \
    libjpeg-dev \
    libgit2-dev \
    pkg-config \
    git \
    curl && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Install renv and rsconnect
RUN R -e "install.packages(c('renv', 'rsconnect'), repos = c(CRAN = 'https://cloud.r-project.org'))"

# Create app directory
WORKDIR /app

# Copy renv.lock and restore to default library paths (global)
COPY renv.lock /app/renv.lock
RUN R -e "renv::restore()"

# Copy the rest of the app
COPY . /app/

# Set environment variable to suppress GUI warnings
ENV DISPLAY=""

# Set the entrypoint to run the deployment script
ENTRYPOINT ["Rscript", "--vanilla", "aws/deploy_script.R"]