# Dockerfile for Shinyapps.io Deployment Runner
FROM r-base:4.4.2

# Confirm the base codename before trusting the URL below:
#   docker run --rm r-base:4.4.2 sh -c 'cat /etc/os-release | grep VERSION_CODENAME'
# r-base:4.4.2 should be Debian bookworm. If it's trixie, swap the codename below.
ENV P3M=https://packagemanager.posit.co/cran/__linux__/bookworm/latest

# Install system dependencies (still needed at runtime even for binary packages)
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

# Install renv and rsconnect (as binaries)
RUN R -e "install.packages(c('renv', 'rsconnect'), repos = '$P3M')"

# Create app directory
WORKDIR /app

# Force renv to restore from P3M binaries instead of compiling from source.
# This is the line that kills the ~46 min duckdb build in the deploy path.
ENV RENV_CONFIG_REPOS_OVERRIDE=$P3M

# Copy renv.lock and restore to default library paths (global)
COPY renv.lock /app/renv.lock
RUN R -e "renv::restore()"

# Copy the rest of the app
COPY . /app/

# Set environment variable to suppress GUI warnings
ENV DISPLAY=""

# Set the entrypoint to run the deployment script
ENTRYPOINT ["Rscript", "--vanilla", "aws/deploy_script.R"]