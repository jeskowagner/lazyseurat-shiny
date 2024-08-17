# Minimal container
FROM mambaorg/micromamba

# Conda dependency list
COPY --chown=$MAMBA_USER:$MAMBA_USER env.yaml /tmp/env.yaml

# R shiny launch script in home dir
COPY --chown=$MAMBA_USER:$MAMBA_USER secondary_entrypoint.sh /home/$MAMBA_USER/secondary_entrypoint.sh
RUN chmod +x /home/$MAMBA_USER/secondary_entrypoint.sh

# Ensure base env is activated for run commands
ARG MAMBA_DOCKERFILE_ACTIVATE=1

# Install deps
RUN micromamba install -y -n base -f /tmp/env.yaml && micromamba clean --all --yes
RUN Rscript -e "devtools::install_github('jeskowagner/lazyseurat', depencencies=FALSE, upgrade='never')"

# Download app
RUN git clone --depth 1 "https://github.com/jeskowagner/lazyseurat-shiny.git" "/home/mambauser/app"

# Set entrypoint to launch app
ENTRYPOINT ["/usr/local/bin/_entrypoint.sh", "/home/mambauser/secondary_entrypoint.sh"]