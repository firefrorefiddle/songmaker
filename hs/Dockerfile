# Start with a Haskell base image
FROM ubuntu:20.04

ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    git \
    libgmp-dev \
    zlib1g-dev \
    make \
    texlive-full \
    texlive-xetex \
    texlive-latex-extra \
    texlive-fonts-recommended \
    texlive-fonts-extra \
    texlive-science \
    && apt-get clean

RUN apt install -y build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Set working directory
WORKDIR /usr/src/app

SHELL ["/bin/bash", "-c"]

RUN echo 'export PATH="/root/.ghcup/bin:$PATH"' >> ~/.bashrc

ENV HOME="/root"

# Add GHCup (Haskell toolchain installer) to the PATH
ENV PATH="/root/.ghcup/bin:$PATH"

RUN /bin/sh -c 'export PATH="$HOME/.ghcup/bin:$PATH" && cabal update'

# Copy the project files into the container
COPY . .

RUN export PATH="$HOME/.ghcup/bin:$PATH" && cabal install --only-dependencies

# Build the Haskell project
RUN export PATH="$HOME/.ghcup/bin:$PATH" && cabal build

RUN tar xzf songs-3.1.tar.gz && cd songs-3.1 && ./configure && make && make install

# Specify the command to run the project or start a shell by default
CMD export PATH="$HOME/.ghcup/bin:$PATH" && cabal run songmaker-cli
