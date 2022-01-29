ARG UBUNTU_TAG=21.10
FROM ubuntu:"$UBUNTU_TAG"

ENV LANG=C.UTF-8
RUN \
    apt-get update && \
    apt-get install --assume-yes curl gcc git libgmp-dev libtinfo5 make sudo zlib1g-dev

ARG USER_NAME=haskell
RUN \
    useradd --create-home --shell "$( which bash )" "$USER_NAME" && \
    echo "$USER_NAME ALL=(ALL) NOPASSWD: ALL" | tee "/etc/sudoers.d/$USER_NAME"
USER "$USER_NAME"
ENV PATH="/home/$USER_NAME/.local/bin:/home/$USER_NAME/.cabal/bin:/home/$USER_NAME/.ghcup/bin:$PATH"
WORKDIR "/home/$USER_NAME"

ARG GHCUP_VERSION=0.1.17.4
RUN \
    mkdir --parents ~/.local/bin && \
    curl --output ~/.local/bin/ghcup "https://downloads.haskell.org/~ghcup/$GHCUP_VERSION/x86_64-linux-ghcup-$GHCUP_VERSION" && \
    chmod +x ~/.local/bin/ghcup && \
    ghcup --version

ARG GHC_VERSION=9.2.1
RUN \
    ghcup install ghc "$GHC_VERSION" --set && \
    ghc --version

ARG CABAL_VERSION=3.6.2.0
RUN \
    ghcup install cabal "$CABAL_VERSION" --set && \
    cabal --version

ARG HLS_VERSION=1.6.0.0
RUN \
    ghcup install hls "$HLS_VERSION" --set && \
    haskell-language-server-wrapper --version
