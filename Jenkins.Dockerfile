FROM ubuntu:cosmic

RUN DEBIAN_FRONTEND=noninteractive apt-get update && apt-get install -y gcc make doxygen graphviz wget

ENV RUSTUP_HOME=/usr/local/rustup \
    CARGO_HOME=/usr/local/cargo \
    PATH=/usr/local/cargo/bin:$PATH

RUN set -eux; \
    \
    url="https://static.rust-lang.org/rustup/dist/x86_64-unknown-linux-gnu/rustup-init"; \
    wget "$url"; \
    chmod +x rustup-init; \
    ./rustup-init -y --no-modify-path --default-toolchain nightly-2018-12-17; \
    rm rustup-init; \
    chmod -R a+w $RUSTUP_HOME $CARGO_HOME; \
    rustup --version; \
    cargo --version; \
    rustc --version;

RUN rustup toolchain install nightly-2018-12-17-x86_64-unknown-linux-gnu
RUN rustup default nightly-2018-12-17-x86_64-unknown-linux-gnu
RUN rustup component add clippy-preview 
RUN rustup component add rustfmt-preview

RUN DEBIAN_FRONTEND=noninteractive apt-get install -y git

RUN DEBIAN_FRONTEND=noninteractive apt-get install -y python


RUN DEBIAN_FRONTEND=noninteractive apt-get install -y libclang-dev


RUN DEBIAN_FRONTEND=noninteractive apt-get install -y clang


RUN DEBIAN_FRONTEND=noninteractive apt-get install -y python3


RUN DEBIAN_FRONTEND=noninteractive apt-get install -y default-jdk
