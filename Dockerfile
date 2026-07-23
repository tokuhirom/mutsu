# mutsu — a batteries-included Raku interpreter, with the bundled `mzef`
# package manager. Try it:
#
#   docker run --rm -it ghcr.io/tokuhirom/mutsu            # Raku REPL
#   docker run --rm ghcr.io/tokuhirom/mutsu mutsu -e 'say (^10).sum'
#   docker run --rm ghcr.io/tokuhirom/mutsu mzef --version
#
# Build locally:
#   docker build -t mutsu .

# Two stages, fully separated:
#   * builder — the Rust toolchain compiles the binaries (heavy; ~1.5GB, never
#     shipped).
#   * runtime — a slim Debian image that only carries the compiled binaries,
#     the bundled zef tree, and zef's runtime shell-out tools.
# The final image contains NONE of the build toolchain or source tree.

# ---- builder (build stage) --------------------------------------------------
FROM rust:1.96-bookworm AS builder

# pcre2 links the system libpcre2 (the pcre2 feature); libffi is built vendored
# and statically linked into the binary, so it needs no runtime package.
RUN apt-get update && apt-get install -y --no-install-recommends \
        libpcre2-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /src
COPY . .

# Build both shipped binaries: the interpreter and the mzef package-manager shim.
RUN cargo build --release --bin mutsu --bin mzef

# ---- runtime (run stage) ----------------------------------------------------
FROM debian:bookworm-slim AS runtime

# Runtime deps:
#   libpcre2-8-0            - dynamically linked by mutsu (pcre2 feature)
#   libssl3                 - dlopen'd by the bundled OpenSSL battery for HTTPS/TLS
#                            (the crypto rides the OS so its CVEs are patched by
#                            apt, independent of a mutsu release)
#   curl / git / tar / unzip / ca-certificates
#                          - zef's fetch/extract backends shell out to these
RUN apt-get update && apt-get install -y --no-install-recommends \
        libpcre2-8-0 \
        libssl3 \
        curl \
        git \
        tar \
        unzip \
        ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Self-contained layout, identical to the release tarball: mzef resolves the
# bundled zef via ../share/mutsu/zef relative to its own executable, and mutsu
# resolves the bundled batteries via ../share/mutsu/modules.
COPY --from=builder /src/target/release/mutsu /usr/local/bin/mutsu
COPY --from=builder /src/target/release/mzef  /usr/local/bin/mzef
COPY --from=builder /src/vendor/zef           /usr/local/share/mutsu/zef
COPY --from=builder /src/modules              /usr/local/share/mutsu/modules

# Belt-and-suspenders: the exe-relative resolution already finds the trees above,
# but pin them explicitly so the shim/interpreter keep working even if the
# binary is copied elsewhere in a derived image.
ENV MZEF_ZEF_HOME=/usr/local/share/mutsu/zef
ENV MUTSU_BUNDLE_DIR=/usr/local/share/mutsu/modules

WORKDIR /work

# Default to the interactive Raku REPL (run with `-it`); override with any
# command, e.g. `mutsu script.raku` or `mzef install <dist>`.
CMD ["mutsu", "--repl"]
