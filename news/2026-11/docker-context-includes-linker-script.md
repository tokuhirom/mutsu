# The Docker build sees the startup linker script again

#19203 added `build.rs` and `linker/startup-text.ld`, which `src/main.rs` embeds with
`include_bytes!`. The Docker build context in `.dockerignore` allowlists only the paths the
image build needs, and these two were not on it. Every image build therefore failed with
`couldn't read src/../linker/startup-text.ld`. The nightly `docker-build` job caught it (#19205)
before a `v*` tag could. Both paths are now allowlisted.
