#!/bin/bash
# Setup script for the mutsu Incus VM
# Run from host: bash scripts/incus-setup.sh
set -euo pipefail

VM=mutsu

echo "=== Configuring VM resources ==="
NEED_RESTART=false

CURRENT_MEM=$(incus config get "$VM" limits.memory)
if [ "$CURRENT_MEM" != "20GB" ]; then
    echo "Setting memory to 20GB (was: ${CURRENT_MEM:-default})"
    NEED_RESTART=true
else
    echo "Memory already set to 20GB"
fi

CURRENT_DISK=$(incus config device get "$VM" root size 2>/dev/null || true)
if [ "$CURRENT_DISK" != "100GB" ]; then
    echo "Setting disk to 100GB (was: ${CURRENT_DISK:-default})"
    NEED_RESTART=true
else
    echo "Disk already set to 100GB"
fi

if [ "$NEED_RESTART" = true ]; then
    incus stop "$VM" --timeout 30 || incus stop "$VM" --force
    incus config set "$VM" limits.memory 20GB
    incus config device override "$VM" root size=100GB
    incus start "$VM"
    echo "Waiting for VM to be ready..."
    sleep 5
fi

echo "=== Updating packages ==="
incus exec "$VM" -- apt-get update -qq
incus exec "$VM" -- apt-get upgrade -y -qq

echo "=== Creating user tokuhirom ==="
incus exec "$VM" -- bash -c 'if id tokuhirom &>/dev/null; then
    echo "user tokuhirom already exists"
else
    useradd -m -s /bin/bash tokuhirom
    usermod -aG sudo tokuhirom
    echo "tokuhirom ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/tokuhirom
    echo "user tokuhirom created"
fi'

echo "=== Installing build dependencies ==="
incus exec "$VM" -- apt-get install -y -qq \
    git \
    curl \
    build-essential \
    pkg-config \
    libssl-dev \
    dnsutils \
    ca-certificates \
    tmux

echo "=== Installing Rust via rustup ==="
incus exec "$VM" -- su - tokuhirom -c 'source ~/.cargo/env 2>/dev/null; if command -v rustup &>/dev/null; then
    rustup update
else
    curl --proto "=https" --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
fi'
# Ensure .profile sources cargo env for login shells (su -, ssh, etc.)
incus exec "$VM" -- su - tokuhirom -c 'grep -q cargo/env ~/.profile 2>/dev/null || echo ". \"\$HOME/.cargo/env\"" >> ~/.profile'

echo "=== Installing Raku ==="
incus exec "$VM" -- bash -c 'if command -v raku &>/dev/null; then
    echo "raku already installed"
else
    apt-get install -y -qq rakudo
fi'

echo "=== Installing prove (perl Test::Harness) ==="
incus exec "$VM" -- apt-get install -y -qq perl

echo "=== Installing GitHub CLI ==="
incus exec "$VM" -- bash -c 'if command -v gh &>/dev/null; then
    echo "gh already installed: $(gh --version | head -1)"
else
    curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg
    echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | tee /etc/apt/sources.list.d/github-cli.list > /dev/null
    apt-get update -qq
    apt-get install -y -qq gh
fi'

echo "=== Installing Node.js (for Claude Code) ==="
incus exec "$VM" -- bash -c 'if command -v node &>/dev/null; then
    echo "node already installed: $(node --version)"
else
    curl -fsSL https://deb.nodesource.com/setup_22.x | bash -
    apt-get install -y -qq nodejs
fi'

echo "=== Installing Claude Code ==="
incus exec "$VM" -- npm install -g @anthropic-ai/claude-code

echo "=== Configuring git ==="
incus exec "$VM" -- su - tokuhirom -c 'git config --global user.name "Tokuhiro Matsuno"'
incus exec "$VM" -- su - tokuhirom -c 'git config --global user.email "tokuhirom@gmail.com"'

echo "=== Verifying installations ==="
incus exec "$VM" -- su - tokuhirom -c 'source ~/.cargo/env 2>/dev/null; echo "git: $(git --version)"; echo "rustc: $(rustc --version)"; echo "cargo: $(cargo --version)"; echo "raku: $(raku --version | head -1)"; echo "prove: $(prove --version | head -1)"; echo "node: $(node --version)"; echo "claude: $(claude --version)"'

echo "=== Setup complete ==="
echo ""
echo "Login: incus exec $VM -- su - tokuhirom"
