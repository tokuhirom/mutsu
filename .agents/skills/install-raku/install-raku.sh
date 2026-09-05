#!/usr/bin/env bash
# Install an upstream Rakudo prebuilt binary so that `raku` is available as an
# oracle for mutsu development. No compilation: the archive from rakudo.org is a
# self-contained, relocatable tree.
#
# Usage: install-raku.sh [--prefix DIR] [--bindir DIR] [--version VER]
#                        [--force] [--no-verify] [--print-url] [--help]
set -euo pipefail

INDEX_URL="https://rakudo.org/dl/rakudo"
PREFIX="${RAKUDO_PREFIX:-$HOME/.local/rakudo}"
BINDIR="${RAKUDO_BINDIR:-$HOME/.local/bin}"
WANT_VERSION=""
FORCE=0
VERIFY=1
PRINT_URL_ONLY=0

die() { printf 'install-raku: %s\n' "$*" >&2; exit 1; }
info() { printf 'install-raku: %s\n' "$*" >&2; }

usage() {
  # Reprint the leading comment block (everything after the shebang).
  awk 'NR > 1 { if (!/^#/) exit; sub(/^# ?/, ""); print }' "$0"
  cat <<'EOF'

Options:
  --prefix DIR    where the rakudo tree is unpacked (default ~/.local/rakudo,
                  or $RAKUDO_PREFIX)
  --bindir DIR    where `raku`/`rakudo` symlinks are created (default
                  ~/.local/bin, or $RAKUDO_BINDIR)
  --version VER   install this release (e.g. 2026.07) instead of the newest
  --force         reinstall even if a working `raku` is already on PATH
  --no-verify     skip the SHA256 checksum check
  --print-url     print the selected tarball URL and exit
EOF
}

while [ $# -gt 0 ]; do
  case "$1" in
    --prefix) PREFIX="${2:?--prefix needs a directory}"; shift 2 ;;
    --bindir) BINDIR="${2:?--bindir needs a directory}"; shift 2 ;;
    --version) WANT_VERSION="${2:?--version needs a release}"; shift 2 ;;
    --force) FORCE=1; shift ;;
    --no-verify) VERIFY=0; shift ;;
    --print-url) PRINT_URL_ONLY=1; shift ;;
    -h|--help) usage; exit 0 ;;
    *) die "unknown option: $1 (try --help)" ;;
  esac
done

# ---------------------------------------------------------------- preflight --
if [ "$FORCE" -eq 0 ] && [ "$PRINT_URL_ONLY" -eq 0 ] && command -v raku >/dev/null 2>&1; then
  info "raku is already on PATH: $(command -v raku)"
  raku --version >&2 || die "the existing raku is broken; rerun with --force"
  exit 0
fi

command -v curl >/dev/null 2>&1 || die "curl is required"
command -v tar  >/dev/null 2>&1 || die "tar is required"

case "$(uname -s)" in
  Linux)  OS=linux ;;
  Darwin) OS=macos ;;
  *) die "unsupported OS: $(uname -s). Prebuilts exist for linux, macos and win only." ;;
esac
case "$(uname -m)" in
  x86_64|amd64)  ARCH=x86_64 ;;
  arm64|aarch64) ARCH=arm64 ;;
  *) die "unsupported CPU: $(uname -m)" ;;
esac

if [ "$OS" = linux ] && [ "$ARCH" = arm64 ]; then
  die "rakudo.org publishes no linux/arm64 prebuilt. Build from source with
  rakubrew (https://rakubrew.org/), or run the oracle in the official
  rakudo/rakudo container image instead."
fi

# ------------------------------------------------------- pick the newest URL --
# The index is a JSON array of release entries. Choose the newest one that is a
# moar-backend *archive* for this OS/arch, sorting by (ver, build_rev). The
# `latest` flag is not usable here: it marks the newest source release, which is
# often published before the binary builds for a given platform exist.
select_url() {
  local index="$1"
  if command -v python3 >/dev/null 2>&1; then
    OS="$OS" ARCH="$ARCH" WANT_VERSION="$WANT_VERSION" python3 - "$index" <<'PY'
import json, os, sys
os_, arch = os.environ["OS"], os.environ["ARCH"]
want = os.environ.get("WANT_VERSION") or None
with open(sys.argv[1]) as fh:
    entries = json.load(fh)
cands = [
    e for e in entries
    if e.get("name") == "rakudo" and e.get("type") == "archive"
    and e.get("backend") == "moar" and e.get("platform") == os_
    and e.get("arch") == arch and (want is None or e.get("ver") == want)
]
if not cands:
    sys.exit(1)
best = max(cands, key=lambda e: (e.get("ver", ""), e.get("build_rev", 0)))
print(best["url"])
PY
  elif command -v jq >/dev/null 2>&1; then
    jq -r --arg os "$OS" --arg arch "$ARCH" --arg want "$WANT_VERSION" '
      [ .[] | select(.name == "rakudo" and .type == "archive" and .backend == "moar"
                     and .platform == $os and .arch == $arch)
            | select($want == "" or .ver == $want) ]
      | sort_by(.ver, (.build_rev // 0)) | last | .url // empty
    ' "$index"
  else
    die "either python3 or jq is required to read the rakudo release index"
  fi
}

TMPDIR_INSTALL="$(mktemp -d)"
trap 'rm -rf "$TMPDIR_INSTALL"' EXIT

info "fetching release index from $INDEX_URL"
curl -fsSL -o "$TMPDIR_INSTALL/index.json" "$INDEX_URL" \
  || die "could not fetch $INDEX_URL"

URL="$(select_url "$TMPDIR_INSTALL/index.json" || true)"
[ -n "$URL" ] || die "no ${OS}/${ARCH} moar prebuilt${WANT_VERSION:+ for $WANT_VERSION} in the index"

if [ "$PRINT_URL_ONLY" -eq 1 ]; then
  printf '%s\n' "$URL"
  exit 0
fi

TARBALL="${URL##*/}"
# rakudo-moar-2026.07-01-linux-x86_64-gcc.tar.gz -> rakudo-moar-2026.07-01-linux-x86_64-gcc
RELEASE="${TARBALL%.tar.gz}"

# ------------------------------------------------------ download and verify --
info "downloading $TARBALL"
curl -fSL --progress-bar -o "$TMPDIR_INSTALL/$TARBALL" "$URL" \
  || die "download failed: $URL"

if [ "$VERIFY" -eq 1 ]; then
  if command -v sha256sum >/dev/null 2>&1; then
    GOT="$(sha256sum "$TMPDIR_INSTALL/$TARBALL" | cut -d' ' -f1)"
  elif command -v shasum >/dev/null 2>&1; then
    GOT="$(shasum -a 256 "$TMPDIR_INSTALL/$TARBALL" | cut -d' ' -f1)"
  else
    GOT=""
    info "no sha256sum/shasum available; skipping checksum verification"
  fi
  if [ -n "$GOT" ]; then
    # The .checksums.txt is a clear-signed file with one "SHA256 (file) = hex" line.
    if curl -fsSL -o "$TMPDIR_INSTALL/checksums.txt" "$URL.checksums.txt"; then
      WANT_SUM="$(grep -o "SHA256 ($TARBALL) = [0-9a-f]*" "$TMPDIR_INSTALL/checksums.txt" \
                  | head -n1 | awk '{print $NF}')"
      [ -n "$WANT_SUM" ] || die "no SHA256 line for $TARBALL in the checksum file"
      [ "$WANT_SUM" = "$GOT" ] \
        || die "checksum mismatch for $TARBALL: expected $WANT_SUM, got $GOT"
      info "sha256 verified"
    else
      die "could not fetch $URL.checksums.txt (rerun with --no-verify to skip)"
    fi
  fi
fi

# ------------------------------------------------------------------ install --
DEST="$PREFIX/$RELEASE"
mkdir -p "$DEST"
info "unpacking into $DEST"
tar -xzf "$TMPDIR_INSTALL/$TARBALL" -C "$DEST" --strip-components=1

[ -x "$DEST/bin/raku" ] || die "the archive contains no bin/raku (layout changed?)"

mkdir -p "$BINDIR"
for exe in "$DEST"/bin/*; do
  [ -f "$exe" ] && [ -x "$exe" ] || continue
  ln -sf "$exe" "$BINDIR/$(basename "$exe")"
done
info "symlinked $(basename "$DEST")/bin/* into $BINDIR"

# ------------------------------------------------------------------- verify --
"$DEST/bin/raku" -e 'say "raku ok: ", $*RAKU.compiler.version' \
  || die "the installed raku does not run"

if ! command -v raku >/dev/null 2>&1; then
  cat >&2 <<EOF

install-raku: $BINDIR is not on PATH. Add it, e.g.:
  export PATH="$BINDIR:\$PATH"
EOF
fi
