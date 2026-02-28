#!/bin/bash
set -euo pipefail

usage() {
  echo "Usage: ai-sandbox [--set-window-title] [--recreate] [--dry-run] <branch> [claude|codex|bash] [args...]" >&2
  exit 1
}

SET_WINDOW_TITLE=false
RECREATE=false
DRY_RUN=false
while [[ "${1:-}" == --* ]]; do
  case "$1" in
    --set-window-title) SET_WINDOW_TITLE=true; shift ;;
    --recreate)         RECREATE=true; shift ;;
    --dry-run)          DRY_RUN=true; shift ;;
    *)                  break ;;
  esac
done

BRANCH="${1:?$(usage)}"
TOOL="${2:-bash}"
shift 2 2>/dev/null || shift $#
EXTRA_ARGS=("$@")

# .git のあるディレクトリを探す
find_repo_root() {
  local dir="$PWD"
  while [[ "$dir" != "/" ]]; do
    if [[ -d "$dir/.git" ]]; then
      echo "$dir"
      return
    fi
    dir="$(dirname "$dir")"
  done
  echo "Error: not in a git repository" >&2
  exit 1
}

if [[ -z "${GH_TOKEN:-}" ]]; then
  echo "Error: GH_TOKEN is not set" >&2
  exit 1
fi

REPO_ROOT="$(find_repo_root)"
SANDBOX_BASE="${REPO_ROOT}/.git/sandbox"
CLONE_DIR="${SANDBOX_BASE}/${BRANCH}"
SHARED_TARGET="${SANDBOX_BASE}/.shared-target"

# --recreate: 既存クローンを削除して作り直す
if [[ "$RECREATE" == true && -d "$CLONE_DIR" ]]; then
  echo "Removing existing sandbox clone for branch '${BRANCH}'..."
  rm -rf "$CLONE_DIR"
fi

# クローンがなければ作成
if [[ ! -d "$CLONE_DIR" ]]; then
  mkdir -p "$SANDBOX_BASE"
  # origin の最新を取得
  DEFAULT_BRANCH="$(git -C "$REPO_ROOT" symbolic-ref --short HEAD 2>/dev/null || echo "main")"
  echo "Fetching origin/${DEFAULT_BRANCH}..."
  if git -C "$REPO_ROOT" fetch origin "$DEFAULT_BRANCH" 2>/dev/null; then
    # update-ref only when the working tree is clean to avoid confusing git status
    if git -C "$REPO_ROOT" diff --quiet && git -C "$REPO_ROOT" diff --cached --quiet; then
      git -C "$REPO_ROOT" update-ref "refs/heads/${DEFAULT_BRANCH}" "origin/${DEFAULT_BRANCH}" 2>/dev/null || true
    else
      echo "Warning: working tree has uncommitted changes, skipping update-ref" >&2
    fi
  else
    echo "Warning: could not fetch from origin (offline?), using local state" >&2
  fi
  # 親リポジトリの GitHub remote URL を取得
  UPSTREAM_URL="$(git -C "$REPO_ROOT" remote get-url origin)"

  echo "Cloning into sandbox for branch '${BRANCH}'..."
  git clone "$UPSTREAM_URL" "$CLONE_DIR"

  cd "$CLONE_DIR"

  # ブランチが既にリモートにあれば checkout、なければ新規作成
  if git show-ref --verify --quiet "refs/remotes/origin/${BRANCH}"; then
    git checkout -b "$BRANCH" "origin/${BRANCH}"
  else
    git checkout -b "$BRANCH"
  fi

  git submodule update --init --recursive --depth 1

  # 共有 target からビルドキャッシュをコピー（存在すれば）
  # コピー後は独立して動くので他の sandbox とのビルド競合が起きない
  if [[ -d "$SHARED_TARGET" ]]; then
    echo "Copying shared build cache into sandbox..."
    cp -a "$SHARED_TARGET/." "${CLONE_DIR}/target/"
  fi

  cd "$REPO_ROOT"
fi

case "$TOOL" in
  claude|codex|bash) CMD_ARGS=("$TOOL" "${EXTRA_ARGS[@]}") ;;
  *)         echo "Unknown tool: $TOOL" >&2; usage ;;
esac

BWRAP_ARGS=(
  # システム領域を読み取り専用でマウント (/home を除く)
  --ro-bind /usr /usr
  --ro-bind /bin /bin
  --ro-bind /sbin /sbin
  --ro-bind /lib /lib
  --ro-bind /lib64 /lib64
  --ro-bind /etc /etc
  --ro-bind /run /run
  --dev /dev
  --proc /proc
  --tmpfs /tmp
  --tmpfs /home
)

# 存在する場合のみマウントするシステムディレクトリ
for dir in /opt /snap /nix /lib32; do
  [[ -d "$dir" ]] && BWRAP_ARGS+=(--ro-bind "$dir" "$dir")
done

BWRAP_ARGS+=(
  # HOME 配下: 必要なディレクトリのみ公開
  --ro-bind "${HOME}/.local/share/mise" "${HOME}/.local/share/mise"
  --ro-bind "${HOME}/.local/share/claude" "${HOME}/.local/share/claude"
  --ro-bind "${HOME}/.local/bin" "${HOME}/.local/bin"
  --ro-bind "${HOME}/.rustup" "${HOME}/.rustup"
  --bind "${HOME}/.cargo" "${HOME}/.cargo"
  --ro-bind "${HOME}/.gitconfig" "${HOME}/.gitconfig"
  --ro-bind "${HOME}/.config/gh" "${HOME}/.config/gh"
  --bind "${HOME}/.claude" "${HOME}/.claude"
  --bind "${HOME}/.claude.json" "${HOME}/.claude.json"
  --bind "${HOME}/.codex" "${HOME}/.codex"
  --ro-bind "${HOME}/.ssh" "${HOME}/.ssh"
  # クローンディレクトリだけ書き込み可能（target/ も含む）
  --bind "${CLONE_DIR}" "${CLONE_DIR}"

  --share-net
  --unshare-pid
  --die-with-parent
  --chdir "${CLONE_DIR}"
  --setenv HOME "${HOME}"
  --setenv GH_TOKEN "${GH_TOKEN:-}"
  --unsetenv CLAUDECODE
)

# tmux のウィンドウ名をブランチ名に設定 (--set-window-title 指定時のみ)
if [[ "$SET_WINDOW_TITLE" == true && -n "${TMUX:-}" ]]; then
  case "$TOOL" in
    claude)    TMUX_PREFIX="cl" ;;
    codex)     TMUX_PREFIX="cx" ;;
    bash)      TMUX_PREFIX="sh" ;;
  esac
  tmux rename-window "${TMUX_PREFIX}:${BRANCH}"
fi

# --dry-run: 設定内容を表示して終了
if [[ "$DRY_RUN" == true ]]; then
  echo "=== ai-sandbox configuration ==="
  echo "Repository:  ${REPO_ROOT}"
  echo "Branch:      ${BRANCH}"
  echo "Clone dir:   ${CLONE_DIR}"
  echo "Tool:        ${TOOL}"
  echo "GH_TOKEN:    $([[ -n "${GH_TOKEN:-}" ]] && echo "set" || echo "(not set)")"
  echo "Command:     ${CMD_ARGS[*]}"
  echo ""
  echo "=== bwrap command ==="
  echo "bwrap ${BWRAP_ARGS[*]} ${CMD_ARGS[*]}"
  exit 0
fi

exec bwrap "${BWRAP_ARGS[@]}" "${CMD_ARGS[@]}"
