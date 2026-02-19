#!/bin/bash
set -euo pipefail

while true; do
  echo "=== $(date): 新しいサイクル開始 ==="

  git checkout main
  git pull origin main

  # 失敗テストファイルを取得
  TEST_FILE=$(./scripts/pick-next-roast.sh)

  if [ -z "$TEST_FILE" ]; then
    echo "全テスト完了！修正対象なし。"
    break
  fi

  echo "対象: $TEST_FILE"

  BRANCH="fix/$(basename "$TEST_FILE" | sed 's/[^a-zA-Z0-9_-]/-/g')-$(date +%s)"
  git checkout -b "$BRANCH"

  claude -p \
    "$TEST_FILE のテストを実行し、通るまでコードを修正してください。

手順:
1. テストを実行して失敗内容を確認
2. 対応するソースコードを修正
3. テストが通るまで繰り返す（最大5回）
4. 修正完了したら git add && git commit
5. 5回試しても通らなければギブアップ"

  # 最終確認
  if go test "./${TEST_FILE%/*}/..." 2>&1; then
    echo "✅ テスト通過 → PR作成（auto-merge）"
    git push origin "$BRANCH"
    PR_URL=$(gh pr create \
      --title "fix: $(basename "$TEST_FILE") のテスト修正" \
      --body "Claude Code 自動修正。対象: $TEST_FILE" \
      --base main --head "$BRANCH")
    gh pr merge "$PR_URL" --auto --squash
  else
    echo "❌ テスト失敗 → draft PR作成"
    git push origin "$BRANCH"
    gh pr create \
      --title "WIP: $(basename "$TEST_FILE")（要レビュー）" \
      --body "自動修正失敗。手動レビュー必要。対象: $TEST_FILE" \
      --base main --head "$BRANCH" --draft
  fi

  git checkout main
  echo "=== サイクル完了 ==="
  sleep 5
done
