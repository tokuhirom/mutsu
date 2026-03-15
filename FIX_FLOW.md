# FIX_FLOW.md - Roast test fixing workflow

## Overview

TODO.md をベースに、3並列のエージェントで roast テストの修正を進めるワークフロー。

## Steps

### 1. 現状の把握

```bash
# roast の全テストを分類
./scripts/roast-history.sh

# カテゴリ別のファイルが tmp/ に生成される
#   tmp/roast-panic.txt    — Rust panic (最優先)
#   tmp/roast-timeout.txt  — タイムアウト
#   tmp/roast-error.txt    — TAP plan なし
#   tmp/roast-fail.txt     — 一部サブテスト失敗
#   tmp/roast-pass.txt     — 全パス
```

### 2. TODO.md の確認と差分チェック

```bash
# TODO.md の最終更新以降のコミットログを確認し、
# 既に実装済みの項目を把握する
git log --oneline --since="<TODO.md last update date>" --no-merges | head -50
```

TODO.md の「High-impact blockers」と「Low-hanging fruit」から、
まだ未実装の項目をリストアップする。

### 3. タスクの選定と並列実行

以下の優先順位でタスクを3つ選ぶ:

1. **Panic** — Rust panic を引き起こすテスト (最優先)
2. **Near-passing / Single-fix unlocks** — 少ない修正で多くのサブテストが通るもの
3. **High-impact blockers** — B1-B7 の中で残っている大きな課題

各タスクは独立した領域 (IO, OOP, signatures 等) から選び、
衝突しないようにする。

### 4. エージェント起動

Agent ツールで3つのエージェントをバックグラウンドで起動する。
**必ず `isolation: "worktree"` を指定すること。**
同じディレクトリで並列に git 操作すると衝突して全滅する。

各エージェントには以下を指示:

- `git pull origin main` で最新を取得
- `cargo build` でビルド
- 対象テストを `raku` と `mutsu` で比較
- 修正を実装
- `cargo clippy -- -D warnings` と `cargo fmt` を実行
- `make test` で回帰がないことを確認
- feature branch を作成し、PR を開く
- `gh pr merge --auto --squash` で auto-merge を有効化
- `gh pr checks --watch` で CI 完了まで監視

### 5. 完了したエージェントの補充

エージェントが完了してマージされたら、
残りのタスクから次のものを選んで新しいエージェントを起動する。
常に3並列を維持する。

### 6. サイクルの繰り返し

全てのエージェントが完了したら、再度 `roast-history.sh` を実行して
新たに通るようになったテストを確認し、ホワイトリストに追加。
TODO.md を更新して次のサイクルに進む。
