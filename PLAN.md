# PLAN.md — mutsu 実用化ロードマップ (2026-05-06)

## 方針転換

roast テスト通過率の追求から、**実用的な Raku インタープリタとしての品質向上**にシフトする。
roast は引き続き回帰検知に使うが、セカンドプライオリティとする。

## 現状 (2026-05-06)

- Whitelist: 1145 / 1296 (88.3%)
- 基本的なスクリプトは動く: クラス、ファイルI/O、regex、try/CATCH、start/await、qqx、型チェック、MAIN
- **実用上の致命的な穴**がいくつかある

## Phase 1: デバッガビリティ (最優先)

### 1-1. ランタイムエラーに行番号を付与

現状:
```
Runtime error: oops
```

目標:
```
Runtime error: oops
  in sub bar at script.raku line 3
  in sub foo at script.raku line 2
  in block <unit> at script.raku line 4
```

- [ ] コンパイラが各 OpCode にソース行番号を埋め込む
- [ ] VM がエラー発生時に行番号を報告
- [ ] コールスタックを RuntimeError に記録

### 1-2. `.backtrace` メソッド実装

- [ ] `$!.backtrace` が Backtrace オブジェクトを返す
- [ ] `try { die "x" }; say $!.backtrace` が動く

### 1-3. CATCH ブロック内の `$!` 改善

- [ ] `$!.line`, `$!.file` が動く
- [ ] `warn` の位置情報も改善

## Phase 2: Grammar / Match の実用性

### 2-1. 名前付きキャプチャの修正

現状:
```raku
grammar G { token TOP { <word>+ % \s+ }; token word { \w+ } }
my $m = G.parse("hello world");
$m<word>[0]  # → Nil  ← 壊れている
```

- [ ] 繰り返しの名前付きキャプチャが Array として格納される
- [ ] `$m<word>` が Match の Array を返す

### 2-2. Grammar Action クラス

- [ ] `G.parse($str, actions => Actions)` が動く
- [ ] `make` / `$/.made` (`.ast`) が動く

## Phase 3: CLI ツール品質

### 3-1. MAIN 引数パースの修正

- [ ] Usage が二重表示されるバグを修正
- [ ] `--help` 自動生成
- [ ] multi MAIN のサポート確認

### 3-2. エラーメッセージの一貫性

- [ ] パースエラーにコード箇所のスニペット表示
- [ ] 「Did you mean ...?」サジェスション

## Phase 4: ドキュメント

### 4-1. README 拡充

- [ ] 「何が動くか」セクション (スクリプト例付き)
- [ ] インストール方法
- [ ] 既知の制限事項

### 4-2. ユーザーガイド

- [ ] 基本的な使い方 (CLI, REPL, スクリプト実行)
- [ ] サポートする構文の概要
- [ ] raku との差異一覧

## Phase 5: モジュールエコシステム

### 5-1. `require` 実装

- [ ] 動的モジュールロード
- [ ] `require Module; Module.new(...)` が動く

### 5-2. 外部モジュール対応

- [ ] `RAKULIB` / `-I` によるモジュール検索パス
- [ ] 既存の Raku モジュール (JSON::Tiny 等) の互換性確認
- [ ] 最小限のモジュールインストーラ検討

## Phase 6: パフォーマンス

- [ ] 起動時間の計測と改善
- [ ] 大きなファイルの処理速度
- [ ] ベンチマーク (vs raku) の公開

## Phase R: Roast (継続的・セカンドプライオリティ)

引き続き roast テスト通過率を上げるが、実用性に寄与する修正を優先する。

- Container semantics — 実用性にも影響大
- Exception X::* クラス — エラー品質にも影響大
- native array — 実用影響は小さい (後回し)
- Threading — 実用影響は中程度 (後回し)

## メトリクス

| 指標 | 現在 | Phase 1 後 | Phase 2 後 |
|------|------|-----------|-----------|
| エラーに行番号 | なし | あり | あり |
| .backtrace | 未実装 | 実装済 | 実装済 |
| Grammar 名前付きキャプチャ | 壊れ | 壊れ | 修正済 |
| README | 最小 | 最小 | 拡充済 |
| Whitelist | 1145 | 1145+ | 1150+ |
