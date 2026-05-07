# PLAN.md — mutsu 実用化ロードマップ (2026-05-07 更新)

## 方針転換

roast テスト通過率の追求から、**実用的な Raku インタープリタとしての品質向上**にシフトする。
roast は引き続き回帰検知に使うが、セカンドプライオリティとする。

## 現状 (2026-05-07)

- Whitelist: 1148 / 1296 (88.6%)
- 基本的なスクリプトは動く: クラス、ファイルI/O、regex、try/CATCH、start/await、qqx、型チェック、MAIN
- JSON::Tiny の `to-json` が動作、`from-json` は部分的に動作
- エラーメッセージに行番号・バックトレース・コードスニペット表示
- 「Did you mean ...?」サジェスション実装済み

## Phase 1: デバッガビリティ ✅ 完了

### 1-1. ランタイムエラーに行番号を付与 ✅

- [x] コンパイラが各 OpCode にソース行番号を埋め込む (SetLine/SetSourceLine opcodes)
- [x] VM がエラー発生時に行番号を報告
- [x] コールスタックを RuntimeError に記録 (RoutineFrame, build_backtrace_string)

PR: #2203

### 1-2. `.backtrace` メソッド実装 ✅

- [x] `$!.backtrace` が Backtrace オブジェクトを返す (frames, .list, .Str)
- [x] `try { die "x" }; say $!.backtrace` が動く
- [x] Backtrace::Frame に .subname, .file, .line メソッド

PR: #2215

### 1-3. CATCH ブロック内の `$!` 改善 ✅

- [x] `$!.line`, `$!.file` が動く
- [ ] `warn` の位置情報も改善 (未着手)

PR: #2206

## Phase 2: Grammar / Match の実用性 ✅ 完了

### 2-1. 名前付きキャプチャの修正 ✅

- [x] 繰り返しの名前付きキャプチャが Array として格納される
- [x] `$m<word>` が Match の Array を返す

PR: #2203 (regex_match_core.rs fast path fix)

### 2-2. Grammar Action クラス ✅

- [x] `G.parse($str, actions => Actions)` が動く
- [x] `make` / `$/.made` (`.ast`) が動く

## Phase 3: CLI ツール品質 ✅ 完了

### 3-1. MAIN 引数パースの修正 ✅

- [x] Usage が二重表示されるバグを修正
- [x] `--help` 自動生成
- [x] multi MAIN のサポート確認

PR: #2203

### 3-2. エラーメッセージの一貫性 ✅

- [x] パースエラーにコード箇所のスニペット表示 (===SORRY!=== 形式)
- [x] 「Did you mean ...?」サジェスション (Levenshtein距離ベース)

PR: #2211, #2218

## Phase 4: ドキュメント ✅ 完了

### 4-1. README 拡充 ✅

- [x] 「何が動くか」セクション (スクリプト例付き)
- [x] インストール方法
- [x] 既知の制限事項

### 4-2. ユーザーガイド ✅

- [x] 基本的な使い方 (CLI, REPL, スクリプト実行)
- [x] サポートする構文の概要
- [x] raku との差異一覧

PR: #2219 — `docs/user-guide.md` (411行)

## Phase 5: モジュールエコシステム ⚠️ 一部完了

### 5-1. `require` 実装 ✅

- [x] 動的モジュールロード
- [x] `require Module; Module.new(...)` が動く

PR: #2213

### 5-2. 外部モジュール対応 ⚠️

- [x] `RAKULIB` / `-I` によるモジュール検索パス — 動作済み
- [x] JSON::Tiny `to-json` — 動作 (PR #2216)
- [x] JSON::Tiny `from-json` — 数値/bool/null 動作、文字列/ネストは未完 (PR #2220)
- [ ] 最小限のモジュールインストーラ検討
- [ ] 他のモジュール互換性確認

### 残タスク

- `from-json` の文字列・配列・オブジェクト対応 (grammar の量指定子付き名前付きキャプチャの改善が必要)
- 他の pure-Raku モジュールの互換性テスト

## Phase 6: パフォーマンス ✅ ベンチマーク作成済み

- [x] 起動時間の計測 — **mutsu 0.006s vs raku 0.135s (25倍速い)**
- [x] ベンチマーク (vs raku) — `benchmarks/` に6種のベンチマーク
- [ ] 関数呼び出しオーバーヘッドの改善 (fib: 79x 遅い)
- [ ] コレクション操作の改善 (array ops: 46x 遅い)

PR: #2217

### ベンチマーク結果 (2026-05-07)

| ベンチマーク | mutsu/raku 比 |
|-------------|--------------|
| 起動時間 | **0.04x (25倍速い)** |
| 文字列操作 | 2.5x 遅い |
| OOP | 5.4x 遅い |
| ハッシュ | 11.7x 遅い |
| 配列操作 | 45.5x 遅い |
| 再帰 fib | 78.8x 遅い |

## Phase R: Roast (継続的・セカンドプライオリティ)

引き続き roast テスト通過率を上げるが、実用性に寄与する修正を優先する。

- Container semantics — 実用性にも影響大
- Exception X::* クラス — エラー品質にも影響大
- native array — 実用影響は小さい (後回し)
- Threading — 実用影響は中程度 (後回し)

### 本セッションの Roast 成果

- S12-class/inheritance.t — Array サブクラス継承、X::Inheritance::UnknownParent (#2205)
- S02-literals/string-interpolation.t — double-sigil 補間パース (#2221)
- S03-metaops/reduce.t — scan reduction 遅延評価 (#2208)
- S06-signature/named-parameters.t — 軽量関数呼び出しパス (#2209)
- S05-metasyntax/longest-alternative.t — regex quantifier 高速化 (#2212)
- S07-hyperrace/basics.t — hyper/race 並列実行実装 (#2214)

## メトリクス

| 指標 | Phase 開始前 | 現在 |
|------|-------------|------|
| エラーに行番号 | なし | ✅ あり |
| .backtrace | 未実装 | ✅ Backtrace オブジェクト |
| $!.line/$!.file | 未実装 | ✅ 実装済 |
| Did you mean? | なし | ✅ 実装済 |
| パースエラースニペット | なし | ✅ ===SORRY!=== 形式 |
| Grammar 名前付きキャプチャ | 壊れ | ✅ 修正済 |
| Grammar Actions | 未実装 | ✅ make/.made 動作 |
| JSON::Tiny | 動かない | ⚠️ to-json 動作, from-json 部分的 |
| README | 最小 | ✅ 拡充済 |
| ユーザーガイド | なし | ✅ docs/user-guide.md |
| ベンチマーク | なし | ✅ benchmarks/ |
| Whitelist | 1145 | 1148 |
