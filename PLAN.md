# PLAN.md — mutsu 実用化ロードマップ

## 方針

**実用的な Raku インタープリタとしての品質向上**を最優先とする。
roast は回帰検知に使うが、セカンドプライオリティ。

## 完了済み Phase (2026-05)

詳細は [news/2026-05.md](news/2026-05.md) 参照。

- **Phase 1: デバッガビリティ** ✅ — 行番号、バックトレース、$!.line/$!.file, Backtrace オブジェクト
- **Phase 2: Grammar/Match** ✅ — 名前付きキャプチャ、Grammar Actions (make/.made)
- **Phase 3: CLI 品質** ✅ — MAIN 修正、===SORRY!===、Did you mean?
- **Phase 4: ドキュメント** ✅ — README 拡充、ユーザーガイド
- **Phase 5: モジュール (部分)** — require 動作、JSON::Tiny to-json 動作、from-json 部分的
- **Phase 6: パフォーマンス (部分)** — ベンチマーク作成、起動 25 倍速い

## 現在の課題 (2026-05〜)

### P1: from-json 完全対応

JSON は実用スクリプティングの基本。`to-json` は動くが `from-json` は数値/bool/null のみ。

- [ ] grammar の量指定子付き名前付きキャプチャ (`<value>* % ','`) を action に正しく伝搬
- [ ] `+@$<str>` 等のリスト強制が action メソッド内で動作
- [ ] `from-json('{"key": "value"}')` が動く

### P2: 関数呼び出しパフォーマンス

再帰 fib が raku の 79 倍遅い。起動速度の優位性を活かすために計算性能も改善が必要。

- [ ] 単純な positional-only 関数の軽量呼び出しパス
- [ ] 環境 HashMap の clone 回避 (save/restore 方式)
- [ ] 関数解決のキャッシュ
- [ ] 目標: 79x → 10x 以下

### P3: Container semantics

roast で最も多い失敗原因。実用コードでも予期しないバグの原因。

- [ ] `@a[0] = 42` 等の代入が正しく動く
- [ ] Scalar コンテナの生成・束縛が raku 互換
- [ ] 関連 roast テストの通過率改善

### P4: warn の位置情報

Phase 1 の残り。

- [ ] `warn "msg"` にファイル名・行番号を表示

### P5: バイナリ配布・インストール

- [ ] mise GitHub バックエンドでのインストール検証
- [ ] `mise install mutsu` が動く
- [ ] リリースタグ作成の自動化

### P6: 他モジュール互換性

- [ ] File::Temp, File::Directory::Tree 等の互換性確認
- [ ] 足りない機能の特定と実装

### P7: 配列操作パフォーマンス

map/grep/sort が 46 倍遅い。データ処理系スクリプトでボトルネック。

- [ ] map/grep のイテレーション最適化
- [ ] sort の比較関数呼び出し最適化

## メトリクス

| 指標 | 2026-05-06 | 現在 |
|------|-----------|------|
| Whitelist | 1103 | 1148 |
| エラーに行番号 | なし | ✅ |
| .backtrace | 未実装 | ✅ オブジェクト |
| Did you mean? | なし | ✅ |
| JSON::Tiny to-json | 不可 | ✅ |
| JSON::Tiny from-json | 不可 | ⚠️ 部分的 |
| 起動時間 vs raku | 未計測 | 0.04x (25倍速い) |
| fib(25) vs raku | 未計測 | 79x 遅い |
