# PLAN.md — mutsu ロードマップ

## 方針

**実用的な Raku インタープリタとしての品質向上**を最優先とする。
roast は回帰検知に使うが、セカンドプライオリティ。

過去の実装状況は [news/](news/) を参照。

## 現在の課題

### P1: from-json 完全対応

- [ ] grammar の量指定子付き名前付きキャプチャを action に正しく伝搬
- [ ] `from-json('{"key": "value"}')` が動く

### P2: 関数呼び出しパフォーマンス

再帰 fib が raku の 79 倍遅い。目標: 10x 以下。

- [ ] positional-only 関数の軽量呼び出しパス
- [ ] 環境 HashMap の clone 回避
- [ ] 関数解決キャッシュ

### P3: Container semantics

roast で最も多い失敗原因。実用コードでも予期しないバグの原因。

- [ ] Scalar コンテナの生成・束縛が raku 互換
- [ ] 関連 roast テストの通過率改善

### P4: warn の位置情報

- [ ] `warn "msg"` にファイル名・行番号を表示

### P5: バイナリ配布

- [ ] mise GitHub バックエンドでのインストール検証
- [ ] リリースタグ作成の自動化

### P6: 他モジュール互換性

- [ ] File::Temp 等の互換性確認・修正

### P7: 配列操作パフォーマンス

map/grep/sort が 46 倍遅い。

- [ ] map/grep/sort のイテレーション最適化

## メトリクス

| 指標 | 現在 |
|------|------|
| Whitelist | 1148 / 1296 |
| 起動時間 vs raku | 0.04x (25倍速い) |
| fib(25) vs raku | 79x 遅い |
| JSON::Tiny to-json | ✅ |
| JSON::Tiny from-json | ⚠️ 部分的 |
