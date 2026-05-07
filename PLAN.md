# PLAN.md — mutsu ロードマップ

## 方針

**実用的な Raku インタープリタとしての品質向上**を最優先とする。
roast は回帰検知に使うが、セカンドプライオリティ。

過去の実装状況は [news/](news/) を参照。

## 優先順位

### 1. 関数呼び出しパフォーマンス 🔥 最優先

再帰 fib が raku の 79 倍遅い。関数呼び出しは map/grep/sort の内部でも使われるため、ここを直すと全体に波及する。

- [ ] positional-only 関数の軽量呼び出しパス
- [ ] 環境 HashMap の clone 回避 (save/restore 方式)
- [ ] 関数解決キャッシュ
- 目標: 79x → 10x 以下

### 2. from-json 完全対応

JSON は実用スクリプティングの基本。配列は動くようになった。残りは文字列とオブジェクト。

- [x] `from-json('[1,2,3]')` が動く
- [ ] `from-json('"hello"')` が動く (string token の named_quantified 伝搬)
- [ ] `from-json('{"key": "value"}')` が動く

### 3. Container semantics

roast で最も多い失敗原因。実用コードでも「動くと思ったら動かない」体験の原因。

- [ ] Scalar コンテナの生成・束縛が raku 互換
- [ ] `@a[0] = 42` 等の代入が正しく動く
- [ ] 関連 roast テストの通過率改善

### 4. その他

優先度は低いが、機会があれば対応する。

- **warn の位置情報**: `warn "msg"` にファイル名・行番号を表示
- **バイナリ配布**: mise GitHub バックエンドでのインストール検証
- **他モジュール互換性**: File::Temp 等
- **配列操作パフォーマンス**: map/grep/sort (関数呼び出し高速化で改善される見込み)

## メトリクス

| 指標 | 現在 |
|------|------|
| Whitelist | 1148 / 1296 |
| 起動時間 vs raku | 0.04x (25倍速い) |
| fib(25) vs raku | 79x 遅い |
| JSON::Tiny to-json | ✅ |
| JSON::Tiny from-json | ⚠️ 配列OK、文字列/オブジェクト未 |
