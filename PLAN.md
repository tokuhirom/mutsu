# PLAN.md — mutsu 2026年ロードマップ

## 方針

**実用的な Raku インタープリタ**として使ってもらえる品質を目指す。
起動 25 倍速い強みを活かし、CLI ツール・スクリプティング用途をメインターゲットとする。

過去の実装状況は [news/](news/) を参照。

---

## Q2 (5〜6月): パフォーマンスと JSON

目標: **「簡単なスクリプトなら raku の代わりに使える」レベルに到達**

### 関数呼び出しパフォーマンス 🔥

fib(25) が raku の 79 倍遅い。map/grep/sort にも波及するため最優先。

- [ ] positional-only 関数の軽量呼び出しパス
- [ ] 環境 HashMap の clone 回避 (save/restore 方式)
- [ ] 関数解決キャッシュ
- 目標: 79x → 10x 以下

### from-json 完全対応

- [x] `from-json('[1,2,3]')` — 配列
- [ ] `from-json('"hello"')` — 文字列
- [ ] `from-json('{"key": "value"}')` — オブジェクト

### Container semantics

roast で最も多い失敗原因。

- [ ] Scalar コンテナの生成・束縛が raku 互換
- [ ] `@a[0] = 42` 等の代入

---

## Q3 (7〜9月): モジュール互換性とエコシステム

目標: **実在する pure-Raku モジュールが 5 個以上動く**

### モジュール互換性

- [ ] JSON::Tiny 完全動作 (to-json + from-json)
- [ ] File::Temp
- [ ] File::Directory::Tree
- [ ] MIME::Base64 (pure Raku)
- [ ] Template::Mustache or similar
- 動かないモジュールごとに足りない機能を特定し、汎用的に実装

### バイナリ配布

- [ ] mise GitHub バックエンドでのインストール検証
- [ ] `mise use mutsu` で入る状態にする
- [ ] GitHub Releases の自動化 (タグ push → バイナリ生成)

### Roast 90% 突破

- [ ] Whitelist 1148 → 1170+ (roast 90%)
- [ ] Container semantics 改善で fail テスト減少

---

## Q4 (10〜12月): 安定性とコミュニティ

目標: **他の人が試して「ちゃんと動く」と思えるレベル**

### 安定性

- [ ] エッジケースでの panic/crash を 0 にする
- [ ] エラーメッセージの品質向上 (残りの X::* 例外クラス)
- [ ] warn の位置情報

### パフォーマンス Phase 2

- [ ] 配列操作 (map/grep/sort) の最適化
- [ ] ベンチマークで全項目 10x 以内を目指す

### ドキュメントとコミュニティ

- [ ] 「mutsu で書く CLI ツール」チュートリアル
- [ ] raku との互換性マトリクス公開
- [ ] WASM playground の公開 (既存の wasm-demo を拡張)

### Roast

- [ ] Whitelist 1200+ 目標
- [ ] 残りの主要な言語機能ギャップを埋める

---

## メトリクス

| 指標 | 現在 (5月) | Q2 目標 | Q3 目標 | Q4 目標 |
|------|-----------|---------|---------|---------|
| Whitelist | 1148 | 1155+ | 1170+ | 1200+ |
| fib(25) vs raku | 79x | <10x | <10x | <10x |
| 起動時間 vs raku | 0.04x | 0.04x | 0.04x | 0.04x |
| JSON::Tiny | to-json✅ | 完全✅ | 完全✅ | 完全✅ |
| 動作モジュール数 | 1 | 1 | 5+ | 5+ |
| mise install | ❌ | ❌ | ✅ | ✅ |
