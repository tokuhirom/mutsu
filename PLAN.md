# PLAN.md — mutsu 2026年ロードマップ

## 方針

**実用的な Raku インタープリタ**として使ってもらえる品質を目指す。
起動 25 倍速い強みを活かし、CLI ツール・スクリプティング用途をメインターゲットとする。
最終的には **mutsu でウェブブログが作れる** レベルのライブラリ互換性を実現する。

過去の実装状況は [news/](news/) を参照。

---

## Q2 (5〜6月): パフォーマンスと JSON

目標: **「簡単なスクリプトなら raku の代わりに使える」レベルに到達**

### 関数呼び出しパフォーマンス ✅

- [x] positional-only 関数の軽量呼び出しパス (#2229)
- [x] fib(25): 79x → 0.8x に改善（**raku より高速**）
- [x] Int/Num 算術 fast path、NFC skip、hash fast path、method cache 等 (#2341, #2342, #2343, #2344)
- [x] 6ベンチマーク中5つで raku より高速を達成

### JSON::Tiny 完全対応

- [x] `to-json` (#2216)
- [x] `from-json` 配列 (#2227)
- [x] `from-json` 文字列 (#2231)
- [x] `from-json` オブジェクト (#2233)
- [x] JSON::Tiny テストスイート全 pass (#2329: ignoremark, Scalar hash coercion, Uni binding)

### Container semantics

- [x] WhateverCode 複数引数 (#2232)
- [ ] Scalar コンテナの生成・束縛が raku 互換
- [ ] 関連 roast テストの通過率改善

---

## Q3 (7〜9月): ウェブアプリに必要なモジュール互換性

目標: **mutsu でウェブブログシステムが構築できる**

### ウェブブログに必要なスタック

| レイヤー | モジュール | 状態 | 備考 |
|----------|-----------|------|------|
| JSON | JSON::Tiny | ✅ テスト全 pass | #2329 |
| テンプレート | Template::Mustache | ⚠️ grammar action dispatch がブロッカー | proto regex in alternation の action 呼び出し |
| HTTP サーバー | HTTP::Server::Tiny | ❌ 依存未解決 | HTTP::Parser, IO::Blob, HTTP::Status |
| DB | (検討中) | ❌ | NativeCall 不可。JSON file / SQLite CLI wrapper |

### モジュール対応の進め方

1. **JSON::Tiny** テストスイート全 pass
2. **Template::Mustache** — `.meta` メソッド等を修正してテスト通過
3. **HTTP::Server::Tiny** の依存モジュール群（HTTP::Parser, IO::Blob, HTTP::Status）
4. **HTTP::Server::Tiny** 本体
5. DB アクセス — pure Raku の簡易実装 or qqx ベースの SQLite wrapper

### その他モジュール

- [ ] File::Temp
- [ ] MIME::Base64 (pure Raku)
- [ ] File::Directory::Tree

### バイナリ配布

- [ ] mise GitHub バックエンドでのインストール検証
- [ ] GitHub Releases の自動化

### Roast 90% 突破

- [ ] Whitelist → 1170+ (roast 90%)

---

## Q4 (10〜12月): 安定性とコミュニティ

目標: **他の人が試して「ちゃんと動く」と思えるレベル**

### 安定性

- [ ] エッジケースでの panic/crash を 0 にする
- [ ] エラーメッセージの品質向上

### パフォーマンス Phase 2

- [ ] 配列操作 (map/grep/sort) の最適化
- [ ] ベンチマークで全項目 10x 以内を目指す

### ドキュメントとコミュニティ

- [ ] 「mutsu でウェブブログを作る」チュートリアル
- [ ] raku との互換性マトリクス公開
- [ ] WASM playground の公開

### Roast

- [ ] Whitelist 1200+ 目標

---

## メトリクス

| 指標 | 現在 (5月) | Q2 目標 | Q3 目標 | Q4 目標 |
|------|-----------|---------|---------|---------|
| Whitelist | 1182 | 1185+ | 1190+ | 1200+ |
| fib(25) vs raku | **0.8x** ✅ | <10x ✅ | <10x | <10x |
| 起動時間 vs raku | 0.04x | 0.04x | 0.04x | 0.04x |
| JSON::Tiny | ✅ テスト全pass | ✅ | ✅ | ✅ |
| Template::Mustache | ⚠️ grammar action | - | ✅ | ✅ |
| HTTP::Server::Tiny | ❌ | - | ✅ | ✅ |
| 動作モジュール数 | 1 | 2 | 5+ | 5+ |
| mise install | ❌ | ❌ | ✅ | ✅ |
