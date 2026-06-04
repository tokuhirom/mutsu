# PLAN.md — mutsu 2026年ロードマップ

## 方針

**実用的な Raku インタープリタ**として使ってもらえる品質を目指す。
起動 25 倍速い強みを活かし、CLI ツール・スクリプティング用途をメインターゲットとする。
最終的には **mutsu でウェブブログが作れる** レベルのライブラリ互換性を実現する。

過去の実装状況は [news/](news/) を参照。
パフォーマンス詳細は [PERFORMANCE.md](PERFORMANCE.md) を参照。
roast ブロッカー分析は [TODO_roast/BLOCKERS.md](TODO_roast/BLOCKERS.md) を参照。

---

## 🔴 最優先: バイトコード VM をちゃんと治す

**roast テストを1件ずつ潰すより、VM アーキテクチャの根本改修を最優先する** (ユーザー方針 2026-06-03)。

mutsu の「バイトコード VM」は実態として tree-walking Interpreter の薄いフロントエンドであり、
VM は Interpreter を共有実行状態コンテナ + フォールバック先として使っている
([ANALYSIS.md](ANALYSIS.md) §1)。これを **strangler-fig 方式**で段階的に切り離す:
古いフォールバックを残したまま計測し、毎 PR で「フォールバック率 X%→Y%」を可視化しながら縮める。
進捗台帳は [docs/vm-decoupling.md](docs/vm-decoupling.md)（dispatch）と
[docs/vm-dual-store.md](docs/vm-dual-store.md)（locals↔env）。

**これはアーキテクチャ改善 = 結合削減が目的であり、パフォーマンス改善ではない**（perf は副次的、
fib が速くなるかでは判断しない。ユーザー方針 2026-06-04）。CI（`make test` + 包括的 `make roast`）が
全マージをゲートするので、本質的リファクタは小さく刻みすぎず**大胆に**やり、CI を安全網にする
（CLAUDE.md「Refactor boldly」）。

### 計測（done）
- [x] メソッド/関数ディスパッチのフォールバック率計測 + **関数名・メソッド名別 histogram**
      (`MUTSU_VM_STATS=1`、PR #2571/#2601/#2604)。どの builtin/method がフォールバックしているかを
      推測でなくデータで特定できる。

### レバー A: ディスパッチのフォールバックを native 化（フォールバック率を下げる）
- [x] **`sprintf`/`zprintf`** を VM native テーブルへ (#2601)。sprintf.t の function fallback 96%→3.5%、
      実スクリプト 0%。純粋 builtin の典型。
- [x] **属性アクセサ読み** `$obj.x` を mut/非mut 両 opcode で native 化 (#2604)。method-call.raku 60%→20%。
- [ ] **`.new`（デフォルトコンストラクタ）** = 残る method fallback の支配項。native 化は実質
      「クラスレジストリ/BUILD/TWEAK/属性デフォルト/型強制を VM 所有データへ移す」大物（§1.1）。
      まず「カスタム new/BUILD/TWEAK なし・単純属性」のデフォルト構築 fast path から段階的に。
- [ ] **クロージャ/正規表現を取るメソッド** (`sort` の比較子, `.subst`, `map`/`grep` のブロック)。
      VM→コンパイル済みコードへのコールバック基盤が要る。
- [ ] **Test 関数** (`is`/`ok`/`plan`/`is-deeply`/`subtest`…) = roast の残フォールバックの大半。
      TAP `TestState` を VM から到達可能にする必要（大物・後回し）。
- [ ] `EVAL` / symbolic deref / `CALLER::` は本質的に interpreter 経由でよい（reflective）。

### レバー B: `locals`↔`env` 二重ストアの解消（共有状態結合の本丸, §1.2）
- [x] **Slice 1–4c**: 計測 + 環境の base-tier 化 + closure capture を free-var に限定 + 各種 format!/Symbol
      churn 除去（docs/vm-dual-store.md 参照）。
- [x] **Slice 5 step 1** (#2608): slot-only local（GetLocal でしか読まれない）を Interpreter env に
      ミラーしない。`ensure_env_synced` を `needs_env_sync` でゲート。ループ/ブロックは制御テンポラリの
      env 往復のため保守的に全 sync。
- [ ] **Slice 5 collapse proper（次の本丸）**: per-call **scoped/overlay env**（呼び出しフレームごとの
      子スコープ、復帰で破棄）を導入。これで callee の env 書き込みが caller を汚さなくなり、
      param-bind の env 書き込み・`env_dirty` 起因の post-call pull・ループ保守フォールバックを一掃。
      `clone_env()`/dirty 追跡（env_dirty/locals_dirty/locals_dirty_slots）の撤廃へ。

### レバー C: クロージャ upvalue 化（§1.3）
- [x] **Slice 4a**: closure 存在時の保守的 `needs_env_sync.fill(true)` を撤廃、free-var 集合に限定。
- [ ] 自由変数を indexed upvalue として捕捉し、closure が親 env を名前で読むのをやめる。
      `&?BLOCK`/`__mutsu_callable_id` の setup 書き込みを共有 env から外す。B（scoped env）と連動。

### 最終ゴール
- [ ] メソッド/関数フォールバック率を 0%（Test/EVAL 等の本質的例外を除く）にし、
      Interpreter のメソッド/関数実行パスを削除。当面は残フォールバックに
      `// TODO: compile to bytecode` を付け負債を可視化。

**次の着手候補（優先順）:** B の scoped/overlay env（A の `.new` と C の upvalue の前提にもなる本丸）
→ A の `.new` デフォルト構築 → C の upvalue。

---

## Q2 (5〜6月): パフォーマンスと Container semantics

目標: **「簡単なスクリプトなら raku の代わりに使える」レベルに到達**

### メソッド呼び出しパフォーマンス (進行中)

- 現状: method-call 2.7x、bench-class 2.3x（目標: 2x 以下）
- 残りのボトルネックは env deep clone (~9μs/call)
- [ ] closure captures as indexed slots (Phase 3b) — env サイズ自体を削減

### Container semantics

- [x] `our $x` クラス属性のバインド (S12-attributes/class.t — tests 11-12 は既に pass、#2541 で 26/28 に改善)
- [x] 多次元構造のエレメントレベルバインド (nested.t — PR #2413 で 42/43 に改善)
- [x] `undefine` の aggregate 参照セマンティクス (undef.t — PR #2414 で 90/91 に改善)

### Exception types (高インパクト — 残り ~16 roast テストをブロック)

- [x] X::TypeCheck::Binding::Parameter, X::Assignment::RO 実装 (#2477)
- [x] X::Adverb 実装 (#2505)
- [x] X::PseudoPackage::InDeclaration 実装 (#2507)
- [x] X::Worry::Precedence::Range 実装 (#2502)
- [x] X::IllegalDimensionInShape, X::Comp::BeginTime 実装 (#2503)
- [ ] 残りの型付き例外 (X::Str::Numeric, X::Method::NotFound, X::Undeclared, X::Cannot::Lazy, X::EXPORTHOW::InvalidDirective 等)
- [ ] 詳細は [TODO_roast/BLOCKERS.md](TODO_roast/BLOCKERS.md) の "throws-like / Exception Types" セクション参照

### アーキテクチャ・正しさの修正 (高インパクト — [ANALYSIS.md](ANALYSIS.md) 由来)

コードベース精読で判明した根本的な正しさ・健全性の問題。Threading/Async (BLOCKERS 31件) の
最大ボトルネックに直結するため最優先。詳細・再現コマンドは ANALYSIS.md 各節を参照。

- [ ] **無限 Range の即時展開クラッシュを撲滅** (ANALYSIS §8.2) — `(a..=b).map(Value::Int).collect()`
      が src 全体 43 箇所で無ガード、`MAX_ARRAY_EXPAND` ガードは 9 箇所のみ。無限 Range で
      `capacity overflow` パニックしプロセスごと落ちる。展開サイトを単一ヘルパに集約しガードを一元化。
      `(1..Inf).grep(* %% 2)[^3]` がクラッシュ (raku は `(2 4 6)`)。
- [ ] **遅延リストを pull/Iterator モデルに統一** (ANALYSIS §8.1) — `grep` 等の eager 経路を
      `map`/`first`/`head`/`[]` と同じ遅延扱いに揃える。Seq/Range を真の遅延イテレータに。
- [ ] **並行 state 共有の修正** (ANALYSIS §8.3, §2.2) — `clone_for_thread` のスナップショットコピーを
      やめ、共有すべきレキシカル/state/global を `Arc<Mutex>` のライブセルとして真に共有する。
      `start` ブロック間で `$counter`/`state $n` が共有されない (mutsu 1/0、raku 4/3)。
- [ ] **`unsafe` の "single-threaded 前提" を是正** (ANALYSIS §2.3) — `Arc::as_ptr as *mut` での
      エイリアス書き換え 11 箇所がスレッド生成と矛盾し UB の余地。配列/ハッシュを共有セル化して撤廃。
- [ ] **VM の panic→`X::` 変換境界を `run()` に設置** (ANALYSIS §2.1, §5) — ユーザコード起因の
      Rust パニックを Raku 例外に変換し、プロセスクラッシュを防ぐ (Q4 「panic/crash を 0 に」の前倒し)。
- [ ] **正規表現のコンパイル済みキャッシュ導入** (ANALYSIS §8.4) — `Value::Regex(Arc<String>)` が
      毎マッチ再パース。実測 raku 比 8.6x 遅 (変数束縛でも改善せず)。パターン→構造のキャッシュを追加。

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

1. **Template::Mustache** — `.meta` メソッド等を修正してテスト通過
2. **HTTP::Server::Tiny** の依存モジュール群（HTTP::Parser, IO::Blob, HTTP::Status）
3. **HTTP::Server::Tiny** 本体
4. DB アクセス — pure Raku の簡易実装 or qqx ベースの SQLite wrapper

### その他モジュール

- [ ] File::Temp
- [ ] MIME::Base64 (pure Raku)
- [ ] File::Directory::Tree

### バイナリ配布

- [ ] mise GitHub バックエンドでのインストール検証
- [ ] GitHub Releases の自動化

### Roast 90% 突破

- [x] Whitelist → 1190+ (roast 90%) — 達成: 1218

---

## Q4 (10〜12月): 安定性とコミュニティ

目標: **他の人が試して「ちゃんと動く」と思えるレベル**

### 安定性

- [ ] エッジケースでの panic/crash を 0 にする（[ANALYSIS.md](ANALYSIS.md) §8.2 の Range 展開クラッシュ・
      §2.1 の panic→`X::` 変換境界。Q2 で着手済みなら継続）
- [ ] エラーメッセージの品質向上
- [ ] 制御フロー (`return`/`last`/`next`/`take`/`emit`) を `RuntimeError` god-struct から
      `enum Control` へ分離（[ANALYSIS.md](ANALYSIS.md) §2.4 — `result_large_err` 負債の解消）

### パフォーマンス Phase 2

- [ ] method-call を 1.5x 以下にする（closure captures indexed slots → NaN-boxing）
- [ ] bench-class を 1.5x 以下にする
- [ ] bench-fib (型制約付き) を 2x 以下にする
- [ ] NaN-boxing: Value を 72 bytes → 8 bytes に（Int/Num/Bool/Nil）
- [ ] JIT compilation (Cranelift) の検討
- [ ] Cycle collector (circular object references)

### ドキュメントとコミュニティ

- [ ] 「mutsu でウェブブログを作る」チュートリアル
- [ ] raku との互換性マトリクス公開
- [ ] WASM playground の公開

### Roast

- [x] Whitelist 1200+ 目標 — 達成: 1218

---

## Backlog: 未実装の言語機能

BLOCKERS.md の分析に基づき、インパクト順に並べたもの。

### Phasers

- [ ] Phaser rvalue caching (INIT/CHECK/BEGIN as rvalues in closures)
- [ ] PRE/POST phasers (contract programming)

### Type constraints / Signatures

- [ ] Signature type-checking enforcement (reject wrong-type args with X::TypeCheck)
- [ ] Native int/uint overflow and bounds checking
- [ ] Multiple signatures on a single sub

### OOP

- [ ] Namespaced class construction (`A::B.new`)
- [ ] `augment class` improvements (augmenting with new attributes)
- [ ] Parameterized role mixin

### Supply/Concurrency

- [ ] Supply backpressure
- [ ] `supply`/`react` block scoping issues
- [ ] Tap management (close, drain)

### IO / Process

- [ ] IO::Handle read modes (binary, encodings)
- [ ] Proc and Proc::Async completeness
- [ ] File test operators (`-e`, `-f`, `-d` etc.)

### Regex / Grammar

- [ ] Match object `.caps` / `.chunks`
- [ ] Lookbehind assertions (`<!after>`)
- [ ] `:Perl5` modifier edge cases

### Parser: 未実装演算子

- [ ] `ff` / `fff` — flipflop operators (8 variants)
- [ ] `==>` / `<==` — feed operators
- [ ] `minmax` — range from min to max
- [ ] `unicmp` / `coll` — Unicode/collation comparison
- [ ] `~&` / `~|` / `~^` / `~<` / `~>` — string buffer bitwise ops

### Parser: メタ演算子

- [ ] Generalized negation meta (`!op`) — beyond `!~~` and `!%%`
- [ ] Hyper assignment (`@a >>+=>> 1`)
- [ ] Triangular reduction (`[\+]`, `[\*]`, etc.)

### アーキテクチャ・リファクタ (中長期 — [ANALYSIS.md](ANALYSIS.md) §3, §4, §6)

VM↔Interpreter の切り離し本体は冒頭の「🔴 最優先」セクション参照。以下はそれ以外の構造的負債。

- [ ] 正規表現の validator/matcher 二重実装を単一パーサに統合 (§3.1)
- [ ] `.^methods`/`.can` の型別メソッド一覧を実ディスパッチ表から導出 (§4)
- [ ] roast fudge ロジックを核から分離 / テストの一時ファイルを `tmp/` へ / 500行超ファイルの分割 (§6)

### Practicality (将来)

- [ ] REPL
- [ ] Debugger
- [ ] `zef` package manager compatibility
- [ ] Native binary output

---

## メトリクス

| 指標 | 現在 (5月末) | Q2 目標 | Q3 目標 | Q4 目標 |
|------|-----------|---------|---------|---------|
| Whitelist | **1218** ✅ | 1190+ ✅ | 1200+ ✅ | 1220+ |
| fib(25) vs raku | **1.0x** ✅ | <10x ✅ | <10x | <10x |
| method-call vs raku | **2.7x** | <2.5x | <2x | <1.5x |
| bench-class vs raku | **2.3x** | <2x | <1.5x | <1.5x |
| 起動時間 vs raku | 0.04x | 0.04x | 0.04x | 0.04x |
| JSON::Tiny | ✅ テスト全pass | ✅ | ✅ | ✅ |
| Template::Mustache | ⚠️ grammar action | - | ✅ | ✅ |
| HTTP::Server::Tiny | ❌ | - | ✅ | ✅ |
| 動作モジュール数 | 1 | 2 | 5+ | 5+ |
| mise install | ❌ | ❌ | ✅ | ✅ |
