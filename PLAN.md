# PLAN.md — mutsu の実装計画

> このファイルには **未完了の作業だけ** を載せる。完了した作業は [news/](news/) に移す。
> 過去ログは [news/](news/)、性能の詳細は [PERFORMANCE.md](PERFORMANCE.md)、
> roast の失敗分析は [TODO_roast/BLOCKERS.md](TODO_roast/BLOCKERS.md) を参照。
>
> **最終更新 2026-06-21**:
> 単一ストア化と tree-walking 撤去が完了したため、計画を「残作業中心」の構成に整理した。
> 完了済みの大型キャンペーンの詳細は `news/2026-06.md` に移した。

## この文書の読み方

- まず **§1 現状** を読むと、「大きな基盤工事はどこまで終わったか」が分かる。
- 次に **§2 substrate** を見ると、「順序依存で、先に片づけるべき残件」が分かる。
- **§3 並列実装可能** は、別ブランチで進めやすい作業の一覧。
- 具体的な roast 失敗の対応順は [TODO_roast/BLOCKERS.md](TODO_roast/BLOCKERS.md) を参照。

## 方針

目標は、**実用的な Raku インタープリタ**として使える品質に持っていくこと。
起動が速い強みを活かし、まずは CLI ツールとスクリプト実行を主戦場にする。
最終的には、**mutsu でウェブアプリやブログを組める程度のライブラリ互換性**を目指す。

### フェーズ構成と次の大型ジャンプ（ADR-0001）

性能と互換性で raku に追いついたその先、**GC と JIT が次の大型ジャンプ**になる。
GC のないインタプリタは「欠陥品」とみなされ誰も使わない＝GC は table stakes。順序と方式は
[docs/adr/0001-gc-strategy-and-phasing.md](docs/adr/0001-gc-strategy-and-phasing.md) で決定済み。要点:

| フェーズ | 内容 | 本書の該当 |
|---|---|---|
| **A. 追いつく** | 互換性＋速度で raku に並ぶ（**いまここ**） | §F roast / §2 multi-dispatch / §H module / §G method-call hot-path・Lever 3 |
| **A'. 地ならし** | root 集約で GC 実装を楽にする | レキシカルスコープ（ANALYSIS §1.4）/ upvalue index 化（ANALYSIS §1.3） |
| **B. Value 表現リワーク＋GC** | Track B（要素 cell 化）＋ cycle collector を**統合**（ADR 層3a） | §G Lever 2 周辺 / §I Track B 依存項目 / §J |
| **C. JIT** | 独自メリット | §G Lever 4 |

- **GC は JIT の前**（JIT を GC 前提の上に載せる）。**GC は Phase A 完了後**に着手（当面は §J の将来扱い）。
- **方式 = cycle collector on Arc（non-moving + refcount・レベル1 採用）**。スカラ系は型フィルタで GC 対象外
  ＝数値/文字列 hot path はコスト 0。性能は GC でなく JIT で稼ぐ。
- **Track B は GC と一体（層3a）。単独で先行着手しない。** NaN-boxing は JIT の地ならし（層3b）、
  biased refcount は層3c。
- Level 1 の詳細設計メモは [docs/gc-level1-detailed-design.md](docs/gc-level1-detailed-design.md) を起点にする。

### 🚫 標準ルール: 「1 操作 = 1 実装」を守る（ユーザー方針 2026-06-07）

実行エンジンは単一の `Interpreter` 構造体（= bytecode VM）に統合済み。
同じ Raku 操作を**複数箇所に重複実装しない**:

1. 新規実装・修正は VM/native 層（`src/vm/` ＋ pure native `src/builtins/`）に **1 回だけ**書く。
2. 呼び出し経路（EVAL / 正規表現の埋め込み `{}` ブロック等）が同じ処理を要するときは、単一の native 実装へ**委譲**する。
3. 重複を見つけたら native 実装を正本にして、重複コピーを削除する。

---

## 1. 現状 — 基盤工事はほぼ完了、残りは機能拡張と最適化

大きな土台作業だった以下は **完了済み**:

- VM/Interpreter の境界整理
- 単一ストア化
- tree-walking interpreter の実行経路撤去
- 第一級コンテナと状態所有まわりの主要キャンペーン

詳細は [news/2026-06.md](news/2026-06.md) と末尾の「✅ 完了した大型キャンペーン」を参照。

現時点で残っているのは次の 2 系統だけ:

- **順序依存の残件**: §2 の multi-dispatch まわり
- **並列で進められる残件**: §3 の roast、性能改善、モジュール互換

つまり、もう「設計の方向性が未確定」という段階ではない。
残りの中心は、個別機能の実装、既知のギャップ埋め、そして性能改善。

---

## 2. 🔴 substrate（順序依存の基盤作業）— 大半は完了、残りは multi-dispatch だけ

完了済みの substrate は次の通り（詳細は `news/2026-06.md`）:
- **A. 単一ストア化**（locals↔env 統合）✅ — reverse pull 撤去 #3354 → boxing 恒久ON #3450 → `env_dirty` 物理削除 #3455。`locals` 単一権威・`env` 派生ビュー。
- **B. tree-walking interpreter 撤去** ✅ — struct 統合 #3075-3104 ＋ `run_instance_method_resolved` 非-delegation arm（~470行 `run_block`）削除 #3664-3680。**bytecode VM がユーザメソッド body の唯一の実行エンジン**。
- **C. 第一級コンテナ Phase 2 + Phase 3 Stage 0-2c** ✅ — SlotRef→`HashEntryRef` 統合 #3472・grep-rw-view 撤去 #3466・scalar/named-param 共有。Phase 3 Stage 3（escape-aware cell 省略）は perf 未正当化で **deferred**（実質ゼロ残）。
- **D. 状態所有（②③）** ✅ — レジストリ所有 #2760-2772 / IO native メソッド族 #3499-3511 / 組込型 ctor の native 化＝③ ctor フォーク完了 #3514-3536（capstone IO::Socket::INET）/ (b) tree-walk dispatch chain 削除（=B）。

### D. multi-dispatch の VM 化 — 残るのはフォールバック除去だけ

ここは、substrate として唯一まだチェックボックスが残っている項目。

すでに実装済みの主な内容（詳細は `news/2026-06.md`）:
- proto sub trivial body #3541
- where 制約候補 #3543
- default-param #3559
- 非 trivial proto body #3550 / #3552
- `{*}` rw-redispatch #3556
- **nextsame + rw チェーン**（§D の capstone）
- `&`-code param
- 非 builtin module / dynamic single sub
- imported test-assertion sub
- **sound multi resolution cache #3684**

- [ ] **残件**:
  bare multi の残フォールバック除去。
  `@_` slurpy recursive sub は別カテゴリで、`@a[1..*]` 再帰の immutable-List bug は §F 扱い。
  `code_signature`（`&cb:(Int)`）param を持つ候補の OTF 化。
  これは resolution ambiguity とは別の軸。
  default-param OTF の builtin-shadow 単一候補。
  name-cache 汚染リスクがあるため、いまは意図的に除外を維持している。
  モジュール sub OTF に残っている interpreter 結合構文。
  対象は `state` / `EVAL` / `CATCH` / sigilless / `rw` / `raw` / code-sig / sub-sig / 戻り型 coercion。
  現在は保守ゲートで除外しており、本筋は `compiled_fns` の拡充。

---

## 3. 🟢 並列実装可能（独立・相互にブロックしない）

> ここにある項目は §2 の本筋と直接は衝突しない。
> 別ブランチで並行に進めやすい。
> 着手前に、対応する BLOCKERS や memory のメモを確認すること。

### F. roast backlog（[TODO_roast/BLOCKERS.md](TODO_roast/BLOCKERS.md) 駆動・インパクト順）

現状 whitelist は **1285**。
診断には `./scripts/roast-history.sh` を使う。
出力は `tmp/roast-{panic,timeout,error,fail,pass}.txt` に保存される。

- [ ] **★型付き例外（最高インパクトの単一ファイル）**: `S32-exceptions/misc.t`（42/157）。X::NotParametric /
      X::Undeclared / X::Redeclaration / X::Bind / X::TypeCheck 他 ~25 種の one-off 型実装。BLOCKERS.md §B。
- [ ] **★lazy-seq（残 ④のみ・2026-06-29 更新）**: `1..*` 配列は reify-on-index 済、`(1...Inf)`=Seq is-lazy=True。
      ①`.List`/`.Array` coercion が laziness を保持＝**解決済**（`.List.is-lazy`/`.Array.is-lazy`=True）。
      ②`eqv` 両-lazy-同型 `X::Cannot::Lazy`＝**解決済**（eqv.t whitelist 済）。③Seq single-pass consumption（`X::Seq::Consumed`）
      ＋ `+a`/`+@a` single-arg-rule 型保持＝**解決済 #3919**（slurpy-params.t whitelist・`for`-loop が `Value::Seq` を `seq_consume`・
      `+a` Seq→Seq/`+@a` Seq→List）。残 ④のみ＝reify-on-mutation／slurpy 真 lazy（`flatten_into_slurpy` 無限展開→hang）／
      lazy `.gist`（`docs/lazy-arrays.md` L3/L4・doc TL;DR は stale）。BLOCKERS.md §「Real lazy infinite sequences」。
- [ ] **Match キャプチャ番号付け / コンテナ kind**: (1) `$<x>=(...)` が positional スロットにも重複格納され番号がずれる、
      (2) `m:g//` を `my @m` 代入後 `@m.gist` が `(…)` を返す（receiver の List-kind dual-store）。S05-capture/array-alias.t（30/37）。
- [ ] 未実装演算子（2026-06-27 再調査）: `ff`/`fff`（flipflop 8 variants）は **完了**（flip-flop.t 40/40 PASS）。
      `==>`・`<==` feed precedence は **完了**（`Expr::Feed` 遅延ノード化＋宣言/代入 split で `=` を `==>` より強く結合させた。
      `my @a = X ==> map` → `(my @a = X) ==> map`。括弧/`do{}` は隔離。t/feed-operators.t 24 で担保）。残ギャップ＝(1) 改行を
      またぐ複数行 feed（行頭 `==>`・`parse_list_infix_loop` の `!ws_before.contains('\n')` ガードが阻む）(2) `==>>`/`<<==` は
      rakudo 自身も未実装。`~<`・`~>`（string bitwise shift）は **rakudo 自身が "not yet implemented"**・仕様未確定（着手不可）。
- [ ] メタ演算子: generalized negation meta（`!op`）/ hyper assignment（`@a >>+=>> 1`）。
- [ ] Phasers: rvalue caching（INIT/CHECK/BEGIN as rvalues）/ PRE/POST（contract programming）。
- [ ] Signatures: type-check enforcement（X::TypeCheck）/ native int/uint overflow・bounds / 単一 sub の複数シグネチャ。
- [ ] OOP: namespaced construction（`A::B.new`）/ `augment class` / parameterized role mixin。
- [ ] Supply/Concurrency: backpressure / `supply`・`react` block scoping / Tap management（close, drain）。
- [ ] IO/Process: IO::Handle read modes（binary/encodings）/ Proc・Proc::Async 完全化 / file test operators（`-e`/`-f`/`-d`）。
- [ ] 孤立サブシステム（main-track 非衝突・BLOCKERS.md §A）: 残 regex（S05-substitution/match capturing-contexts）・
      Unicode CollationTest・shaped arrays・Pod。

### G. perf — 起動／実行速度（計測駆動・MUTSU_VM_STATS / timed roast）

現状（2026-06-28 再計測・PERFORMANCE.md）: 起動 0.04x。method-call ホットパスキャンペーンで
method-call 0.44→**0.24s（~45%）**、bench-class 0.80→**0.54s（~33%）**（release・machine-quiet・best-of-7）。
**重要訂正**: 旧「真因＝env deep clone ~9μs/call」は**もう成立しない**。単一ストア化（#3450 boxing
恒久ON / #3455 env_dirty 物理削除）で per-call の env deep clone は撤廃済み（`MUTSU_VM_STATS` で
`env_deep_copies≈0`／run 全体で 1–2 回のみ）。よって旧 Lever 1（env deep clone 撤廃）は**達成済み＝対象消滅**。

- [x] ~~**Lever 1: closure captures を indexed slot 化（env deep clone 撤廃）**~~ — 単一ストア化で解消済み（計測で確認）。
- [ ] **method-call ホットパス整理（計測駆動）= method-call <2x 継続**:
      - [x] per-call body-fingerprint 撤去（#3853）: `push_method_dispatch_frame` が候補同定のため毎回メソッド body
            AST 全体を `format!("{:?}")` 化していた→単一候補早期 return ＋ alloc-free fingerprint。
      - [x] 非-multi resolution cache（#3857）: compiled-mut の `resolve_method_cached` に monomorphic
            `last_method_resolve` ＋ `method_resolve_cache` を追加（compiled-interpret と整合）。毎回の MRO/specificity
            walk を除去。受け手 Symbol 再利用で `Symbol→resolve()→intern()` round-trip も削減。
      - [x] mro_readonly キャッシュ（#3859・**最大の単独 win**）: `mro_readonly`(&self) が BFS で毎回 ancestor を
            再構築（HashSet+queue＋O(N²) string clone・`.new` で 3 回呼ばれる）→ `class_def.mro` 非空なら直接返す
            （BFS 結果と証明的に等価）。
      - [x] FxHashMap で SipHash 撤去（#3867）: `Env` の `Symbol→Value` オーバーレイ（全変数アクセスの最ホット
            マップ）＋ dispatch キャッシュ群（`fast_method_cache`/`method_resolve_cache`/`multi_resolve_cache`/
            `multi_type_cacheable`・`(Symbol,Symbol)` キー）を `rustc_hash::FxHashMap` に。perf で ~7% が
            `SipHasher::write`/`hash_one` だった（整数キーに暗号強度ハッシュ）。命令数 method-call -7.3% /
            bench-class -4.2%（単一 P-core 固定・決定的計測）。
      - [x] 単一候補ディスパッチのキャッシュ（#3870）: `push_method_dispatch_frame` が毎回 `resolve_all_methods_with_owner`
            （MRO walk＋registry String lookup＋arg-match＋`MethodDef` クローン）を実行し ≤1 候補で早期 return していた。
            `(class,method)→構造的候補 ≥2 か` を FxHashMap で memoize（arg-match 前のオーバーロード数・クローンなし）。
            ≤1 なら resolve を丸ごとスキップ。健全性＝arg-match は候補数を減らすだけ＋registry 形状のみ依存。命令数
            method-call -5.2% / bench-class -3.5%。**#3867 と合算で method-call -12.2% / bench-class -7.6%。**
      - [ ] **次: symbol intern round-trip の残り（大型リファクタ）**: dispatch hot path の `cn = class_sym.resolve()`
            String 確保（perf の malloc ~17% の一角）が downstream の `&str` API（`call_compiled_method`/
            `push_method_dispatch_frame`/`check_method_wrap_chain`＋ registry の String キー lookup）のため残る。
            `with_str` の長期保持はグローバル symbol-table read-lock デッドロックで不可＝registry/dispatch API を
            Symbol キー化する必要。`Symbol::intern(method)` も毎回 ~3.5%（method 名は CompiledCode 定数の `Value::Str`・
            事前 intern は precomp 直列化に波及）。
      - [ ] `Value` clone/drop（bench-class の ~50%）＋ `attributes.to_map()` の毎回クローン ＋ `call_compiled_method`
            の属性キー `format!`（`!x`/`.x`/`@!x`… を attr×6・毎回）＋ instance 構築 HashMap ＋ `merge_method_env` は
            Lever 2（NaN-boxing・GC 後）待ち、ないし属性 materialization の作り直し（深い）。
- [ ] **Lever 2: NaN-boxing（高 payoff・設計済）= ADR-0001 層3b（JIT の地ならし・GC 後）**: `Value` 48→8 bytes
      （Int/Num/Bool/Nil を NaN payload に）。int-arith 2x・fib ~30% 狙い。8B 固定・タグ単純で JIT 生成が楽に
      なる（型境界は GC の型フィルタと一致）。`value_size_guard` テストでサイズ監視中。
- [ ] **Lever 3: threaded dispatch（中 payoff・ラフ）**: opcode の `match` を関数ポインタテーブルに。命令律速ベンチ 10–30%。
- [ ] **Lever 4: JIT（Cranelift）= ADR-0001 層4（GC の後）/ Lever 5: 型制約チェックの tight-loop 省略**（ラフ・大）。
      GC を cycle collector on Arc（non-moving + refcount）にする決定で、JIT は stack map/forwarding/write barrier
      不要・`Arc` inc/dec を emit するだけになる。intループのネイティブ化で hot path は GC/refcost ゼロ（ADR-0001 §3-8）。
- [ ] **Lever 6: biased reference counting = ADR-0001 層3c（GC 後の独立 perf）**: 所有スレッドからの refcount を
      非 atomic 化。JIT が intループをネイティブ化すれば hot path から refcount が消えるため優先度は低め。
- [ ] 正規表現: 量指定子反復ごとの `RegexCaptures.clone()` 削減。
- 目標: method-call <1.5x、bench-class <1.5x、bench-fib（型制約付き）<2x。

### H. モジュール互換（Q3 — ウェブブログスタック）

目標: **mutsu でウェブブログシステムが構築できる**。**Template::Mustache 完動（#3395）**。
HTTP スタック/JSON/DB/ユーティリティは下記調査の通り NativeCall 非依存で動作可能、各々独立した一般機能の欠落待ち。

#### ★ mzef — 実 Zef を同梱した `mzef` パッケージマネージャ（north-star・ユーザー方針 2026-06-28）

ビジョン: **mutsu をインストールすると `mzef` コマンドが使える**。実装は**reimplement せず Zef そのもの**（upstream）を使う。
zef が動けば Raku エコシステム全体に接続でき、PLAN の「ウェブブログを mutsu で組む」に直結する。zef は巨大な実 Raku
プログラムなので、**最強の互換性北極星**でもある（2026-06-28 セッションだけで zef 由来の一般バグを 12 件 landed＝下記「✅ 完動」
末尾参照。すべて zef 専用ハックでなく一般改善）。

到達点と段階ロードマップ:
1. **✅ CLI ロード＋コマンド dispatch（2026-06-28・完了）**: zef 全 36 モジュールがパースし、コアスタック
   （Distribution/Client/Install/Repository/CLI）がロード、`use Zef::CLI` で **`zef --help` が完全な usage を出力・`--version` が実行**。
   `proto MAIN(|) is export` の exported-MAIN dispatch（#3860）が capstone。検証手順＝memory `zef-load-status`。
2. [ ] **ローカル完結コマンドの仕上げ**: `--help`/`info`/`locate` 等を完全動作。2 件の MAIN ディスパッチバグを修正済み:
   (a) `where .so` no-args 誤マッチ（**#3866**）＝named param の `where` 制約を**省略時にも default/type-object 値に対して
   評価**するよう `binding_signature`（バインド経路）と `args_matching`（候補選択経路）両方を直した。`multi sub
   MAIN(Bool :version($) where .so)` は引数無しで発火しない（topic が `Bool` type object・`.so`=False で棄却）。
   (b) **リテラル位置引数の無視**（#TBD）＝`multi MAIN('info', *@ids)` 等の literal 候補が第1引数の値に関係なく同一 arity を
   貪欲マッチしていた。MAIN 候補選択ループ（`main_args.rs`）に literal_value 照合を追加（CLI 文字列を param 型へ coerce
   してから比較・数値 literal 対応）。`zef other` がもう `info` 候補を発火しない。担保＝`t/main-literal-positional-dispatch.t`。
   既知の別軸残件: CLI 数値文字列の `Int $n` への coerce が raku より積極的（`MAIN(Int $n,…)` に `7` がマッチ・raku は
   slurpy fallback）。literal とは独立で、実用上は mutsu 側のほうが直感的。
3. [ ] **★本丸: mutsu ネイティブ `CompUnit::Repository`**: install 済みモジュールが `use` できる橋。mutsu は独自の
   MUTSULIB ロードを持つので、それを zef の install 先として見せる薄い CompUnit 層を作る。**precomp は MoarVM の
   `.precomp` シリアライズ依存＝DBDish の `MoarVM::Guts::REPRs` と同じ MoarVM 内部の壁**なので、precomp 無しの
   source-only リポジトリとして設計する。
4. [ ] **network fetch**: fez エコシステム（`https://360.zef.pm/`）への取得。堅牢な async TLS が前提。
5. [ ] **実 install＋build/test 実行**、`mzef` バイナリ shim ＋ zef 本体＋依存＋config の vendoring
   （debian の zef は `resources/bin/zef` 欠落・要 known-good vendoring）。

切り分け: **「実 Zef をテスト標的として走らせ続ける」は即効性が高く継続**、**「同梱 installer として実 install まで」は段階3
（CompUnit ネイティブ層）が本丸**。DBDishLite 同様の薄い互換層アプローチも併用可。

#### ★ 動作モジュール回帰 CI（PR 非ブロック・main push 時検知・ユーザー方針 2026-06-28）

- [ ] **目的**: 一度「動いた」外部モジュール（HTTP::Server::Tiny / HTTP::Parser / MIME::Base64 / File::Temp /
      Template::Mustache / NativeLibs / DBDish::SQLite::Native / Zef::* など）が**動き続けているか**を継続検知する。
- [ ] **方針**: **PR をブロックしない**（重い・外部 dist 依存・network 可能性）。**main への push 時**に走らせ、退行したら
      気づけるだけでよい（post-merge / scheduled workflow）。`make test`/`make roast` の PR ゲートとは別軸。
- [ ] **設計案**: 各既知モジュールを `use`＋スモーク（ロード・代表 method・`zef --help` 出力一致）するハーネス。zef 等の
      外部 dist は CI で `zef fetch` するか vendoring。失敗はレポートのみ（赤でも main は止めない運用）。記録は news/。

**✅ Tubu — 同期 Sinatra/P6W ウェブフレームワーク完動（2026-06-24）**: `t/lib/Tubu*`（pure Raku・`get`/`post`/path
param/query/form param/cookie/before フック/json/html/redirect/静的ファイル/同期 `IO::Socket::INET` runner）。CI-safe
統合テスト＝`t/tubu-web-framework.t`。実ソケットで curl 応答も実証。現実的ブログ（Tubu + DBDishLite/SQLite + Mustache +
JSON + cookie）が end-to-end で動作（`tmp/webframe/blog.raku`）。HTTP::Server::Tiny は完全非同期（`react`/`whenever
IO::Socket::Async`）で `whenever … done` 並行ギャップ待ち＝同期 INET パスが本命。surfaced bugs: readonly-param
フレーム間漏れ（#3539 修正）/ imported-sub shadows builtin（#3538 テスト）/ stored Regex `<$var>` lexical capture 喪失（未修正・別軸）。

**🟢 既存（off-the-shelf）フレームワーク Humming-Bird 4.0.0 が LOAD＋LISTEN＋accept＋decode まで動作（2026-06-24, #3549）**:
maintained な現行フレームワーク。**Humming-Bird::Core がロードでき、サーバが実 TCP を bind/accept し `Request.decode` がリクエストを
パースする**。付随して 6 件の一般修正を landed（#3549）: ① `CREATE` が宣言属性 slot を確保（`self.CREATE!SET-SELF`、MIME::Types）
② `%?RESOURCES` は実行中ルーチンのパッケージ優先（ロード中外側モジュールでなく）③ `.Buf`/`.Blob` coercion ④ `use strict` が
属性 twigil を未宣言扱いしない ⑤ `use strict` が `__`接頭の内部一時変数を未宣言扱いしない ⑥ regex の `\e`（ESC）。
**重要な訂正**: 「非同期サーバが律速で配信不可」は**誤り** — 素の `react{whenever IO::Socket::Async.listen{whenever
$conn.Supply(:bin){…}}}` は**実 curl に HTTP 配信できる**（`tmp/pcurl.raku` 実証）。完全配信まで残る 2 ブロッカー（別軸・深い）:
**B1** = 型付きパラメータ→呼び出し元同名 lexical への `var_type_constraint` 漏れ（グローバル name-keyed HashMap、env-first→fallback の
fallback が stale param 制約を返す。env-authoritative 化は subset-6e の EVAL 内 subset `where` 再代入を壊す＝fallback 必須。正攻法＝
HashMap を呼び出し境界で scope し `my`宣言/subset 制約は残す）。**B2** = detach した `start{react{whenever $chan{}}}` が await されない限り駆動されない
（HB の `!respond` ハンドラが発火しない）＝並行スケジューリング campaign。詳細＝memory `session-24-humming-bird-loads`。
（先行して #3542 6 件＋#3544 2 件の一般修正も landed＝Bailador/Glue 由来。）

**✅ 完動／native 化したモジュール（詳細は news/2026-06.md ＋ memory）**: Template::Mustache（#3395）/ JSON `to-json`・
`from-json` native（#3402）/ File::Temp 0.0.12（#3399）/ File::Directory::Tree 0.2 / HTTP::Parser 14/14（#3420/#3422/#3423）/
MIME::Base64 1.2.5（#3427）/ IO::Blob（builtin 型サブクラスの user override 修正・own test 一部残）。これらに付随した一般機能
（`:ver<>:auth<>` adverb・`IO(Cool)` coercion param・hash 要素 cell の pair-value デコンテナ化・grammar action・blob バイト反復）も landed。

**✅ HTTP::Server::Tiny 完動＋zef CLI 起動（2026-06-28・12 PR）**: HTTP::Server::Tiny が未改変 upstream で実 HTTP 配信
（#3840・`Thread.start` worker スタックサイズ修正）。NativeLibs/DBDish::SQLite::Native ロード（#3843 `&trait_mod:<>`/
`Rakudo::Internals.IS-WIN`/`is encoded`）。**zef 全モジュールがロード＋CLI 起動**（#3845 type-object 三項 / #3848 lvalue 三項
＋`:=` 式 bind / #3849 Bool default が Int 適合 / #3850 組込 `Distribution` role / #3851 package 内 `my sub` hoist /
#3854 `::("Rakudo::Internals::JSON")` 間接シンボル / #3856 コア型名を env alias で shadow させない / #3858 twigil 変数の
非 package 修飾 / #3860 exported MAIN dispatch）。すべて zef 専用でなく一般 Raku 互換改善。詳細＝memory `zef-load-status`。

#### モジュール動作状況調査（2026-06-21, mutsu でロード＋テスト試行）

候補モジュールを zef で取得し mutsu で `use`＋テスト試行した結果。**HTTP スタック・JSON・ユーティリティは
すべて NativeCall 非依存**（pure Raku）で、原理的に動作可能。各ブロッカーは独立した一般機能の欠落。
ハーネス＝`tmp/webstack/`（gitignored）。

- **✅ HTTP::Server::Tiny が end-to-end で動作（2026-06-28・全て pure Raku, NativeCall なし）**:
      **未改変の upstream モジュールが mutsu で `use`＋`.new`＋`.run` し、実 TCP で HTTP 応答を配信**（GET=`hello`、
      POST+body+`start{}` Promise 返却アプリ=content-length パス／TempFile・IO::Blob 経由で正しく往復）。
      真因＝`Thread.start`（`socket_thread.rs`）だけが素の `std::thread::spawn`（OS デフォルト ~2-8 MiB スタック）を使い、
      `start{}`/Promise/Supply worker が使う 256 MiB の `spawn_user_thread` から漏れていた。非同期サーバの react ループが
      BUILD で VM を再入する深いネストで 8 MiB を超過しオーバーフロー（gdb で 83 フレーム＝小スタックと確定）。修正で
      `Thread.start`／`$*SCHEDULER.cue`／hyper-race の全 user-code spawn を `spawn_user_thread` に統一。担保＝`t/thread-deep-stack.t`。
  - **残（深掘り時）**: keep-alive 連続リクエスト・chunked request body・`whenever $conn.Supply(...)` 内の `done`/`last`
    制御シグナル（real-TCP Supply の tap コールバックが worker thread 上で走り react の control-flow フレームから切れる）。
    upstream のデフォルト構成（max-keepalive-reqs=1・HTTP/1.0）では発火しないため、基本配信はブロックされない。
  - **✅ HTTP::Status v0.0.5 全テスト PASS（68 subtests・#3832）**: 当初診断「`method sink` が呼ばれず空」は誤り。
    実際の 2 ブロッカーは①配列ホール追跡がスコープを越えなかった（autoviv ギャップの `Package("Any")` を実 `Any` と
    区別する `__mutsu_initialized_index::name` env side table がフレーム scope で、外側配列をメソッド/クロージャから
    埋めると喪失→`:exists`/`:k`/`:p`/`.keys` がギャップを存在扱い）。`ArrayData::initialized` 埋め込みセット化で値と共に
    COW 伝播（`default`/`shape` と同パターン）。②`eq`/`is` がユーザ `Str` メソッドを使わず `.gist` 文字列化していた→
    infix `~` と同じ user-stringifier coercion を流用。`:exists` がホール追跡を一切参照しなかった既存バグも併せて修正。
- [ ] **NativeCall（C FFI）— MVP landed、DBDish への正攻法。**
  - **✅ MVP（#TBD）**: `is native(...)` の sub を `dlopen`+`libffi` で実 C 呼び出し。スカラ整数/浮動小数・
    `Str`→`char*`・`Pointer`・戻り値 `char*`→`Str`・`is symbol(...)`・非デフォルトライブラリ（`is native('m')`/
    `'sqlite3'`）に対応。soname フォールバック（`libfoo.so`→`.so.0/.1/.2`）で runtime-only システムでもロード。
    実証: `abs`/`strlen`/`pow`/`sqrt`/`sqlite3_libversion()`→"3.45.1" が動作。担保＝`t/nativecall-mvp.t`。
    実装＝`src/runtime/nativecall.rs`（`native` feature 下、wasm はスタブ）。`use NativeCall` は no-op 認識。
  - **✅ Pointer + out-param（#TBD）**: 組み込み `Pointer` 型（`.new`/`.Int`/`.Bool`/`.gist`・prelude 注入）＋
    `Pointer is rw` out-parameter（C が `void**` に書き戻す）。**ライブラリは process-lifetime でキャッシュ**
    （呼び出しごとの dlclose が libsqlite3 をアンロードしハンドルを無効化する問題を解消）。変数引数の
    varref Capture / Scalar / ContainerRef を marshalling 前に unwrap（リテラルしか無かった MVP で潜在した
    「変数を渡すと 0 になる」バグも修正）。**実証: `sqlite3_open`/`exec`(CREATE/INSERT)/`errmsg`/`close` の
    完全往復が動作**（`:memory:` DB に表作成・挿入・エラー取得）。担保＝`t/nativecall-pointer.t`（posix_memalign）。
  - **✅ Pointer return + 実 SELECT（#TBD）**: `returns Pointer` が実 `Pointer` オブジェクトを返す（`malloc`→Pointer→free）。
    **`sqlite3_prepare_v2`/`step`/`column_int`/`column_text`/`finalize` による prepared-statement SELECT で実際の
    行データ（int+text）を読み取れる**（新規 Rust 不要・既存 marshalling で動作）。担保＝`t/nativecall-sqlite.t`
    （libsqlite3 不在なら graceful skip）。**= mutsu から実 SQLite DB の完全な CRUD 往復が可能。**
  - **✅ モジュール配布 + 無名パラメータ（#TBD）**: NativeCall バインディングを **`use`-可能なモジュールとして配布可能**に
    （`Pointer` prelude を module 解析パス `parse_module_source` でも注入）。さらに無名（型のみ）パラメータにトレイト/where
    を許可（`sub f(Str, Pointer is rw)` がパース可能に・`src/parser/stmt/sub_param.rs`）。担保＝`t/nativecall-in-module.t`
    / `t/anon-param-trait.t`。**= 薄い DBDish::SQLite 互換層を pure-Raku モジュールとして書ける状態。**
  - **残（より広いモジュール互換に）**: ① `CArray[uint8]`・`CArray[Str]` / ② `is repr('CStruct')` 構造体 /
    ③ callback（汎用 C コールバック）。DBDish::SQLite 自体は上記 prepared-statement API で原理的に駆動可能。
  - **✅ 薄い DBDish::SQLite 互換層（pure-Raku モジュール）が動作**: `t/lib/DBDishLite.rakumod`（`use`-可能・
    `connect`→`Connection`・`.execute`→行ハッシュ配列・`.close`）が実 SQLite で CREATE/INSERT/ORDER BY/WHERE SELECT を
    往復（担保＝`t/dbdish-lite.t`）。= ウェブブログの DB 層が再利用可能モジュールとして揃った。これを暴いた precomp
    キャッシュ staleness バグ（注入後 AST をキャッシュするが version stamp が dev ビルド間で不変）は exe-mtime stamp で修正済。
  - **DBIish/DBDish（off-the-shelf）調査（2026-06-28・DBIish 0.6.8）**: zef で取得し mutsu でロード試行。スタック全体
    （`DBDish`/`Connection`/`StatementHandle`/`SQLite`/`SQLite::Native`/`ErrorHandling`）はパース可能。3 件の一般修正で
    **`NativeLibs` と `DBDish::SQLite::Native` がロード可能に**（#TBD）: ① `&trait_mod:<is>`（拡張識別子 `&`-参照に
    `trait_mod` カテゴリ追加）② `Rakudo::Internals.IS-WIN`/`.IS-MACOS`（host target から解決）③ `is encoded('utf8')`
    param trait（許可＋括弧引数消費・`skip_optional_trait_arg`）。担保＝`t/nativecall-module-compat.t`。
    **🧱 残る壁（アーキテクチャ非互換）**: `DBDish::SQLite` は `NativeHelpers::Blob` → **`MoarVM::Guts::REPRs`** に依存し、
    これが MoarVM の**内部メモリ表現を直接エミュレート**（`nativesizeof`〔未実装〕・`Pointer.WHERE` ポインタ演算で
    VMArray/CStruct のオフセットを実メモリ走査・`constant Offset = do{…}` を load 時に eager 実行）。mutsu は MoarVM では
    ないためこの層は原理的に動かせない（BLOB 列の `blob-from-pointer` 専用で、int/text CRUD には本来不要）。
    **= off-the-shelf DBDish::SQLite の完動は MoarVM REPR エミュレーション待ち＝事実上の壁**。実用 SQLite は手書き
    `DBDishLite` + NativeCall MVP が正道。副次的に発見した一般パースバグ（別軸・未修正）: `constant NAME is export =
    <cond> ?? <Type> !! <Type>` が then 枝 bareword/型で失敗（非 export は `constant` を関数呼び出しに誤フォールバックして
    黙って誤パース）＝文レベル `expression` の ternary then 枝 greediness。
- **JSON は native 実装済み**（`to-json`/`from-json`・#3402・news 参照）。Template::Mustache 91/92-specs の残（別軸・
  本タスク外）= 実 spec の rendering ギャップ（delimiter 永続化／inheritable partials／lambda）＋ 最初の spec のみ
  `+$spec.value`=0 になる subtest/Seq-consumption 系バグ（itemization とは独立）。
- [ ] バイナリ配布: mise GitHub バックエンドのインストール検証 / GitHub Releases 自動化。

### I. Track C — 並行（共有セル）残

スカラ／state の `start` 間ライブ共有・hash/array 要素 atomic は landed（→ news）。残:

- [ ] **`state @`/`%`・lexical aggregate の真共有**（Track B 要素 cell 基盤に依存。Track B は GC と統合＝
      ADR-0001 層3a なので、この項目も実質 Phase B 待ち）。
- [ ] Semaphore / nonblocking await / lock 競合（S17・hard・別軸）。
- [ ] `unsafe` の single-thread 前提コメント是正（`Arc::as_ptr as *mut` を strong_count ガード前提に・最終的に要素も cell 化）。

### J. 構造リファクタ・将来（独立・中長期）

- [ ] **★大型ジャンプ: GC（cycle collector on Arc）→ JIT**（ADR-0001・Phase B/C）。Phase A 完了後に着手。
      **Track B（要素 cell 化）と GC は統合キャンペーン（層3a・`Arc → Gc<T>` 一斉置換）**。続いて NaN-boxing
      （層3b・JIT 地ならし）→ JIT（層4）。未決は収集方式（同期/非同期）と A' 地ならしの範囲（ADR §4.2/§4.3）。
- [ ] 制御フロー（`return`/`last`/`next`/`take`/`emit`）を `RuntimeError` god-struct から `enum Control` へ分離（ANALYSIS §2.4）。
- [ ] `.^methods`/`.can` を実ディスパッチ表から導出 / roast fudge ロジック分離 / 500 行超ファイル分割。
- [ ] エラーメッセージ品質向上 / エッジケースの panic・crash を 0 に。
- [ ] REPL / Debugger / `zef` 互換 / native binary output / WASM playground 公開。

---

## メトリクス

| 指標 | 現在 | 目標 |
|------|------|------|
| Whitelist | **1285** | 1300+ |
| fib(25) vs raku | **1.0x** | <10x |
| method-call vs raku | **2.7x** | <1.5x |
| bench-class vs raku | **2.3x** | <1.5x |
| bench-fib（型制約付き）vs raku | **3.2x** | <2x |
| 起動時間 vs raku | **0.04x** | 0.04x |
| tree-walk フォールバック（メソッド/関数） | **~1% / ~18.6%（大半 carrier）** | 0%（carrier 除く） |
| 動作モジュール数 | **7+（Mustache, File::Temp, File::Directory::Tree, HTTP::Parser, MIME::Base64, HTTP::Server::Tiny, zef CLI）** | 10+（ウェブブログスタック＋mzef） |
| Template::Mustache / HTTP::Server::Tiny | **Mustache ✅** / **Tiny ✅（end-to-end 配信・2026-06-28）** | ✅ |
| zef CLI（`mzef` north-star） | **CLI 起動＋dispatch ✅（`zef --help`/`--version`・2026-06-28）** | 実 install まで（CompUnit ネイティブ層） |

---

## ✅ 完了した大型キャンペーン（詳細は news/、ここには残さない）

- **VM decoupling / tree-walking struct 統合（CP-1/2/3, #3075〜#3104）** — 単一 struct が bytecode VM。
- **tree-walking interpreter 撤去（§B・#3664〜#3680）** — `run_instance_method_resolved` 非-delegation arm（~470行 `run_block`）削除。VM がユーザメソッド body の唯一の実行エンジン。後続: sound multi resolution cache（#3684）。
- **状態所有 ③ ctor フォーク（§D(a)・#3514〜#3536, 10 スライス）** — pure-value/VM-owned-state な built-in ctor 全て native 化（capstone IO::Socket::INET）。
- **multi-dispatch VM 化（§D・#3541〜）** — proto/where/default/非trivial-proto/{*}-rw/nextsame-rw/&-param/module-sub/test-assertion 候補を OTF compiled 化。
- **単一ストア化（#3219〜#3455, 第12〜52セッション）** — write-through グラインド → reverse pull 撤去（#3354）→
  boxing 恒久 ON（#3450）→ `env_dirty` 物理削除（#3455）まで完了。詳細＝news/2026-06.md ＋ memory
  `project_env_dirty_physical_removal`。
- **第一級コンテナ Phase 0/1・Phase 3 Stage 0〜2c** — escape 解析・スカラ cell・インスタンス属性 cell（CAS 含む）。
- **React/Supply 統一ループ（Track C Stage 1〜3）** — whenever/LAST/QUIT/CLOSE 全 native。
- **lazy 配列 L1/L1b/L5/L5b/L2a（#3306〜#3315）** — lazy `.gist`/`.elems`/reify-on-demand（整数レンジ）。
- **panic→`X::` 境界 / 無限 Range クラッシュ撲滅 / roast 90% 突破** — 完了。
- **重複実装カタログ消化（dedup A/B/C・レバー A/B/C）** — 完了。
