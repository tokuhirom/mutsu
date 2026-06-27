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
| **A. 追いつく** | 互換性＋速度で raku に並ぶ（**いまここ**） | §F roast / §2 multi-dispatch / §H module / §G Lever 1・3 |
| **A'. 地ならし** | root 集約で GC 実装を楽にする | レキシカルスコープ（ANALYSIS §1.4）/ upvalue index 化（ANALYSIS §1.3） |
| **B. Value 表現リワーク＋GC** | Track B（要素 cell 化）＋ cycle collector を**統合**（ADR 層3a） | §G Lever 2 周辺 / §I Track B 依存項目 / §J |
| **C. JIT** | 独自メリット | §G Lever 4 |

- **GC は JIT の前**（JIT を GC 前提の上に載せる）。**GC は Phase A 完了後**に着手（当面は §J の将来扱い）。
- **方式 = cycle collector on Arc（non-moving + refcount・レベル1 採用）**。スカラ系は型フィルタで GC 対象外
  ＝数値/文字列 hot path はコスト 0。性能は GC でなく JIT で稼ぐ。
- **Track B は GC と一体（層3a）。単独で先行着手しない。** NaN-boxing は JIT の地ならし（層3b）、
  biased refcount は層3c。

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
- [ ] **★lazy-seq（次セッションの開始点・2026-06-27 再 probe）**: `1..*` 配列は reify-on-index 済（`@a[200000]`=200001・100k cap 解消）、
      `(1...Inf)`=Seq is-lazy=True。残 live ギャップ＝① `.List`/`.Array` coercion が laziness を落とす（`coercion.rs`）②`eqv` が両-lazy-同型で
      `X::Cannot::Lazy` を投げない（`vm_comparison_ops.rs`）＝**eqv.t（1 subtest・最も近い +1）** ③Seq single-pass consumption（`X::Seq::Consumed`・broad）
      ＝slurpy-params.t 70-77 ④reify-on-mutation／slurpy 真 lazy（`flatten_into_slurpy` 無限展開→hang）／lazy `.gist`（`docs/lazy-arrays.md` L3/L4・doc TL;DR は stale）。
      詳細＝memory `next-session-lazy-seq`・BLOCKERS.md §「Real lazy infinite sequences」。
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

現状（要再計測・PERFORMANCE.md）: **9/12 ベンチで raku 超え**。起動 0.04x（28倍速）。
**残ボトルネック**: method-call 2.7x / bench-class 2.3x / bench-fib（型制約付き）3.2x。
真因＝メソッド呼び出しの **env deep clone ~9μs/call（全コストの29%）**＝~100 entry の `Arc::make_mut`。

- [ ] **Lever 1: closure captures を indexed slot 化（高 payoff・設計済）**: closure 生成時の env deep clone を撤廃。
      コンパイル時に closure が read/write する変数を解析し `Vec<Value>` に格納。method-call <2x 狙い。
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

#### モジュール動作状況調査（2026-06-21, mutsu でロード＋テスト試行）

候補モジュールを zef で取得し mutsu で `use`＋テスト試行した結果。**HTTP スタック・JSON・ユーティリティは
すべて NativeCall 非依存**（pure Raku）で、原理的に動作可能。各ブロッカーは独立した一般機能の欠落。
ハーネス＝`tmp/webstack/`（gitignored）。

- [ ] **HTTP::Server::Tiny スタック（全て pure Raku, NativeCall なし）— 想像以上に近い。**
      本体は `use`＋`.new`＋非同期サーバが TCP listen/accept まで実際に動く。`:bin` Supply→`Buf[uint8]`（#TBD）と
      HTTP::Parser 14/14（#3420/#3422/#3423）は landed（news 参照）。リクエスト/レスポンス往復を阻む**残ブロッカー**:
  - **`whenever $conn.Supply(...)` の内側で `done`/`last`** を呼ぶと制御シグナルが react ハンドラに捕捉されず
    「Unhandled exception in code scheduled on thread」（空メッセージ）でプロセス終了（bin/非 bin 共通・`.tap` 回避なら OK）。
    real-TCP Supply の tap コールバックが worker thread 上で走り react の control-flow フレームから切れているため。
    HTTP::Server::Tiny のリクエストループが `done` を使うなら要修正。
  - **HTTP::Status v0.0.5**: user `method sink` がシンクコンテキストで呼ばれず status table が空。
    ⚠️ 注意: 過去に sink 修正は sink.t を回帰させた（メモリ `sink-context-blocked-container-identity` 参照）。
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
  - **DBIish/DBDish（off-the-shelf）**: 実配布版はまだ 3 機能待ち（regex "Unmatched (" parse / `Rakudo::Internals.REGISTER-DYNAMIC` /
    `is encoded(...)` NativeCall param trait）。手書き互換層は上記の通り動く。
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
| 動作モジュール数 | **5（Mustache, File::Temp, File::Directory::Tree, HTTP::Parser, MIME::Base64〔own test PASS〕）** | 5+（ウェブブログスタック） |
| Template::Mustache / HTTP::Server::Tiny | **Mustache ✅** / Tiny ❌ | ✅ |

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
