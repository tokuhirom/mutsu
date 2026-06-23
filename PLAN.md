# PLAN.md — mutsu 今後の実装計画

> このファイルは**これからやる作業だけ**を載せる。完了したものは [news/](news/)（月別）へ移す。
> 過去の実装状況は [news/](news/)、パフォーマンス詳細は [PERFORMANCE.md](PERFORMANCE.md)、
> roast ブロッカー分析は [TODO_roast/BLOCKERS.md](TODO_roast/BLOCKERS.md) を参照。
>
> **最終更新 2026-06-21**: 全面再編。2大フラッグシップ（単一ストア化・tree-walking 撤去）が同一の
> 構造的前提に収束したことを反映（§1）。完了済みの大型キャンペーン詳細は news/2026-06.md へ移動。

## 方針

**実用的な Raku インタープリタ**として使ってもらえる品質を目指す。
起動 25 倍速い強みを活かし、CLI ツール・スクリプティング用途をメインターゲットとし、
最終的には **mutsu でウェブブログが作れる** レベルのライブラリ互換性を実現する。

### 🚫 標準ルール: 「1 操作 = 1 実装」を崩さない（ユーザー方針 2026-06-07）

実行エンジンは単一 struct `Interpreter`（＝ bytecode VM）に一本化済み。同じ Raku 操作を**二度書かない**:

1. 新規実装・修正は VM/native 層（`src/vm/` ＋ pure native `src/builtins/`）に **1 回だけ**書く。
2. carrier（EVAL / 正規表現の埋め込み `{}` ブロック等）が同じ処理を要するときは単一 native 実装へ**委譲**する。
3. 重複を見つけたら native を authoritative にして重複コピーを削除する。

---

## 1. 🎯 現在の戦略地図 — 2大フラッグシップは「第一級コンテナ＋状態所有」に収束する

2026-06-21 の棚卸しで判明した最重要の構造:

> **単一ストア化（A）は完了（2026-06-23・#3455）。残る大型キャンペーン B（tree-walking 撤去）と、
> A の真の総仕上げ（Phase-2 コンテナ要素 cell 化）は、同じ2つの substrate 前提を共有している。**
>
> - **前提① 第一級コンテナ Phase 2 完了＋env↔locals がコンテナ cell を共有**（`docs/container-identity.md` /
>   `docs/env-locals-coherence.md`）← env↔locals 共有・env_dirty 物理削除は達成。残＝Phase-2 要素 cell 化（§C）。
> - **前提② 状態所有（state ownership）＝レジストリ／IO ハンドル／型メタを Interpreter から VM が真に所有する**
>   （`docs/vm-interpreter-fallback-ledger.md` の ②③）

∴ **優先すべきは前提①②の substrate（§C・§D）であり、それが B を前進させる。§C の第一級コンテナ Phase 2 は
完了（grep-rw-view 撤去 #3466 + 最終 SlotRef キル #3472・`HashEntryRef` 統合 + scalar-param 共有 follow-up）。
残る substrate は §D 状態所有（前提②）と Phase 3 instance 属性セル。**

### A. 単一ストア化（locals↔env 二重ストア統合）— ✅ **完了（2026-06-23）**

設計＝[docs/env-locals-coherence.md](docs/env-locals-coherence.md) / [docs/vm-single-store.md](docs/vm-single-store.md)。
グラインド詳細は [news/2026-06.md](news/2026-06.md)（reverse pull 撤去 #3354 / double-OFF surface 16→0・25→0 / boxing
恒久 ON #3450 / env_dirty 物理削除 #3455）と MEMORY 第45〜52セッションを参照。

- ✅ reverse pull（`sync_locals_from_env`）撤去（#3354）→ cell-boxing 恒久 ON 化（#3450）→ `env_dirty` /
  `saved_env_dirty` / `reconcile_locals_from_env_at_site` / `blanket_reconcile_if_dirty` / `ensure_locals_synced` /
  `cell_boxing_active()` の物理削除（#3455・604 行減）まで完了。
- **コヒーレンス機構は cell-boxing ＋ precise writeback の2つのみ**。`locals` が単一権威・`env` は派生ビュー。
  §1-A / §2-E 完了。残課題は §C（Phase-2 コンテナ要素 cell 化）に統合。

### B. tree-walking interpreter 撤去 — **struct 統合は完了・残フォークは状態所有待ち**

台帳＝[docs/vm-interpreter-fallback-ledger.md](docs/vm-interpreter-fallback-ledger.md) / [docs/vm-decoupling.md](docs/vm-decoupling.md)。

- ✅ **VM/Interpreter の struct 統合は完了**（CP-1/2/3・#3075〜#3104）。単一 struct が bytecode VM。
- **現状のフォールバック残量**: 記録される tree-walk フォールバックは **16 サイト**（メソッド 10／関数 6）。
  実測フォールバック率はメソッド ~1%／関数 ~18.6%（うち大半は EVAL **carrier**＝tree-walk ではない意図的委譲）。
  - **4 サイトは意図的 carrier**（MOP リフレクション `.WHAT`/`.HOW`/`^methods` 等）＝**撤去対象外**。
  - **残りは構造的ブロック**: 「easy/medium な個別フォールバック撲滅は枯渇済み」（台帳の結論）。残るのは
    ③状態所有（Buf/Failure/IO native メソッドが `io_handles`／型メタ／レジストリに依存）と Phase 2 コンテナ
    （hyper・array-backed instance メソッド）と multi-dispatch（proto/where 評価）。
- `src/runtime/`（~100 ファイル・~11万行）の内訳: **~60% は共有インフラ**（regex エンジン・クラス登録・IO・
  builtins＝VM も依存＝残す）、**~40% が純 tree-walk dispatch**（③ 完了後に削除可）。
- **∴ tree-walking の大量削除も前提②（状態所有）が前提。** 個別フォールバックの easy win は終わっている。

> **結論**: A も B も「地道なグラインドの続き」ではなく、**前提①②の substrate 着手**が律速。次章 §C がその substrate。

---

## 2. 🔴 順序依存・並列不可 — substrate（前提①②）

> 内部に着手順序があり、前段が終わるまで後段に着手できない。A・B 両フラッグシップの律速。

### C. 第一級コンテナ Phase 2 — ✅ **完了（2026-06-23）** → 残テーマは Phase 3（instance 属性セル）

実装台帳＝[docs/container-identity.md](docs/container-identity.md)。Phase 0/1 完了、**Phase 2（配列/ハッシュ要素の
第一級コンテナ化）完了**（Stage 0/1/2 + slotref-removal slice 1-5 #3472 + grep-rw-view 撤去 #3466）。Phase 3
（インスタンス属性 cell）も Stage 0〜2c 完了（Stage 3 = escape-aware cell 省略は perf 未正当化で deferred）。
`=`-share / `:=`-bind / 要素-要素 bind / 深い欠落 path bind / scalar-param 共有 / typed-array 共有 / for-rw /
HoH 深い共有が全て raku 一致（pin=`t/container-identity-phase2-complete.t` 18 / nested.t 43 / 各 share 系 suite）。

- [x] **Phase 2 Stage 2 slice 5（最終 SlotRef キル・完了・2026-06-23・#3472）**: `HashSlotRef` + `DeferredHashAccess`
      の 2 variant を単一 path ベース `Value::HashEntryRef { hash, path }` に統合し、`SlotRef` の名前と概念を払拭。
      欠落 key への `:=` bind の deferred-vivification token は **anti-goal の side-table 無しには原理的に削除不可**
      （`:exists`/iteration を汚さず Any-read / write-materialize する token は Value variant を要求）と確定し、
      「全 variant 削除」でなく「`SlotRef` 概念の払拭＋単一 honest token への統合」を達成（`git grep 'SlotRef\|DeferredHashAccess' src/`=0）。
      `parent_slot` チェーン廃止で **3 段以上の全欠落 path bind（`$b := %h<a><b><c>`）が動く**ようになった（raku 一致）。
      詳細＝[docs/slotref-removal-plan.md](docs/slotref-removal-plan.md) §0 達成注記 / slice 5。pin=`t/hash-entry-ref-deep-bind.t`。
- ✅ **grep-rw-view 撤去（#3466・2026-06-23）→ [news/2026-06.md](news/2026-06.md)**。`.grep` を第一級要素 cell 化し
      `for_grep_view`/`GrepView`/`grep_source`/index-based writeback を全廃（-90 行）。
- ✅ **env↔locals cell 共有 — captured-outer cell 化／純 writeback コヒーレンス（A の律速・完了）**: slice 1〜1.20
      ＋ S1〜S21。台帳＝[docs/captured-outer-cell-sharing.md](docs/captured-outer-cell-sharing.md)、詳細＝news/2026-06.md
      ＋ MEMORY 第45〜52。**残る OFF 依存は IO-Socket-Async.t の flaky のみ**。
- [x] **follow-up（完了・2026-06-23）**: `$x = @arr` 共有の method/sub param 版（`method m($n){ $n.push }` /
      `sub f($n){ $n.push }`）・`is copy` param。probe で raku 完全一致を確認（`sub f($n is copy){...}` は scalar
      container のコピーだが中の Array reference は共有＝raku 同様 outer に伝播、`@a is copy` は要素コピーで隔離）。
      担保＝`t/scalar-param-container-share.t`(21) / `t/scalar-param-container-share-method.t`(18) /
      `t/named-param-container-share.t`(16) / `t/container-identity-phase2-complete.t`(18)。

### D. 状態所有（state ownership）— VM がレジストリ／IO／型メタを真に所有（前提②）

台帳＝[docs/vm-interpreter-fallback-ledger.md](docs/vm-interpreter-fallback-ledger.md) ②③。**B の律速。**

- [x] **レジストリ所有（②）= 完了**（#2760-2772、`docs/vm-registry-ownership.md`）。
- [x] **IO native メソッド族（③ の一部）= 完了**。`io_handles` は `Arc<RwLock>` 共有フィールドで VM が所有しているので、
      **IO::Path FS メソッド族（stat / content-read / fs-mutate / open〔io_handles 確保 capstone〕/ two-path / comb）が
      100% VM ネイティブ化済**（2026-06-23、#3499/#3501/#3503/#3504/#3507/#3511）。`.encode`/`.decode`（#3509）/ coercion 全族
      （scalar 受け手まで・#3497 他）も native。**clean な pure-value native-method ドレインは枯渇**＝残る catch-all バウンスは
      全て別軸 or 構造的ブロッカー前提（下記）。
- [ ] **(a) 組込型 ctor の native 化 — 進行中（2026-06-23・3 スライス landed）**: ③ ctor フォークに着手。
  - `::`-namespaced クラス＋組込例外型（`X::AdHoc`/`X::TypeCheck::Binding` …）の `.new`（#3514）= `is_native_default_constructible`
    の `::` ガード撤去＋`has_attribute || is_exception` 緩和＋VM call site で `materialize_exception_message_in_result`。
  - `Lock`/`Lock::Async`/`Lock::Soft`（#3515）/ `Promise`/`Channel`/`Supplier`/`Supplier::Preserving`（#3517）= static
    `try_native_builtin_construct` に arm 追加（pure data / global counter）。
  - **残 ③ ctor 候補（次スライス）**: **QuantHash 族（Bag/Set/Mix/SetHash/BagHash/MixHash）** が最有力＝`.new` は element 計数＋
    parameterized 型 check で `&mut self`（`type_matches_value`/`tag_container_metadata`）＝static 不可。dispatch_new の 3 arm（~450 行）を
    `&mut self` helper `try_native_quanthash_construct` に抽出し VM call site から呼ぶ（IO::Path family と同パターン・新モジュールで 500 行制限維持）。
    env-pure（値構築＋container metadata tag のみ・caller env 非変異）＝`method_dispatch_pure=true`。次点＝`Array`/`Hash`（shaped/container
    metadata・更に複雑）/ `Capture.new`（**mutsu 未実装＝エラー・別途実装要**）/ `Proxy`（FETCH/STORE closure）。
  - **(b) tree-walk dispatch chain 削除の substrate**: IO/coercion が native 化した今、`dispatch_method_by_name_*` チェーンと
    catch-all バウンス（`vm_call_method_compiled.rs` 末尾）の構造的削除。残る到達カテゴリ＝MOP carrier（WHAT/name/can/HOW・反射で
    撲滅対象外）/ landmine（Instance.Str/.Stringy/.raku/.gist・列挙不能で見送り済）/ block-exec slow path（map/grep・lever B/Phase 2）/
    concurrency（Supply/tap・別軸）/ typed-array mutator（typed/shaped/shared・Phase 2/lever B）。**純粋に削れる残りは少なく、各々別軸の
    前提が要る**＝substrate 着手は要設計。
- [ ] **multi-dispatch の VM 化**: proto multi / where 制約評価を VM 側で（`vm_call_func_ops.rs:1051/1084`）。

---

## 3. 🟢 並列実装可能（独立・互いにブロックしない）

> substrate（§2）と critical path を共有しない。別ブランチで並行に進められる。着手時に該当 BLOCKERS/メモリを確認。

### F. roast backlog（[TODO_roast/BLOCKERS.md](TODO_roast/BLOCKERS.md) 駆動・インパクト順）

現状 whitelist **1285**。診断は `./scripts/roast-history.sh`（`tmp/roast-{panic,timeout,error,fail,pass}.txt`）。

- [ ] **★型付き例外（最高インパクトの単一ファイル）**: `S32-exceptions/misc.t`（42/157）。X::NotParametric /
      X::Undeclared / X::Redeclaration / X::Bind / X::TypeCheck 他 ~25 種の one-off 型実装。BLOCKERS.md §B。
- [ ] **lazy 無限配列 L2b–L4**: L1/L1b/L5/L5b/L2a は landed（→ news/2026-06）。残＝L2b（真のメモリ遅延化・seed `[1]`・
      `docs/lazy-arrays.md`「L2b」節に実行プラン確定済）→ `(1...*)`/closure 配列変換 → L4 slurpy 真 lazy 化。
      whitelist payoff（slurpy-params.t/slice.t）は Seq single-pass consumption（`X::Seq::Consumed`）が別軸。
- [ ] **Match キャプチャ番号付け / コンテナ kind**: (1) `$<x>=(...)` が positional スロットにも重複格納され番号がずれる、
      (2) `m:g//` を `my @m` 代入後 `@m.gist` が `(…)` を返す（receiver の List-kind dual-store）。S05-capture/array-alias.t（30/37）。
- [ ] 未実装演算子: `ff`/`fff`（flipflop 8 variants）/ `==>`・`<==`（feed precedence: `==>` が `=` より強く結合する差）/
      `~<`・`~>`（string bitwise shift・優先度低）。
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
- [ ] **Lever 2: NaN-boxing（高 payoff・設計済）**: `Value` 48→8 bytes（Int/Num/Bool/Nil を NaN payload に）。
      int-arith 2x・fib ~30% 狙い。`value_size_guard` テストでサイズ監視中。
- [ ] **Lever 3: threaded dispatch（中 payoff・ラフ）**: opcode の `match` を関数ポインタテーブルに。命令律速ベンチ 10–30%。
- [ ] **Lever 4: JIT（Cranelift）/ Lever 5: 型制約チェックの tight-loop 省略**（ラフ・大）。
- [ ] 正規表現: 量指定子反復ごとの `RegexCaptures.clone()` 削減。
- 目標: method-call <1.5x、bench-class <1.5x、bench-fib（型制約付き）<2x。

### H. モジュール互換（Q3 — ウェブブログスタック）

目標: **mutsu でウェブブログシステムが構築できる**。**Template::Mustache 完動（#3395）**。
HTTP スタック/JSON/DB/ユーティリティは下記調査の通り NativeCall 非依存で動作可能、各々独立した一般機能の欠落待ち。

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
- [ ] **DB アクセス — sqlite3 CLI ラッパ（pure Raku）が現実解。**
  - **DBIish/DBDish**: NativeCall 依存 → **ブロック**（ドライバは `sqlite3_*` C API）。API shape のみ再利用可。
  - **推奨**: `run`/`qqx` で `sqlite3`（`/usr/bin/sqlite3` 3.45.1, インストール済）を呼ぶ薄い pure-Raku ラッパ。
    mutsu の `run`/`qqx`（`:out`/`:err`/`exitcode`）は raku とバイト一致で動作確認済。`sqlite3 -json` で行を JSON 出力可。
    工数 ~1-2日。値エスケープ/1クエリ1プロセスは要注意。
  - **フォールバック**: flat-file `.raku`＋`EVALFILE`（mutsu で round-trip 確認済）は今日すぐ動く MVP。
- **JSON は native 実装済み**（`to-json`/`from-json`・#3402・news 参照）。Template::Mustache 91/92-specs の残（別軸・
  本タスク外）= 実 spec の rendering ギャップ（delimiter 永続化／inheritable partials／lambda）＋ 最初の spec のみ
  `+$spec.value`=0 になる subtest/Seq-consumption 系バグ（itemization とは独立）。
- [ ] バイナリ配布: mise GitHub バックエンドのインストール検証 / GitHub Releases 自動化。

### I. Track C — 並行（共有セル）残

スカラ／state の `start` 間ライブ共有・hash/array 要素 atomic は landed（→ news）。残:

- [ ] **`state @`/`%`・lexical aggregate の真共有**（Track B 要素 cell 基盤に依存）。
- [ ] Semaphore / nonblocking await / lock 競合（S17・hard・別軸）。
- [ ] `unsafe` の single-thread 前提コメント是正（`Arc::as_ptr as *mut` を strong_count ガード前提に・最終的に要素も cell 化）。

### J. 構造リファクタ・将来（独立・中長期）

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
- **単一ストア化（#3219〜#3455, 第12〜52セッション）** — write-through グラインド → reverse pull 撤去（#3354）→
  boxing 恒久 ON（#3450）→ `env_dirty` 物理削除（#3455）まで完了。詳細＝news/2026-06.md ＋ memory
  `project_env_dirty_physical_removal`。
- **第一級コンテナ Phase 0/1・Phase 3 Stage 0〜2c** — escape 解析・スカラ cell・インスタンス属性 cell（CAS 含む）。
- **React/Supply 統一ループ（Track C Stage 1〜3）** — whenever/LAST/QUIT/CLOSE 全 native。
- **lazy 配列 L1/L1b/L5/L5b/L2a（#3306〜#3315）** — lazy `.gist`/`.elems`/reify-on-demand（整数レンジ）。
- **panic→`X::` 境界 / 無限 Range クラッシュ撲滅 / roast 90% 突破** — 完了。
- **重複実装カタログ消化（dedup A/B/C・レバー A/B/C）** — 完了。
