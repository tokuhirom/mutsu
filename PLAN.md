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
- [x] **(a) 組込型 ctor の native 化 — ③ ctor フォーク完了（2026-06-24・10 スライス landed）**: pure-value / VM-owned-state な
      built-in ctor は全て native 化済（IO::Socket::INET capstone #3536）。残 `new` fallback は CallFrame〔call stack〕・error-only のみ。
  - `::`-namespaced クラス＋組込例外型（`X::AdHoc`/`X::TypeCheck::Binding` …）の `.new`（#3514）= `is_native_default_constructible`
    の `::` ガード撤去＋`has_attribute || is_exception` 緩和＋VM call site で `materialize_exception_message_in_result`。
  - `Lock`/`Lock::Async`/`Lock::Soft`（#3515）/ `Promise`/`Channel`/`Supplier`/`Supplier::Preserving`（#3517）= static
    `try_native_builtin_construct` に arm 追加（pure data / global counter）。
  - **QuantHash 族（Set/SetHash/Bag/BagHash/Mix/MixHash）の `.new`（#3520）= 完了**。`&mut self`（element 計数＋parameterized 型 check
    ＋container metadata）なので dispatch_new の 3 arm（414 行）を新モジュール `methods_quanthash_ctor.rs` の helper
    `try_native_quanthash_construct` に**丸ごと抽出（真の単一 impl）**、VM 非mut/mut 両 path から `..._for_package` wrapper で呼ぶ。
  - **`FakeScheduler`/`Proxy`/`Match` の `.new`（#3523）= 完了**。clean static 残（pure data / process-global counter）。per-type helper
    抽出＋`dispatch_new` arm delegation で true single impl。
  - **`Capture.new`（#3524）= 実装完了**。default ctor は empty Capture（named arg drop・positional reject）＝`Mu.new` named-only と一致。
    populated は `\(...)`。残ギャップ＝Capture **instance** 受け手の `.new`（別軸の built-in value variant instance ctor 委譲）。
  - **`Array`/`List`/`Positional`/`array`/`Hash`/`Map` の `.new`（#3526）= 完了**。`&mut self`（shaped/typed/metadata）なので QuantHash 同様
    新モジュール `methods_aggregate_ctor.rs` に 2 arm（~310 行）丸ごと抽出＝true single impl。
  - **allomorph〔`IntStr`/`NumStr`/`RatStr`/`ComplexStr`〕＋`ObjAt`/`ValueObjAt` の `.new`（#3528）= 完了**。`dispatch_new` ではなく
    `dispatch_new_and_constructors`（slow-path）側に在った pure-static ctor。`new` fallback receiver の再計測で最頻（RatStr 892 等）と判明。
    static `build_native_allomorph_value`/`build_native_objat_value` を `try_native_builtin_construct` と interpreter 両方が呼ぶ＝true single impl。
  - **`IO::Path` family〔`IO::Path`/`::Unix`/`::Win32`/`::Cygwin`/`::QNX`〕の `.new`（#3529）= 完了**＝IO::Path native 化の ctor capstone。
    pure path-string assembly（registry 読みのみ・FS/cwd/env 不要）。`dispatch_new` の IO::Path arm（~117 行）を `build_io_path_instance` に丸ごと抽出、
    VM ゲート `try_native_io_path_construct` は `is_io_path_lexical_class`（built-in family のみ）に絞って非mut/mut 両 call site から呼ぶ＝true single impl。
  - **`Failure.new($exception?)`（#TBD）= 完了**＝残 `new` fallback の最大カウント（2593）。VM 所有 state のみ読む pure data assembly（明示 exception /
    `$!` env / `X::AdHoc` default＋非例外値の `X::AdHoc` wrap・MRO 読み `mro_readonly`）。`dispatch_new_and_constructors` の arm を `build_native_failure_value`
    に丸ごと抽出し VM/interpreter 両方が呼ぶ＝true single impl。単一ストア化後 `self.env`（＝`$!`）は VM/interpreter 同一＝byte-identical。
  - **`Seq.new($iterator?)`（#3533）= 完了**。前回「iterator carrier＝別軸（impure）」と保守分類していたが carrier state 自体が VM 所有
    （`predictive_seq_iters` フィールド＋env 内部キー＋Seq Arc キーのグローバル deferred-iter 表）＝構築は eager pull せず VM 所有 state への登録のみ。
    `dispatch_new` の Seq arm を `try_native_seq_construct` に丸ごと抽出し VM/interpreter 両方が呼ぶ＝true single impl・byte-identical。
  - **`Proc::Async.new(@cmd, :w, :enc)`（#3535）= 完了**。台帳が「state 依存」と分類していたが ctor は完全 pure data（実プロセス spawn は `.start`・
    ctor は引数パース＋3 本の process-global supply id〔`next_supply_id` free fn〕＋空の stdout/stderr/merged `Supply` 構築のみ・`&self` 依存ゼロ）。
    `dispatch_new` の arm を static `build_native_proc_async_value` に丸ごと抽出し、VM は `try_native_builtin_construct` に arm を `Promise`/`Channel` と
    同型で wire＝true single impl・byte-identical。
  - **`IO::Socket::INET.new(...)`（#3536）= 完了＝③ ctor フォーク capstone**。実 bind/connect を行うが書き込み先は VM 所有 `io_handles`
    （`insert_handle_state`＝native 化済 `IO::Path.open`〔#3507〕と同型）。既存 `&mut self` helper `dispatch_socket_inet_new` を `pub(crate)` に広げ、
    VM の非mut/mut 両 catch dispatch から同じ helper を直接呼ぶ＝true single impl・byte-identical（新規コピー無し）。
  - **∴ pure-value / VM-owned-state な built-in ctor は全て native 化済＝③ ctor フォーク完了**。残 `new` fallback receiver は `CallFrame.new`
    〔call-stack carrier〕・error-only（HyperWhatever/Whatever/Instant）のみ＝別軸 or 構造ブロック。次は §D の本丸＝(b) tree-walk dispatch-chain
    削除 substrate or multi-dispatch VM 化（下記の唯一の `[ ]`）。
  - **(b) tree-walk dispatch chain 削除の substrate**: IO/coercion が native 化した今、`dispatch_method_by_name_*` チェーンと
    catch-all バウンス（`vm_call_method_compiled.rs` 末尾）の構造的削除。残る到達カテゴリ＝MOP carrier（WHAT/name/can/HOW・反射で
    撲滅対象外）/ landmine（Instance.Str/.Stringy/.raku/.gist・列挙不能で見送り済）/ block-exec slow path（map/grep・lever B/Phase 2）/
    concurrency（Supply/tap・別軸）/ typed-array mutator（typed/shaped/shared・Phase 2/lever B）。**純粋に削れる残りは少なく、各々別軸の
    前提が要る**＝substrate 着手は要設計。
- [ ] **multi-dispatch の VM 化**（着手済・proto sub の trivial-body 経路は #3541 で landed）:
  - [x] **proto sub dispatch（trivial body）= 完了（#3541）**。`proto foo {*}` / bodyless proto を VM call site で直接ディスパッチ
    （`vm_resolve_trivial_proto_candidate` が VM 所有レジストリで winner 候補を解決→`compile_and_call_function_def` で compiled 実行）。
    tree-walk な proto body＋`__PROTO_DISPATCH__` round-trip＋候補 body `run_block` を全てバイパス。proto sig は gate として検証
    （`method_args_match`）。非trivial body / 非OTF候補 は interpreter fallback 維持。実測 `proto factorial` で fallback 100%→0%。
  - [x] **where 制約付き候補の OTF 化 = 完了（#3543）**。`def_is_otf_compilable` の `where_constraint.is_none()` を撤去。
    安全性の根拠＝winner は `resolve_function_with_types`/`resolve_proto_candidate_with_types` が `args_match_param_types` 経由で
    where を評価して選ぶので解決済み def は既に where を満たす。compiled binding（`call_compiled_function_named`→`bind_function_args_values`）が
    where を再検証（単一候補の失敗は interpreter と同じ `X::TypeCheck::Binding::Parameter`）＋`&name` Sub の captured env を merge して
    閉包変数参照 where も解決＝byte-identical。`is_light_call_eligible`/`is_positional_light_call_eligible`（full binding を飛ばす fast path）は
    where 除外を維持。実測 where-multi 3 種で fallback 77.8%→0%。pin=`t/multi-where-otf-dispatch.t`(20)。
    **★同 PR で nextsame/callsame 候補順序バグも修正**: `resolve_all_multi_candidates` が HashMap 順だったため、複数候補が同一引数に
    マッチ（重複 where + generic fallback）すると nextsame が広い候補を先に拾い狭い候補を脱落させた（hash-seed flake で defer-next.t を CI が捕捉）。specificity 順ソートで決定化。
  - [x] **default param 値を持つ multi/proto 候補の OTF 化 = 完了（#3559 genuine-multi / #TBD proto 候補）→ [news/2026-06.md](news/2026-06.md)**。
    `def_is_otf_compilable` から `default.is_none()` を**全撤去**する単純版（PR #3546 close）は builtin-shadow パスで Test::Util
    `our sub run(Str, Str = '')` を OTF compile し name-keyed `otf_call_cache` を汚染→subtest 文脈で後続コア `run` を mis-bind
    （release roast で env.t/system.t/cur-current-distribution.t 回帰）。**安全実装＝genuine multi/proto 候補サイトのみ default を許可**:
    非proto multi fork（`has_multi_candidates_cached` ブランチ・#3559）＋ trivial/non-trivial proto 候補（`vm_resolve_trivial_proto_candidate`
    ・`vm_call_proto_dispatch`・#TBD）。これらは `compile_and_call_function_def` の caching profile が default 有無で不変（非default 候補が
    既に安全に OTF されている）＝汚染ハザードが構造的に起きない。新 `def_is_otf_compilable_multi_candidate`（code_signature のみ除外）を適用。
    単一/builtin-shadow/非builtin 単一パスは `def_is_otf_compilable`（default 除外）維持。回帰した env.t/system.t/cur-current-distribution.t
    + subtest/Test::Util 系ローカル全 PASS。pin=`t/multi-default-otf-dispatch.t`(14・proto default 含む)。
    **残**: builtin-shadow 単一 / 非builtin 単一の default-OTF（name-cache 汚染リスクで除外維持）。
  - [x] **非trivial proto body の VM 化 = 完了（#3550 ①body compiled / #3552 ②候補 OTF）→ [news/2026-06.md](news/2026-06.md)**。
    `proto foo($x){ say "x"; {*} }` の body＋`{*}` 候補ディスパッチを両方 compiled 実行（`vm_try_run_nontrivial_proto_body`
    ＋`vm_call_proto_dispatch`）。実測 `t/proto-nontrivial-body-vm.t` で interpreter_fallbacks 50.1%→0.5%。
    pin=`t/proto-nontrivial-body-vm.t`(13)/`t/proto-candidate-otf-dispatch.t`(7)。
  - [x] **`{*}` を proto の現在パラメータで再ディスパッチ（scalar rw/raw・proto sub #3556 / proto method #TBD）= 完了 → [news/2026-06.md](news/2026-06.md)**。
    `proto pr($x is rw){ $x=99; {*} }`/`multi pr(Int $x is rw){ $x=$x+1 }` で `pr($v)`（$v=10）が raku=**100** に対し mutsu=**99**
    （候補が body 変異済 `$x`=99 を見ず・候補の rw write が消える）だった。**★前回「§C varref container と絡む（重い）」と分類したが
    arg_sources 機構で解決可と判明（terminator 同型の誤分類）**。修正＝`proto_rw_redispatch_args`（proto param_defs を取る）:
    proto が scalar rw/raw fixed-positional param を持つとき（`%_`/`@_` catch-all は除外）、`{*}` 時点の **proto param 現在値**を
    rebuild（VM sub=live body locals〔scalar rw param は mid-body slot-only で env 未 flush〕／interpreter method=env）＋
    **arg_sources=proto param 名**を resolution 前に `set_pending_call_arg_sources`。候補の `is rw` は plain 値でも arg_sources で
    writability チェックを通り（`args_matching.rs:211` `has_arg_source`）、候補の writeback が proto frame に着地→call-site の
    writeback drain→proto exit の `apply_rw_bindings_to_env` が caller へ伝播。VM sub=`vm_call_proto_dispatch`（#3556）／
    interpreter proto method=`call_proto_dispatch` の method_ctx 分岐（#TBD・`lookup_proto_method` で proto def 解決）。
    pin=`t/proto-rw-redispatch-coherence.t`(9・sub) / `t/proto-method-rw-redispatch.t`(5・method)。
    **残ギャップ（別軸）**: **nextsame+rw チェーン**（first 候補の rw は伝播するが nextsame で次候補へ渡る rw write は
    `multi_dispatch_stack` 別機構で消える・2 候補ケース 40→41 改善も raku=1041 未達）。
  - [x] **`&`-code param を持つ候補の OTF 化 = 完了（#TBD）→ [news/2026-06.md](news/2026-06.md)**。`def_is_otf_compilable` の
    `!pd.name.starts_with('&')` ガードを撤去（`code_signature`〔`&cb:(Int)`〕と default 値の除外は維持）。`multi f(&cb){…}` 候補が
    bare multi / proto 経由とも compiled 実行され interpreter fallback を解消（proto `&cb` 経由 25%→0%）。block literal / 引数付き
    callback / `&name` 渡し / outer 変数を閉包する callback まで byte-identical（pin=`t/multi-amp-param-otf-dispatch.t`(8)）。
  - [x] **非builtin モジュール/動的 single sub の OTF 化（default-param 含む）= 完了（#TBD）→ [news/2026-06.md](news/2026-06.md)**。
    `use Mod; greet(...)` のような **importing scope の compiled_fns に無い** non-builtin single sub は `user_function_matches_call`
    ブランチ経由で常に tree-walk していた（module sub は 100% interpreter fallback）。`is_builtin_function` ゲートを「builtin-shadow は
    `def_is_otf_compilable`（strict・default 除外）／非builtin は新 `def_is_otf_compilable_module_single`（default 許可）」に分岐。
    **非builtin single の default は name-cache 安全**（同名 builtin が無く mis-bind 不可・single 候補は常に同 def 解決・light-call fast path は
    default 除外で slow binding が default 評価）。**保守ゲート**＝state（並行共有セル）・sigilless/`is rw`/`is raw`/code-sig/sub-sig/trait 付き param・
    nested routine decl・`subtest`/`CATCH`/`CONTROL`/phaser・`start`/`EVAL`/`EVALFILE`（CALLER context）・`is test-assertion` を全除外
    （`module_otf_body_needs_interpreter` 再帰スキャン）＝interpreter 結合構文は fallback 維持。実測 module `greet($n,$g="Hello")` 100%→0%。
    回帰 3 件を捕捉・修正（concurrent-state-var＝state 共有／sigilless-params＝EVAL 経由 alias writeback／throws-like-any＝subtest+CATCH+EVAL）。
    pin=`t/module-sub-otf-dispatch.t`(14)。
  - [ ] **残**: bare multi の残フォールバック（`@_` slurpy recursive sub 等は別カテゴリ・`@a[1..*]` 再帰の immutable-List bug は §F）/
    `code_signature`〔`&cb:(Int)`〕param を持つ候補の OTF 化（依然除外・別軸で `&cb:(Int)` vs `&cb` の resolution ambiguity も要）/
    default-param OTF の **builtin-shadow 単一候補**（name-cache 汚染リスクで除外維持・PR #3546）/ nextsame+rw チェーン（上記）/
    モジュール sub OTF の interpreter 結合構文（state/EVAL/CATCH/sigilless 等）＝保守ゲートで除外中・compiled_fns 拡充が本筋。

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
