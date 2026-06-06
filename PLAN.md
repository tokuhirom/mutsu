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
- [x] **完了済み（詳細は [news/2026-06.md](news/2026-06.md)）**: `sprintf`/`zprintf` (#2601)、
      属性アクセサ読み (#2604)、`.new` デフォルト構築 (Step 5)、`.map` 単純ブロック (#2619)、
      `.subst`（Regex/Str・`:g`, #2621）、`.sort`（比較子なし・単純 `{$^a<=>$^b}`/`cmp`, #2622）、
      `.map` 多 arity + pointy ブロック (#2623)、**Test 関数のディスパッチ層** (#2625)。
      **注意**: map/grep/sort/subst の「フォールバック」は**ブロック本体ではなく orchestration の入口のみ**
      （本体は既に `run_compiled_block`(`VM::new`) で VM コンパイル実行）。これらの native 化は
      ツリーウォーク削減ではなく**計測上のフォールバック率改善**で、`.new`（interpreter 実行経路を除去）
      とは性質が違う。
- [x] **`.grep` の rw ビュー writeback（for ループ経路を実装）**: `@a.grep(...)` の結果は元配列スロットへの
      rw エイリアスで、`for @a.grep(...) { $_++ }` / `$_++ for @a.grep(...)` / `-> $x is rw` が元を更新する。
      既存の grep-view binding（filtered 結果の Arc をキーに `(source Arc, matched indices, kind)` を保持）を
      for ループ writeback でも参照し、ループ終了時に修正トピック／rw パラメータを source の matched スロットへ
      Arc-identity 上書きで反映。**transient 限定ガード**: `container_binding`（= for の iterable が名前付き
      `@`変数）が立つ場合は name-based writeback が所有するので grep-view writeback を抑止し、
      `my @g = @a.grep(...); for @g { $_++ }`（`=` で decontainerize 済み）が @a へ漏れないようにした。
      pin `t/grep-rw-view.t`（10件）。**残**: `@a.grep(...)[i] = v` の lvalue index 代入、および
      `my @g = @a.grep(...); @g>>++` の `=` 後 hyper writeback は依然 binding が残り元を更新する既知の
      pre-existing バグ（Arc-pointer keyed binding が `=` 代入を跨いで残存）。完全な rw aggregate binding
      （配列要素を共有コンテナ化）は別途。
- [x] **map の残（完了）**: (1) Pair 要素 — native map が Pair でも `$_`/単一 plain positional を明示
      バインド（`call_compiled_closure_with_topic`）。implicit-`$_` ブロックと pointy `-> $p` を native 化。
      placeholder (`$^a`) / multi-arity over pairs はフォールバック維持。(2) `$_` mutation 書き戻し
      (rw エイリアス) — mut method 経路の `target_name` を native map に渡し、ブロックの最終 `$_` を
      `rw_map_topic_capture` で捕捉して `@`-配列変数へ書き戻し（`@a.map({ $_++ })` が `@a` を変異）。
      型メタ (`my Int @a`) も再登録で保持。**副産物: 既存の `$_++`/`$_--` 書き戻しバグも修正**
      （旧 `__mutsu_rw_map_topic__` シグナル経路は increment を取りこぼし `[1 2 3]`、raku は `[2 3 4]`）。
- [x] **Test 関数の TAP 状態所有を Interpreter god-struct から分離（A で終着・#2659）**: #2625 は
      **ディスパッチ層のみ**。本体 (`is`/`ok`/`is-deeply`/`subtest`…) は TAP カウンタ・TAP 出力・method
      dispatch (`.gist`)・`EVAL`・subprocess に本質的に結合。**真のワート（コアの Test 特別扱い）は既知負債
      として下に記録・低優先 deferred**。
    - **調査で判明**: 当初「VM へ」とあったが、VM は `run.rs` の `std::mem::take(self)` で Interpreter を
      move-in/out する**一時オブジェクト**（`run_compiled_block`/EVAL/subtest/closure ごとに `VM::new(interp)`）。
      TAP 状態は1プログラム実行を通じて多数の sub-VM をまたいで持続・共有される必要があり、VM 構造体の
      フィールドには置けない。永続オーナーは Interpreter。よって正しい着地点は VM フィールドではなく
      **Interpreter から独立した carrier**。
    - **Step A 済（#2659 マージ）**: 4つの生フィールド（`test_state`/`subtest_depth`/`subtest_callable_is_sub`/
      `bailed_out`）を **`TapState` 構造体**（`src/runtime/tap_state.rs`）にカプセル化し、~50箇所の直接
      フィールドアクセスを `TapState` の API（`ensure_state`/`state`/`begin_subtest`/`end_subtest`/
      `clone_for_thread`/`bailed_out` 等）経由に統一。神オブジェクトの名前空間から分離した。
    - **`Rc<RefCell>` 版（当初 Step B）は却下**: Interpreter は `spawn_user_thread`（`builtins_system.rs:119`、
      境界 `F: Send`）で OS スレッドへ `move` されるため `Interpreter: Send` が必須。`Rc<RefCell<TapState>>` は
      `!Send` でビルド不能。`Arc<Mutex>` はスレッド別 plan/failed セマンティクス（現状 `clone_for_thread` が
      カウンタのみ `Arc` 共有）を壊す。→ TAP 状態は **value 所有 + 自前 thread-clone** が構造的に正解
      （Perl の `MY_CXT`/`CLONE` ithreads と同型）。
    - **真のワート（既知負債・低優先 deferred、ユーザー方針 2026-06-06）= コアに焼き込まれた Test 専用
      ディスパッチ機構**。A は状態所有を整理しただけで、これには未着手。Test *だけ*がコアに名前を知られている:
      - `is_test_function_name()` の **~50 Test 関数名ハードコードリスト**（`test_functions/mod.rs`）。
      - VM ディスパッチ4ファイルの Test 固有分岐（`vm_call_dispatch.rs`/`vm_call_func_ops.rs`/
        `vm_var_get_ops.rs`/`vm_native_test.rs`）+ `try_native_test_function`/`is_interpreter_handled_function`/
        `test_module_loaded`。
      - 経緯: 初期に mutsu が `Test.rakumod`（pure Raku）を走らせられず、速度と bootstrap のため Rust builtin
        で再実装しディスパッチに名前直書きしたため。他のユーザモジュールはこの特権を持たない。
      - きれいに消す道（どちらも ROI 低・deferred。最終ゴールが Test を**本質的例外**と明記）:
        1. **generic native-module table 化（安い方）**: `use` ロード時にモジュールが関数名を汎用テーブルへ
           登録し、VM は Test 固有分岐ゼロでそれを引く。Rust 実装は残すがコアから Test 知識を撤去。
        2. **Test-as-Raku-module（純粋な方）**: `Test.rakumod` を mutsu 上で eval（= 実 Rakudo と同形）。
           コアの Test 知識が完全消滅するが多数の前提機能に依存する大事業。
      - 結論: TAP は **A で終着**。上記2案は負債として記録のみ、当面着手しない。
- [ ] `EVAL` / symbolic deref / `CALLER::` — **tree-walk ではない**。`EVAL` の実行は既に
      `eval_block_value` で compile→バイトコード→サブ VM 実行（`run_compiled_block` が `VM::new(interp)`）。
      残る結合は (1) 入口が `Interpreter::call_function` 経由でフォールバック計測に乗る計測ノイズ、
      (2) 共有可変状態（env/classes/roles レジストリ）の所有が Interpreter 構造体である点。後者は
      **レバー B（状態オーナーシップ）に収斂**する課題であり「恒久的に interpreter でよい例外」ではない。
      symbolic deref / `CALLER::` も reflective な共有 env アクセスが要るだけで tree-walk は不要。

### レバー B: `locals`↔`env` 二重ストアの解消（共有状態結合の本丸, §1.2）
- [x] **Slice 1–4c**: 計測 + 環境の base-tier 化 + closure capture を free-var に限定 + 各種 format!/Symbol
      churn 除去（docs/vm-dual-store.md 参照）。
- [x] **Slice 5 step 1** (#2608): slot-only local（GetLocal でしか読まれない）を Interpreter env に
      ミラーしない。`ensure_env_synced` を `needs_env_sync` でゲート。ループ/ブロックは制御テンポラリの
      env 往復のため保守的に全 sync。
- [~] **Slice 5 collapse proper（次の本丸・着手中）**: per-call **scoped/overlay env**（呼び出しフレーム
      ごとの子スコープ、復帰で破棄）を導入。これで callee の env 書き込みが caller を汚さなくなり、
      param-bind の env 書き込み・`env_dirty` 起因の post-call pull・ループ保守フォールバックを一掃。
      `clone_env()`/dirty 追跡（env_dirty/locals_dirty/locals_dirty_slots）の撤廃へ。
    - **設計（採用方針）**: `Env` に `parent: Option<Arc<HashMap<Symbol,Value>>>` を追加した3層オーバーレイ
      （overlay → parent → GLOBAL_BASE）。`parent=None` で現状とバイト一致（全 ~80 イテレーション消費者の
      契約不変＝後方互換）。**不変条件**: `insert/remove/get_mut` は overlay のみ、`get/contains` は親→base へ
      フォールスルー、`iter/keys/values/len` は overlay-only（= callee 自身の書き込み）。merge-back は
      overlay-only を走査するので **O(full-env) → O(callee-writes)** になり、callee-local の書き込みは
      overlay を捨てるだけで破棄される。scoped env は fast フレームの直接実行中のみ live にし、
      `clone_env()`（nested call / block / thread が env を捕捉する箇所）では flatten して安全性を担保
      （`clone_for_thread` 等の overlay-only イテレーション消費者が親レキシカルを取りこぼさない）。
    - **これはアーキテクチャ改善であり、ROI で取捨選択しない**（ユーザー方針 2026-06-06、
      `memory: feedback_architecture_over_roi`）。collapse は **全呼び出しパスを scoped overlay へ移し、
      dirty 追跡と双方向 sync を完全撤廃**して初めて完了する。途中の perf や個別パスの ROI で
      named/positional_light 等を飛ばさない（飛ばすと coupling が残り collapse 未完）。物差しは
      「完成したクリーンアーキテクチャまでの到達速度」。
    - **全パス変換チェックリスト**（各スライスは `make test` + 関連 roast をローカル検証、全 roast は CI）:
      - [x] `call_compiled_function_fast`（0-arg helper）pilot — #2647
      - [x] `call_compiled_method_fast`（method fast path、born-owned overlay、accessor flatten 回避）— #2648
      - [x] `call_compiled_function_positional_light`（name-based param-env juggling を overlay drop に置換）
      - [x] `call_compiled_function_light`（named-param light）
      - [x] `call_compiled_function_named`（heavy path）— **完了**。当初の回帰（overlay 親フォールスルーが
            `:=` と衝突: `for @t -> \d,\seed,\endpoint,\result { ts(...) }` の raw param sub 内
            `my $result; $result := list.List` で「Cannot modify an immutable value」）は **`Env` の tombstone
            機構**で根治。原因: `my $result` 宣言が外側 `\result` の `__mutsu_sigilless_readonly::result` を
            `env.remove()` でクリアするが、overlay の remove は親キーを shadow できず親の readonly が残存。
            `Env::remove` を scoped env では tombstone（削除マーカー）にして `get`/`contains` が親へ
            フォールスルーしないように修正（"clear inherited state" イディオム全般を overlay 下で正しく動作させる
            一般修正）。pin `t/scoped-overlay-named.t`、`roast/S03-sequence/exhaustive.t` で検証。
      - [x] `call_compiled_method`（method 非 fast path）
      - [x] `call_compiled_closure` / `vm_closure_dispatch`（closure capture/writeback）— overlay 親=flat caller、
            capture-merge を born-empty overlay へ、exit writeback は overlay-only。advent integration + closure pins green。
      - [x] `force_lazy_list_vm` / `_n`（gather/lazy-list）— overlay 親=gather captured env(`list.env`)。coroutine は
            suspend で scoped env を coro 状態に保存、resume で復元（clone が overlay+parent+tombstone 保持）。
      - [x] **Slice 6.1**: compiled fast パスの **per-call dirty churn を撲滅**。全パス overlay 隔離後、
            compiled 呼び出しが行っていた dual-store sync は「ゼロ仕事のオーバーヘッド」だったと判明。
            `bench-fib`(fib 27) で `env_flushes 317810→0`、`locals_pulls 317811→0`（出力不変）。
            根因2つ: (1) `positional_light` の bind 時 mark-all-params-dirty（slot-only param まで
            `locals_dirty` を立て no-op flush を毎回誘発）、(2) compiled 呼び出し後の **無条件 `env_dirty=true`**
            （ultra-fast positional_light/light/OTF cache + dispatch tail）。compiled fast パスは
            scoped-overlay merge が captured-outer write 時のみ env_dirty を精密に signaling するので blanket set は冗長。
            interpreter/native フォールバックは自分で env_dirty を立てる。**named(heavy)パスは `is rw`/`is raw` の
            caller writeback（param=callee-local で merge がスキップ）のため保守的に env_dirty=true 維持**（ホットパスではない）。
            pin: `t/is-rw-traits.t` / `t/scoped-overlay-named.t` / `t/scoped-overlay-env-dirty.t`。docs/vm-dual-store.md Slice 6.1。
      - [x] **Slice 6.2**: `locals_dirty` → **write-through 化、dirty フラグ機構を削除**。`needs_env_sync` slot の
            書き込みを書き込みサイトで即 `flush_local_to_env` で env へミラー（遅延 `ensure_env_synced` batch flush を撤廃）。
            env が常に locals と coherent になり stale window が消えるので、pre-read flush バリアと dirty 追跡を全削除:
            `ensure_env_synced`(関数+~30 呼び出し)、`mark_local_dirty`(→ `flush_local_to_env` 22 サイト)、
            `locals_dirty`/`locals_dirty_slots`(VM フィールド+全 save/restore)、VmCallFrame の saved_*、
            `GatherCoroutineState::locals_dirty_slots`。**監査は想定より小**: `ensure_env_synced` は mark された slot のみ flush
            していたので、mark しない ~82 直接 `self.locals[idx]=` は元々 flush されず無影響＝22 mark サイトのみ変換。
            **Net -135 行**。fib は dual-store sync ゼロ維持、perf bench で env_flushes=0（write-through は free-var/closure-captured
            slot 書き込み時のみ発火、ホットループ退行なし）。make test + S17 atomic/CAS/thread + closures roast green。docs Slice 6.2。
      - [ ] **Slice 6.3（最後のフラグ）**: `env_dirty` → tree-walking interpreter が env を名前変更した後の slot 再 sync 用。
            VM op の interpreter フォールバックが残る限りクリーンに撤去不可（VM decoupling 完了に gated）。

### レバー C: クロージャ upvalue 化（§1.3）
- [x] **Slice 4a**: closure 存在時の保守的 `needs_env_sync.fill(true)` を撤廃、free-var 集合に限定。
- [x] **サブゴール#2（`&?BLOCK`/`__mutsu_callable_id` の setup 書き込みを共有 env から外す）は
      Slice 6 で実質達成**。`call_compiled_closure` はクロージャ呼び出し冒頭で born-empty な
      scoped overlay を install し（`Env::scoped_child`）、`&?BLOCK`/`__mutsu_callable_id`/param 束縛/
      捕捉 env merge はすべてその overlay に書かれ、return で `frame.saved_env` 復元により破棄される。
      共有/caller env を汚さない。残るのは upvalue 化（#1）の本丸のみ。
- [x] **クロージャ捕捉の正しさ — for ループ変数の per-iteration binding（#2659 系の続き）**: 現行モデルは
      free var を env から**名前で遅延読み**するため、捕捉値の精度はクロージャ呼び出し時の call-site env が
      持つ値に依存する。`exec_for_loop_int_range`（`for 1..N -> $x` の高速パス）が named param の保存・
      `for_param_restore_stack` への push を行わず、コンパイラが発行する `RestoreForParam` opcode と
      非対称だったため、`$x` がループ後に**最終値で enclosing scope へリーク**。ループ後に呼ばれる
      クロージャ `{ $x }` は、正しく凍結された捕捉値（COW で分離済み）ではなく、リークした最終値を
      don't-overwrite merge 経由で読み、全クロージャが最終値を返していた（`for 1..3 -> $i { @c.push({$i}) }`
      → mutsu `333`、raku `123`）。`exec_for_loop_body` と同じ save/restore プロトコルを int-range パスに
      追加して根治。pin `t/loop-var-closure-capture.t`（14件）。
- [x] **upvalue 化 Slice 1: loop-body-local `my` の per-iteration クロージャ捕捉（owned_captures）**。
      ループ本体の `my $x`（および `for -> $x` param）は反復ごとに fresh binding なので、本体で作られた
      クロージャは「その反復の値」を捕捉すべき。だが現行モデルはクロージャが free var を **env から名前で
      遅延読み**し、`my $x` は VM スロットを持つため、ループ後 `sync_env_from_locals` が**スロット値
      （最終反復値）を caller env に無条件で再注入**し、全クロージャがそれを拾って最終値を返していた
      （`for 1..3 { my $x = $_; @c.push({$x}) }` → `333`、raku `123`）。env teardown だけでは
      この slot 再注入に勝てない（dual-store 結合）。**根治: ループが `loop_local_vars` スコープを push し、
      本体の `my` 宣言を登録。クロージャ生成時に loop-local な free var を `SubData::owned_captures` として
      マークし、呼び出し時にそれらを自身の凍結 captured env（COW で反復ごとに別値）から読み、caller env を
      上書き**する。これで slot 再注入に免疫化。`owned_captures` の上書きは per-instance state
      （`closure_captured_state`）より**前**に適用し、ループ内の変異クロージャの累積状態は維持
      （`for 1..2 { my $x=$_; @c.push({$x++;$x}) }` → `2,3,3`）。共有レキシカル（非ループ）は従来どおり
      遅延束縛（`my $g=1; my $c={$g}; $g=9; $c()` → `9`）。全ループ種別（for 両パス/while/C-style/repeat、
      gather suspend 含む）に適用。pin `t/loop-body-local-closure-capture.t`（15件）。
- [x] **upvalue 化 Slice 2: 共有 `ContainerRef` セルで loop-body-local の intra-iteration mutation を正しく**（PR #2669）。
      box-on-capture: ループ本体内で生成されるクロージャの被捕捉 **loop-body-local** スカラ（`loop_local_vars`
      追跡 + `code.locals` に slot）を slot+env 両方で `ContainerRef` に置換してから env をスナップショット →
      Arc 共有で捕捉後変異が見える（`for 1 { my $x=1; my $c={$x}; $x=2; $c() }` → `2`）。同一反復内の兄弟
      クロージャ共有も対応。per-iteration freshness は vardecl 時に slot の stale ContainerRef もクリアして維持。
      付随で Pre{Inc,Dec} の ContainerRef deref 欠落と for-loop rw writeback の `$pair.value` clobber も修正。
      pin `t/closure-container-capture.t`。**スコープを loop-body-local に限定**したのが要点 — 全被捕捉スカラを
      box すると非ループ捕捉（Test の `lives-ok {}` が周囲の `$obj`/型オブジェクト等を捕捉）が ContainerRef
      未対応の多数経路（不変性チェック・型オブジェクト dispatch・`.kv` rw writeback 等）を踏んで広範囲に壊れる
      （CI で mix/submethods/subset/recursive/kv/pairs/cas 等が失敗→限定で解消）。
- [ ] **残（延期）: 非ループの兄弟クロージャ捕捉変数共有**（`my ($g,$s)=make(); $s(42); $g()` が raku `42` /
      mutsu `1`）。一般的な「クロージャはコンテナを捕捉する」意味論で、全被捕捉スカラの ContainerRef 化 +
      全読み書き/型/不変性経路の deref 監査が必要。Slice 2 の限定スコープでは未対応（`t/closure-container-capture.t`
      で該当 2 ケースを `todo`）。
- [ ] **残（別根）: body-local `my` の env leak**（`for 1..3 { my $x=$_ }; say $x` が raku 未宣言エラー、
      mutsu は最終値）。クロージャ捕捉は Slice 1/2 で免疫化済みだが、非クロージャの post-loop 名前読みは
      依然リークを拾う。これは compile 時のブロックスコープ解析（block-scoped slot/teardown）の問題で、
      コンテナ化とは別根。Slice 3 として別 PR で。

### 最終ゴール
- [ ] メソッド/関数フォールバック率を 0%（Test/EVAL 等の本質的例外を除く）にし、
      Interpreter のメソッド/関数実行パスを削除。当面は残フォールバックに
      `// TODO: compile to bytecode` を付け負債を可視化。

**次の着手候補（優先順）:** B の scoped/overlay env（C の upvalue・Test TAP 状態移管の前提にもなる本丸）
→ A の `.grep` rw ビュー設計 / Test `TestState` 所有移管 → C の upvalue。

---

## 🟣 第2優先（インタープリタ廃止の次）: 第一級コンテナ (container identity) への移行

**優先順位**: 上の「🔴 最優先 = バイトコード VM をちゃんと治す（tree-walking Interpreter 廃止）」を
**第1優先**、本セクションを**第2優先**とする（ユーザー方針 2026-06-06）。両者は独立ではなく地続き
（レバー B/C の本丸が本移行の前提・一部）なので、インタープリタ廃止の完了を待ってから本格着手しつつ、
その尾部（レバー C upvalue 等）と自然に接続する。

実装台帳: [docs/container-identity.md](docs/container-identity.md)（現状の地図・段階スライス・進捗ログ）。

最終ゴールは **世界最高の Raku インタープリタ ＝ 最速かつ最もメンテしやすい** こと。その物差しで、
インタープリタ廃止の次に最大の構造的負債かつ最大の正しさブロッカーは「mutsu が値 (`Value`) を裸で持ち、
Raku の**コンテナ**（`Scalar` / 配列・ハッシュ要素セル / 属性セル）の identity を持たない」こと。これは
BLOCKERS の複数セクションを横断して塞いでおり、roast を個別に潰す作業では絶対に届かない。
**VM decoupling のレバー C 本丸 ＝ Q2 の Arc-pointer-keying flaky ＝ この一点に収斂する。**

### なぜ最優先の戦略課題か（インパクト）

裸 Value モデルが直接ブロックしているもの（一例。単発では「小さな別バグ」に見えるが根は1つ）:

- **束縛 vs 代入**: `my $x := (1,2,3)` がリストとして平坦化されない / `.VAR` が常に `Scalar`
  （reduce.t 62, S02 variables-and-packages 16件、`:=`-束縛コンテナ識別）
- **`=:=` / `.VAR` / itemization**: コンテナ identity が無く `$(...)` と裸値を区別できない
- **`is rw` / `is raw` / take-rw**: 呼び出し側コンテナをエイリアスできない（gather.t 38, S06 各種,
  S12-methods/accessors, S12-attributes/instance）
- **配列/ハッシュ要素の lvalue**: `@a[0] := …`, `>>++` の深い変異, `deepmap(++*)`/`*--`, object-hash
  （hyper.t 330-333, classify.t Junction キー, S03-binding/nested）
- **兄弟クロージャの捕捉変数共有**: `my ($g,$s)=make(); $s(42); $g()` が共有されない（レバー C 本丸）
- **属性へのバインド**: `$!x := …` / per-attribute container template（S03-binding/attributes,
  S14-traits/attributes 5-8）
- **型メタの flaky**: 副テーブルの Arc-pointer-keying（後述 Q2 項目、S02-names-vars/perl.t 等の間欠 die）は
  「コンテナ自身が安定 identity を持たない」ことの裏返し

→ **一度きちんと入れれば 30+ テストと複数の flaky が同時に解け、以後この種の workaround を書かなくて済む。**

### なぜ過去のプロトタイプは失敗したか — "deref everywhere" 問題

「捕捉スカラーを `ContainerRef` に昇格」する素朴な試みは、ローカルでは tests 18-20 を直したが roast を
広く回帰させた（BLOCKERS S02 節）。原因は、値を消費する**全 op**（算術・比較・ディスパッチ・型チェック・
出力・coercion … 数百サイト）が deref を要し、1つでも漏れると `ContainerRef` が値コンテキストに漏れて
誤動作すること。コンテナを「足す」だけのアプローチは消費面が広すぎて破綻する。

### 設計の鍵: deref を「散在」させず「単一チョークポイント」に集約する

Rakudo/MoarVM が実証する解法 ＝ **decont（脱コンテナ）を 1 箇所に集約**する。コンテナは*格納サイト*
（変数スロット・配列/ハッシュ要素・属性）にのみ存在し、*値読み出し*は VM のオペランド取得経路という
**唯一のチョークポイント**で必ず decont する。op はスタックから「既に decont 済みの値」を pop するので、
算術・比較・ディスパッチ op は**一切変更不要**。コンテナが見えるのは、明示的に lvalue/コンテナを要求する
数少ない経路（`:=` bind, `is rw`, `.VAR`, `=:=`, take-rw, itemization 判定）だけ。
これで消費面が「数百の値 op」→「一握りの lvalue op」へ**反転**する（列挙可能で扱える）。これが素朴版との
決定的な違い。

### 段階導入（big-bang 回帰を避ける順序）

- [ ] **Phase 0 — decont チョークポイント整備（挙動不変リファクタ）**: 値スタックの不変条件を「常に
      decont 済み」とし、全読み出しをそこへ集約。lvalue 専用 opcode（`GetLocalContainer`/`IndexContainer`
      等）を追加。コンテナはまだ殆ど存在しないので**挙動は不変** ＝ roast 完全一致で検証。単一サイトを
      先に確立する（以後の Phase の安全網）。
- [ ] **Phase 1 — スカラーの第一級コンテナ化**: `$` 変数が `Arc<Scalar>` セルを持つ。`=` はセルへ格納、
      `:=` は束縛差し替え、itemization はセル wrap。→ 束縛/平坦化（reduce.t 62）, `=:=`/`.VAR`,
      兄弟クロージャ共有（レバー C 完了）, S02 変数捕捉, S03-binding を解消。
- [ ] **Phase 2 — 配列/ハッシュ要素のコンテナ化**: 要素を COW な `Arc<Vec<Scalar>>` 等のセルに。
      → take-rw（gather.t 38）, `@a[0] :=`, 深い `>>++` / `deepmap(++*)`（hyper.t 330-333）, object-hash,
      S12 accessors/instance。最もホットな表現に触るので Phase 1 の後。
- [ ] **Phase 3 — 属性コンテナ + 属性束縛**: `$!x :=` / per-attribute container template
      （S03-binding/attributes, S14-traits/attributes 5-8）。

各 Phase は `make test` + 関連 roast をローカル検証、全 roast は CI。Phase 0 は挙動不変なので安全に
大きく入れられる（CLAUDE.md「Refactor boldly」）。

### 「最速 × メンテしやすい」をどう両立するか

- **メンテ性（直接の勝ち筋）**: 統一コンテナモデルは散在する workaround を**削除**する — dual-store
  env↔locals、Arc-pointer-keyed 副テーブル（＝ flaky の根。下記 Q2 項目を吸収）、ad-hoc itemization
  フラグ、grep-rw-view binding、name-based writeback reconcile。**1 つの概念が十数個の特例を置換**する。
  これが「世界最高 ＝ 最もメンテしやすい」の核。
- **速度（設計で担保）**: コンテナは間接参照を足すので、(a) **エスケープ解析でコンテナを省略** — 捕捉も
  `.VAR` もエイリアスもされないローカルは裸値のまま（コンパイラが判定。MoarVM の spesh と同型）、
  (b) **配列は COW** で読みはクローン無し、(c) decont は単一分岐で予測が効く、(d) 中期の NaN-boxing で
  payload 8 byte 化すればセルも安価。pervasive container でも spesh/escape で高速化できることは Rakudo が
  実証済み。mutsu の賭けは「コンテナ + エスケープ解析で hot path から消す」。

### 既存項目との関係（重複ではなく収斂）

- レバー C「本丸: 自由変数を indexed upvalue（`ContainerRef`）として捕捉」は **Phase 1 の一部**として完成する。
- Q2「コンテナ型メタを安定 ID へ移す」は **本移行に吸収**される — コンテナが identity を持てば型メタは
  コンテナ自身に載り、Arc-pointer-keying と flaky が構造的に消える。
- レバー B（scoped overlay env）は Phase 1 の前提（変数の所有を env に集約済みであること）であり地ならし。

**着手順**: レバー B 完了 → Phase 0（チョークポイント・挙動不変）→ Phase 1（スカラー）→ Phase 2（要素）
→ Phase 3（属性）。

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
- [ ] **コンテナ型メタデータを生 Arc ポインタ keying から安定コンテナ ID へ移す**（間欠 flaky の根本原因。
      ANALYSIS §2.3 の `Arc::as_ptr as *mut` エイリアスと同根の Arc-ポインタ-identity 不健全性）—
      **※ 上の「🟣 第2優先: 第一級コンテナ」に吸収される（コンテナが identity を持てば型メタはコンテナ
      自身に載り、ポインタ keying と flaky が構造的に消える）。単独で着手せず本移行の一部として扱う。**
      `array_type_metadata`/`hash_type_metadata`/`set`/`bag`/`mix` の 5 マップが `Arc::as_ptr as usize`
      をキーにしており、コンテナ drop 後にそのポインタが無関係の後続アロケーションに再利用されると、
      stale な型情報がそちらに aliasing する。typed 配列 `@.items` の `Item` 要素型が `EVAL` の生成
      リストに乗り移って `Int` を `Item` と型チェックし die（`roast/S02-names-vars/perl.t` ~10%、exit 255 =
      plan 未完了）、object hash のキー制約が `.clone`/再構築コンテナに乗り移って読みが `Nil` になる
      （`roast/S02-types/hash.t` の `%a.clone` ブロック ~0.2%）。CLAUDE.md で「CI-load timeout」と誤分類
      されていたが、実体は alloc/hash 順依存の**正しさバグ**（テストは ~0.07s 実行・起動 ~0ms、perf 無関係）。
    - **済**: ハッシュ要素 READ 経路を #2635 で部分対処（name-based reconcile + stale 上書き復元、
      S09-typed-arrays/hashes.t を 0/500 に）。
    - **試して revert（2回・いずれも不成立）**: (1) メタに `Weak` を併存させ lookup 時 `Arc::ptr_eq` 検証
      する案は family 全体を 0/300 にしたが、native typed 配列（`my int @a`/`my str @a`）で
      `native-int.t` 240 件回帰 → revert。(2) ハッシュで効いた name-based reconcile（`var_type_constraints`
      から mut メソッド入口 + 要素代入で再登録）を配列にも一般化して Weak と併用したが、native 配列の
      240 件は**全く減らず**（reconcile が native 配列のメタライフサイクルに届かない）→ これも破棄。
      **結論: Weak + name-reconcile の安価なパッチは native 配列で行き止まり。** 部分対処の積み増しでは
      family を根治できない。
    - **本筋（次セッションはここから直接着手）**: 生ポインタ keying を**完全に廃止**し、型メタを
      **コンテナ Value 自体に載せる**（例: `Value::Array(Arc<Vec<Value>>, ArrayKind, Option<Arc<ContainerTypeInfo>>)`、
      Hash/Set/Bag/Mix も同様）か、**真の安定コンテナ ID**（生成時に採番し COW・再構築・`clone_for_thread`
      を跨いで保持）。前者は Value variant 署名変更で全構築/match サイトに波及する大改修だが、副テーブルの
      ポインタ再利用 aliasing を構造的に消せる唯一の道。hot path 全体に関わるため段階的に。再現手順・失敗
      した2手法の詳細はメモリ `project_known_failing_tests_reclassified` 参照。

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

- [ ] **Match キャプチャ番号付け / コンテナ kind**（`.caps`/`.chunks` の値と `Match.gist` 位置
      キャプチャ表示・ネスト Match の corner-quote は #2644 で実装済み。残は別根の2件）:
      (1) `$<x>=(...)` 名前付きキャプチャが positional スロットにも重複格納され `(\d)` の番号がずれる
      （`/$<x>=(\w)(\d)/` で raku は `x`+`0`、mutsu は `0`+`x`+`1`）。
      (2) `m:g//` 結果を `my @m` に代入後 `@m.gist` が `(…)` を返す（`say @m` は正しく `[…]`）—
      method receiver が結果を List-kind で見る dual-store ナンス。
- [ ] Lookbehind assertions (`<!after>`)
- [ ] `:Perl5` modifier edge cases

### Parser: 未実装演算子

- [ ] `ff` / `fff` — flipflop operators (8 variants)
- [ ] `==>` / `<==` — feed operators の **precedence の残**（基本動作・インライン `my @o`/`my $x`
      sink 代入・スカラー sink の Array 化は #2643 で実装済み）。`==>` は `=` より緩い結合のはずだが
      mutsu は強く結合する: `my @out = (1,2,3) ==> map {...}` は raku では `@out = (1,2,3)`（feed は
      map に流して捨てる）だが mutsu は `[2 4 6]`。`say [1,2,3] ==> grep {...}` も同様の結合差。
- [ ] `~<` / `~>` — string bitwise shift（raku 本体も "not yet implemented" のため優先度低。
      `~&` / `~|` / `~^` は実装済み）。
  - 実装済み（Backlog から削除）: `minmax`（メソッド `.minmax` + 中置）、`unicmp` / `coll`。

### Parser: メタ演算子

- [ ] Generalized negation meta (`!op`) — beyond `!~~` and `!%%`
- [ ] Hyper assignment (`@a >>+=>> 1`)
  - 実装済み（Backlog から削除）: Triangular reduction (`[\+]`, `[\*]` など)。

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
