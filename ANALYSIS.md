# mutsu コードベース分析

この文書は、mutsu のコードベースを読んだうえで、
「設計上どこまで整理できていて、何がまだ負債として残っているか」をまとめたもの。
バグ票の一覧ではなく、**アーキテクチャと健全性のレビュー**として読む想定。

初版: 2026-06-03 / rev2: 2026-06-15 / rev3: 2026-06-17 / **rev4: 2026-06-27 (単一ストア化 #3455・ユーザメソッド本体 tree-walk 撤去 §B #3680 を反映)**
方法:
- 調査エージェントによるサブシステム単位の精読
- 主張ごとの実機再現確認

すべての指摘には `file:line` の根拠を付けている。
再現可能な不具合は、実際に走らせて確認した。

> ## 解決済み (初版・rev2 の主要指摘 — 本文からは削除)
> 初版・rev2 の最重要指摘は実機再現でいずれも解消を確認したため、本文から除去した。
> 記録としてのみ列挙する:
> - **VM はバイトコードシム** → CP-3 collapse で bytecode VM が `Interpreter` 構造体へ完全統合
>   (別 struct・型エイリアス・`self.interpreter.*` 参照すべて消滅)。残課題は §1.1 に縮約。
> - **遅延リスト崩壊・無限 Range 即時展開クラッシュ** → `Seq`/`Range`/`grep` を pull モデルへ統一、
>   展開サイトを `materialize_capped`/`value_to_list` (`MAX_RANGE_EXPAND` キャップ) 経由へ。
>   旧 §8.7 (`.Supply` の無限 Range 未ガード展開) も解消済み (実機: `(1..Inf).Supply` → `1 2 3`)。
> - **並行 scalar / state 共有崩壊** → Track C で捕捉スカラ・`state`・hash/array 要素をライブ共有セル化。
> - **regex 毎マッチ再パース / validator・matcher 二重実装** → `REGEX_PARSE_CACHE` 導入、
>   `src/regex_validate.rs` 削除し単一パーサへ統合。
> - **roast fudge の全入力適用誤作動** → `MUTSU_FUDGE` でゲート化。
> - **`Value` enum 肥大** → 6 variant を Box 化し `size_of::<Value>()` を 72→48B に縮小 (guard でロック)。
> - **メインスレッドの panic→`X::` 変換境界不在** → `run`/`run_inner`/`run_range_guarded` で変換 (#3045)。
> - **worker thread の panic→`X::` 変換境界** → `start{}`/`Promise` は `guard_worker_panic` で、
>   `hyper`/`race` は #3214 で同経路に統一済み。catchable な Rust panic はもうプロセスを落とさない。
>   ユーザ指定サイズの確保失敗 (配列 autoviv 巨大 index・文字列リピート・shaped 配列宣言・`Buf.allocate`)
>   は全て `try_reserve` でガード済み (§5)。残る理論上の abort は正当な操作からの真の OOM のみ。

---

## 0. サマリ

mutsu は Rust 製の minimal Raku 互換インタプリタで、`roast-whitelist.txt` は **1285 件**まで伸びている。
テスト通過のためだけに出力を偽装しているような、露骨なハードコードは見当たらない。

初版で問題視した大きな設計負債のうち、次はすでに解消済み:

- VM シム構造
- 遅延評価まわりの大きな崩れ
- 並行共有の主要な破綻
- `locals ↔ env` 二重ストア
- ユーザメソッド本体の tree-walk 実行

特に重要なのは次の 2 点:

- **`locals ↔ env` 二重ストアは単一権威ストア化で解消済み**（#3455）
- **ユーザメソッド本体の tree-walk 実行は撤去済み**（§B #3680）

そのため、ここで挙げる残課題はどれも
**「基本設計が破綻している」という種類ではなく、設計・健全性・保守性の負債**に寄っている。

Rust panic についても、catchable なものはメイン/worker の両スレッドで `X::` に変換されるため、
直接プロセスを落とす経路はかなり減っている。
いま理論上残る abort は、`catch_unwind` では捕捉できない**真の OOM**が中心で、
ユーザが巨大サイズを直接与える主要経路は `try_reserve` でガード済み（詳細は §5）。

### 先に結論だけ知りたい場合

- §1: 主要なアーキテクチャ課題はどこまで片づいたか
- §2: いま残っている正しさ・健全性の論点
- §4: ハードコードやドリフトの危険箇所
- §7: 優先度つきの実行順

---

## 1. アーキテクチャ評価

### 1.1 tree-walk 実行の残存 — ユーザコード本体は撤去済み

CP-3 collapse で VM と Interpreter の二重構造は消えた。
さらに §B キャンペーンで、**ユーザメソッド本体を tree-walk で実行する経路も撤去済み**。
`run_instance_method_resolved` にあった非 delegation arm
（`run_block(method_def.body)` の約 470 行）が削除され、
**ユーザメソッド/サブ本体の実行エンジンは bytecode VM に一本化された**。

いま残っている tree-walk は、「ユーザコード本体の実行」ではなく、次の周辺処理だけ:

- **宣言登録** (`class`/`role`/`enum`/`sub`/`method` の `register_*_decl`) は依然 tree-walk で実行
  (`Register*` opcode → `register_sub_decl` / `register_class_decl`)。クラスシステム・MRO・role 合成は
  未コンパイル。ただしこれは**宣言の登録**であって本体実行ではない。
  - **sub 登録の冪等化 (slice 1)**: `RegisterSub` は enclosing frame が走るたびに再実行される
    (hot routine 内の `my sub` 等) が、宣言が識別不変なら再実行は no-op であるべき。各宣言に
    コンパイル時 fingerprint (`CompiledCode.sub_fingerprints`) を持たせ、`register_sub_decl` が
    `SubRegisterOutcome::{Installed, Unchanged}` を返すことで、**識別不変な再登録を `FunctionDef`
    再導出なしに O(1) で検出し、resolution cache を乱さない**よう型で明示した。
  - **registry の `Arc<FunctionDef>` 化 (slice 2)**: `my sub` を宣言するルーチンへ入るたびに
    `snapshot_routine_registry` が `functions.clone()` で**プログラム中の全ルーチン body を deep-clone**
    していた (lexical scope を巻き戻すため)。registry が `HashMap<Symbol, Arc<FunctionDef>>` を保持する
    ことで、この snapshot は O(n) の refcount-bump になり、body の大きいルーチンを多数持つ現実的な
    プログラムで顕著に軽くなる (200-sub のマイクロベンチで ~28%)。registry は**不変・共有可能な定義**を
    保持する設計になった (in-place 変異サイトは皆無＝`make_mut` 不要)。dispatch 側の戻り値型は
    `FunctionDef` のまま (resolution 境界で `(**arc).clone()`)。
  - **dispatch resolution への Arc 貫通 (slice 3)**: ホットな単一結果リゾルバ
    (`resolve_function`/`resolve_function_with_types`/`_with_arity`/`_with_alias`/
    `choose_best_matching_candidate`) の戻り値を `Option<Arc<FunctionDef>>` 化。**呼び出し毎の
    resolution が body を deep-clone していたのを Arc bump に**。caller は Deref 経由で読み、所有 FunctionDef
    が要る数箇所のみ明示 clone。multi 候補列挙 (`resolve_all_*`) と proto/redispatch
    (`MultiDispatchEntry`) はレアパスなので境界で `FunctionDef` に変換し、core struct への波及を回避。
  - **derive-once キャッシュ (slice 4)**: `my sub` はルーチン return 時に registry から外れ
    (lexical scope の snapshot/restore)、次の呼び出しで再 install される。slice 1 の冪等パスは
    「registry に残っている」場合専用なので、この**再 install は AST→`FunctionDef` を毎回再導出**して
    いた (auto-sig scan・validation・body clone)。導出済み `Arc<FunctionDef>` を
    `(fingerprint, package)` キーでキャッシュ (`prepared_fn_defs`) し、単純な単一 sub の再 install は
    キャッシュした `Arc` を clone して streamlined install するだけにした (**真の derive-once**)。
    package をキーに含めるので同 body の別 package sub が混ざらない。
  - **残 (次スライス)**: `MultiDispatchEntry` の候補列 (`Vec<FunctionDef>`) を `Arc` 化すると
    redispatch (`nextsame`/`callsame`) の候補 clone も消える。ただし `resolve_all_*` 候補列挙の
    Arc 貫通 cascade を伴う (slice 3 で意図的に scope 外にした範囲・別スライス)。
- **メソッド dispatch の resolver オーバーヘッド**: multi/submethod や `samewith`/`nextsame` は
  `run_instance_method` (resolve + frame setup) を**入口として**通る (本体は compiled)。`MUTSU_VM_STATS` の
  `resolver-path method dispatches` カウンタはこの dispatch 入口数を測る (tree-walk 実行ではない)。
  次の最適化は VM-native resolution caching (multi は #3684 で着手・`CallMethod` op が resolver を
  経ずに直接 dispatch する拡張が残)。
- **delegation forwarder** (`handles`) は `forward_resolved_delegation` で native に転送 (run_block なし)。

つまり、§B の本質課題だった
「tree-walk **実行**を bytecode 化する」は達成済み。
残りは宣言登録の bytecode 化と dispatch overhead の削減であり、
どちらも性能改善や後片づけの話であって、ユーザコード本体の tree-walk 実行ではない。

### 1.2 二重変数ストア（`locals ↔ env`）— 単一権威ストア化で解消済み

rev3 で「最大の設計負債」としていた `locals ↔ env` の二重ストアは、
単一ストア化キャンペーンで解消済み。
対象だったのは `sync_locals_from_env` / `ensure_locals_synced` / `env_dirty` による手動同期系。

流れは次の通り:

- reverse pull（`sync_locals_from_env`）撤去 #3354
- cell-boxing 恒久 ON #3450
- `env_dirty` 系の物理削除 #3455（604 行減）

現在の整合性維持は **cell-boxing と precise writeback の 2 本だけ**。
`locals` が単一権威で、`env` は派生ビューという整理になっている。
設計メモは [docs/env-locals-coherence.md](docs/env-locals-coherence.md) と
[docs/vm-single-store.md](docs/vm-single-store.md) を参照。
残テーマは、§C の第一級コンテナ Phase 3 に吸収されている。

### 1.3 クロージャが env ベース (upvalue 不在)

クロージャは upvalue を持たず、自由変数は `GetGlobal`/`GetArrayVar`/`GetHashVar` として env HashMap を引く。
捕捉時に env をフラットコピーして `Value::Sub` に持たせる (`vm/vm_register_ops.rs`)。
`compute_needs_env_sync` (`opcode.rs`) は **ネストクロージャの自由変数になっているローカルだけ**を選別して
マークするよう精緻化されたが (`free_var_syms` 解析)、`ForLoop`/`BlockScope`/`MakeGather` を含むコードは
依然全ローカルを保守的にマークするフォールバックが残る。upvalue 機構そのものは未導入。
捕捉して書き換えられるローカルは `box_captured_lexicals` で `ContainerRef` セルに昇格し、
兄弟クロージャ間・スレッド間で共有される (Track C ライブセル化と同じ基盤)。

### 1.4 ローカルスロットのレキシカルスコープ不在

`alloc_local` (`compiler/mod.rs`) は変数 **名** をキーに単調増加のフラットな `local_map` を
1 つの `CompiledCode` に持つだけで、**スコープの push/pop が一切無い**。結果:

- 死んだネストブロックのスロットが解放されずフレームが肥大。
- **内側ブロックのシャドウイングがスロット衝突**する (`my $x` が外側 `$x` と同じスロットに解決)。
  正しいシャドウイングは env フォールバックに依存しており、§1.3 と悪循環。

### 1.5 最適化パスが事実上皆無 + opcode セットの肥大

定数畳み込み・DCE・peephole・レジスタ割り当ては無い。`1 + 2` は常に `LoadConst, LoadConst, Add` を出す。
唯一の "畳み込み" は `Nil.gist`→"Nil" と `start xx N` の unroll という超特化ケースのみ。

最適化の代わりに **opcode が ~297 variant** まで肥大している (`opcode.rs`)。多くは正当な演算子マッピング
(`NumEq`/`StrEq` など) だが、**LHS の構文形での特化**が ad-hoc 増殖の本体:
`ContainerEq{,Named,Indexed,Raw}`、`IndexAssign{ExprNamed,PseudoStashNamed,ExprNested,DeepNested,Generic}`。
これらはパーサ形状を opcode に焼き込んでおり、1 命令 + フラグオペランドに畳めるはず。
`is rw` の書き戻しも、引数のソース変数名を**文字列で定数プールに直列化**して VM が後から書き戻す方式
(`compiler/mod.rs`) で、IR が lvalue/参照情報を持たないことの裏返し。

### 1.6 パーサ (良好) と slang の擬似実装

手書きの scannerless 再帰下降 (nom 風 `PResult`)。**precedence 実装は教科書的で良好**
(`parser/expr/precedence.rs`、非結合/連鎖比較を X 例外で処理)。`memo.rs` は packrat 的バックトラック緩和。

真の slang スタックは無く、Regex/Quote/Pod それぞれが独立スキャナで擬似切替:
- Regex: パース時は `scan_to_delim` で生テキスト切り出し + 検証のみ、構造パースは実行時。
- Pod: パーサは読み飛ばすだけ、実構築は実行時に生ソース再スキャン (`runtime/io.rs`)。

ユーザ定義 grammar/token/rule の本格対応は将来課題 (CLAUDE.md の認識どおり)。

---

## 2. 正しさ・健全性の残課題

### 2.1 `unsafe` の "single-threaded 前提" コメント — **是正済み (コメント面)**

Arc 中身を生ポインタ経由で書き換える in-place mutation は **単一の監査済みチョークポイント
`value::aliased_mut::arc_contents_mut` に集約済み** (生 `Arc::as_ptr as *mut` キャストはこの 1 箇所のみ・
コードベース内に他のアドホックキャストはゼロ)。各呼び出しは `Arc::strong_count` ガード付きで、ユニーク
所有でなければ `Arc::make_mut` の COW パスに落ちる。陳腐化していた `// SAFETY: mutsu is single-threaded`
コメント群 (env `set_var`/`remove_var`・hash 内部変異・`descend_container_ref`/`env_root_descended_mut`・
`HyperSeq`/`RaceSeq` の "single-threaded implementation" doc) は**実際の不変条件を述べる正確な文面に是正済み**:
env 変異は「別スレッドからの並行 env アクセスが UB」、aliased mutation は「ライブな別 borrow が無いこと＋
クロスthread 共有は `aliased_mut.rs` 記載の既知ギャップ」。**残る本質的 unsoundness** は `aliased_mut.rs`
が明記するとおり、生ポインタ書き込みの provenance 違反＋クロスthread 共有時のデータ競合で、これは
**配列/ハッシュ要素を first-class `ContainerRef` セル化 (Track B) して interior mutability に置換する**ことで
初めて解消する (高ブラスト半径・別 PR)。

### 2.2 `RuntimeError` god-struct が制御フローをエラーチャネルで運ぶ — **bool→enum 分離は完了、残るは縮小・Box 化**

`RuntimeError` (`value/error.rs`) は `return`/`last`/`next`/`take`/`emit`/... を `Result::Err` で
実装しており、エラーと制御が同じチャネルを流れる。かつては
`is_return/is_last/.../is_emit` の**制御フロー bool が 18 個超**同居していたが、§7-4 キャンペーン
(#3701 slice 1 / #3706 slice 2 / slice 3) で**単一の `enum Control` (`value/error.rs`) へ統合済み**。
いまは `control: Option<Control>` 1 フィールドが排他的な制御シグナルを保持し、各 `is_*()` は
そこから派生する。残る独立 bool は `fail_handled` (Fail の修飾) と `is_leave`
(Last と同時成立しうるので enum に畳めない) の 2 つだけで、これらは意図的に分離している。

**残課題**: `RuntimeError` 本体はまだ ~270 バイトと巨大で、`#[allow(clippy::result_large_err)]` が
**23 箇所**に残る。縮小には (a) cold な routing フィールド
(`leave_*`/`return_target_callable_id`/`container_name`/`backtrace`/`hint`/parse 用 `code`/`line`/`column`)
を `Box` した補助構造体へ退避するか、(b) `Result<T, Box<RuntimeError>>` 化する必要がある。
いずれも構築サイト ~1700・`Result` シグネチャ ~1280 に波及する高 churn 作業で、別 PR 群として実施する。

---

## 3. 重複実装

### 3.1 制御構文の文 / 式 二重コンパイル

各制御構文に「文形」と「式形」の別コンパイラがある:
`compile_do_if_expr` / `compile_do_for_expr` / `compile_do_while_expr` / `compile_do_loop_expr` /
`compile_lazy_for_expr` (`compiler/helpers_do_expr.rs`) が `stmt.rs` / `helpers_control_flow.rs` の
ロジックを微妙な差異つきで重複。`compile_if_value` vs `Stmt::If` など。
**改善**: 値を返す単一パスに統一。

### 3.2 sub/method 本体の二重実装 — **本体実行は単一化済み、宣言登録のみ二重**

メソッド/サブの**本体実行**は §B (#3680) で bytecode 単一化された (tree-walk 実行は撤去)。残るのは
**宣言登録**の二重性: `SubDecl` は `RegisterSub` (インタプリタ登録) **と** `compile_sub_body`
(`compiler/stmt.rs`) を両方やる。登録側 (`register_*_decl`) はまだ tree-walk で、§1.1 の宣言登録が
依存して load-bearing。本体は登録後にコンパイル済み bytecode で走る。

### 3.3 メソッドディスパッチの多入口・名前マッチ散在

入口が乱立: `call_method_with_values` (`runtime/methods.rs`)、`dispatch_method_by_name_{1,2,3}` (arity 別)、
`run_instance_method` (`class.rs`)、`native_method_{0,1,2}arg` (`builtins/`)、加えて `dispatch_*` 関数が多数。
同一メソッド名の文字列マッチが分散 (`"elems"`/`"Str"`/`"join"`/`"reverse"` が複数ファイル)。
**改善**: 型 × メソッドの単一ディスパッチテーブルに集約。

---

## 4. ハードコード / ドリフトリスク

露骨な「テスト専用ハードコード出力」は見つからなかった。以下は導出を怠った直書きでドリフトリスクがある。

1. **`.^methods`/`.can` の型別メソッド一覧が直書き**:
   `collect_builtin_type_methods` (`runtime/methods_classhow.rs`) が `"Str" => &["chars","codes",...]` 等を
   実ディスパッチ表 (`builtins/methods_*`) から **導出せず直書き**。継承表も `"Bool" => &["Bool","Int","Cool"]`
   と直書き。自認コメントあり ("We model a subset that matches what the roast tests check")。
   メソッド追加で必ずドリフトする。**改善**: 実ディスパッチ表からメソッド集合を導出。

2. **パーサの roast 向け文法緩和** (軽微): `parser/.../helpers.rs` (`is List` 受理)、
   `misc.rs` (Test::Assuming 周辺)、`stmt/mod.rs` (`throws-like` 特例構文)。

---

## 5. 値モデル・性能・robustness

- **状態を値の外に置く設計**: Failure の handled/pending と pending DESTROY (`value/mod.rs`) が
  `thread_local!`。Value がスレッド境界を越えると登録が失われる。Seq の consumed/lazy 状態は
  `OnceLock<Mutex<Vec<Weak>>>` に Arc アドレスをキーに保持し、操作毎に O(n) 線形スキャン。脆く遅い。
- **Env がレキシカルチェーン不在**: `Env` (`env.rs`) はフラットな COW `HashMap<Symbol,Value>`。
  スコープは `let_saves`/`caller_env_stack` で擬似。`Env::get(&str)` は毎回 `Symbol::intern` し
  **グローバル `RwLock<SymbolTable>`** を引く → 変数読み取り毎にグローバルロック (多スレッド時は競合点)。
- **`.clone()` 約 7700 箇所**: Value 所有渡し設計の必然コストだが、`Array`/`Hash` 等は深いコピーになりうる。
  代入/ディスパッチ毎に大配列 O(n) コピーの懸念。
- **アロケーション失敗 abort** 【ユーザ指定サイズの確保はガード済み】: catchable な Rust panic
  (overflow / capacity-overflow / index-OOB) はメイン (#3045) と worker (`start{}`/`Promise` は
  `guard_worker_panic`、`hyper`/`race` は #3214) の両方で `X::AdHoc` へ変換され、もうプロセスを
  落とさない。**ユーザ指定サイズの確保 — 配列 autoviv の巨大 index / 文字列リピート (`"x" x 1e15`) /
  shaped 配列宣言 (`my @a[1e15]`) / `Buf.allocate`・`reallocate` (BIG) — は全て `try_reserve` による
  fallible 確保でガード済み** (確保を試みる前に catchable な `X::AdHoc` を投げる・共有 helper
  `Interpreter::autoviv_resize` ＋ `make_shaped_array`/`buf_allocate` の upfront probe)。
  実機: `my @a; try { @a[9999999999999]=1 }` も `try { my @a[1e15] }` も core dump せず捕捉可能。
  raku は同条件で `MoarVM panic` で落ちるため、mutsu はより親切。残る理論上の abort は
  **正当に見える操作からの真の OOM** (どの言語でも起こりうる) のみで、ユーザが直接サイズを与える
  経路ではない。
  Supply の detached worker での panic は握り潰されるため、QUIT への伝播は別途要検討。
- **`unwrap`/`expect`/`panic!`/`unreachable!` 約 1476 箇所**: 大半は不変条件アサート/テスト内/ガード済みで許容。
  VM の `unreachable!` はコンパイラ生成定数の不変条件でユーザ入力では到達不能。
- **`#[allow(lint)]` 138 箇所**: `result_large_err` 23 (= `RuntimeError` 肥大の表れ、§2.2 と同根)、
  `dead_code`/`too_many_arguments`/`type_complexity` ほか。

---

## 6. リポジトリ衛生

- **テストが CWD に一時ファイルを書き散らす — 根本原因を修正済み**: ルート直下の `bom-test-*` が増殖していたが、
  真因は **`roast/S16-io/bom.t` の末尾ブロックの `LEAVE unlink $temp-file` が発火しないコンパイラバグ**だった
  (テスト側でなくインタプリタ側の欠陥)。プログラム/`do`/ブロックの**末尾位置のベアブロックをインライン化する
  最適化が ENTER/LEAVE/KEEP/UNDO phaser を黙って捨てていた** (`compile_block_inline`)。tail block が phaser を
  持つ場合は実 `BlockScope` を通すよう修正し、`bom-test-2-*` のリークは解消。pin=`t/tail-block-leave-phaser.t`。
  残: `t/bom-stripping.t` は自前で `LEAVE unlink` 済 (CWD だが掃除される)。別軸の未修正＝bare block での KEEP が
  raku では発火しないのに mutsu は発火する (tail/非-tail 共通・本修正と独立)。
- **500 行規約の大幅違反**: `src/` に 500 行超 **164 ファイル**、1000 行超 **82 ファイル**。
  最大は `vm/vm_var_assign_ops.rs` **7267**、`runtime/mod.rs` **6633**、`runtime/regex_parse.rs` **6193**、
  `runtime/methods_object.rs` 5135、`runtime/io.rs` 4494。規約 (500 行上限) は形骸化。

---

## 7. 推奨ロードマップ (優先度順)

初版〜rev3 ロードマップの上位 (遅延リスト・並行ライブセル・メイン/worker panic 境界・regex 統合・
`.Supply` クラッシュ・`Value` 縮小・**locals↔env 単一ストア化** #3455・**ユーザメソッド本体の tree-walk
撤去** §B #3680) はすべて完了済み。残る課題:

| # | 項目 | 区分 | 効果 |
|---|------|------|------|
| 1 | アロケーション abort ガードは **完了** (配列 autoviv・文字列リピート・shaped 宣言・`Buf.allocate`/`reallocate`・§5)。残: Supply worker panic の QUIT 伝播 | 堅牢性 (任意) | 低 |
| 2 | クロージャに upvalue を導入し env 経由捕捉を撤廃 (PLAN §G Lever 1) | 性能 | 中 |
| 3 | ローカルスロットにレキシカルスコープを導入 (シャドウ衝突解消) | 正しさ + 設計 | 中 |
| 4 | 制御フローを `RuntimeError` から `enum Control` へ分離は **完了** (§2.2・#3701/#3706 ほか)。残: `RuntimeError` 本体の縮小・Box 化で `result_large_err` 23 箇所を撤去 (高 churn・別 PR 群) | 設計 | 中 |
| 5 | `.^methods`/`.can` の型別リスト: 確認済みドリフトは是正 (Str/Int/List・§4.1)。完全な実ディスパッチ表からの導出は arity-dispatch 非列挙のため別軸 | ドリフト解消 | 中 |
| 6 | 陳腐化した `unsafe` SAFETY コメント是正は **完了** (§2.1)。残: 配列・ハッシュ要素の `ContainerRef` 化で生ポインタ unsoundness を撤廃 (高ブラスト・別 PR) | 健全性 (UB) | 中 |
| 7 | 宣言登録の bytecode 化: sub 登録の冪等化 (**slice 1**・`SubRegisterOutcome`+fingerprint) と registry Arc 化 (**slice 2**・snapshot 共有) + dispatch resolution への Arc 貫通 (**slice 3**・resolution 毎の body clone を Arc bump に) 着手済 (§1.1)。残: 導出済み `Arc<FunctionDef>` キャッシュ再利用 (真の derive-once) + `MultiDispatchEntry` の Arc 化。method dispatch の resolution caching (multi は #3684 着手) | 設計 + 性能 | 中 |
| 8 | 一時ファイルを `tmp/` へ (root の `bom-test-*`) / 巨大ファイル分割 (500 行規約) | 衛生 | 低〜中 |

### Interpreter 除去という長期目標について — **達成 (2026-06)**

CP-3 collapse で "別 struct を消す" 意味で達成 (bytecode VM が `Interpreter` 構造体へ完全統合) し、
さらに §B (#3664〜#3680) で「**ユーザメソッド本体の tree-walk 実行**」を撤去 — bytecode VM がユーザ
コード本体の唯一の実行エンジンになった。残る tree-walk は**宣言登録** (`register_*_decl`) のみで、
これは本体実行ではなく登録処理。dispatch の resolver 入口 (`run_instance_method`) も本体は compiled で、
残るのは resolution overhead の削減 (perf) のみ。

---

*この分析は静的読解 + 実機再現に基づく。*
*rev3 (2026-06-17): 解決済み項目を本文から削除し、残存する設計・健全性・衛生課題のみへ整理した。*
