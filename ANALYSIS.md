# mutsu コードベース分析

この文書は、mutsu のコードベースを読んだうえで、
「設計上どこまで整理できていて、何がまだ負債として残っているか」をまとめたもの。
バグ票の一覧ではなく、**アーキテクチャと健全性のレビュー**として読む想定。

初版: 2026-06-03 / rev2: 2026-06-15 / rev3: 2026-06-17 / rev4: 2026-06-27 (単一ストア化 #3455・ユーザメソッド本体 tree-walk 撤去 §B #3680) / **rev5: 2026-06-27 (クロージャ upvalue Phase 1 #3715・Track B 着手前設計メモを §1.3/§2.1 に反映)** / **rev6: 2026-06-27 (GC 方針確定 ADR-0001＝cycle collector on Arc・Track B は GC 統合=層3a・§2.1/§7-6 に反映)** / **rev7: 2026-06-28 (§7-8 巨大ファイル分割 sweep 完了・registry-Arc/単一ストア/bool→enum の完了詳細を news へ移動し §1.1/§6/§7 をスリム化)**
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
  - **sub 登録の冪等化 + registry-Arc キャンペーン完了** (slice 1-8・詳細は [news/2026-06.md](news/2026-06.md)):
    `my sub` 宣言ルーチンへ入るたびに `snapshot_routine_registry` が registry クローンで**全ルーチン body を
    deep-clone** していた問題を、registry の全定義マップ (`functions`/`proto_functions`/grammar `token_defs`/
    `our_scoped_functions`) を `Arc<FunctionDef>` 化し、コンパイル時 fingerprint
    (`CompiledCode.sub_fingerprints`) による再登録冪等化 (`SubRegisterOutcome::{Installed, Unchanged}`) と
    `(fingerprint, package)` キーの derive-once キャッシュ (`prepared_fn_defs`) を加えて解消。dispatch
    resolution と `MultiDispatchEntry` も `Arc` 貫通。**どの registry クローン (snapshot/`clone_for_thread`/
    EVAL/block-scope restore) でも `FunctionDef` body が deep-clone されなくなった。** 残る §1.1 課題は
    メソッド dispatch の resolution caching のみ (下記)。
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

### 1.3 クロージャの upvalue — **Phase 1 着手 (#3715)、残りは Track B 待ち**

クロージャの自由変数は元々 `GetGlobal`/`GetArrayVar`/`GetHashVar` として env HashMap を引き、
捕捉時に env をフラットコピーして `Value::Sub` に持たせていた (`vm/vm_register_ops.rs`)。

**Phase 1 (#3715): index ベース upvalue 読み取りを導入。** `OpCode::GetUpvalue { index, name_idx }`
を追加し、`CompiledCode::compute_upvalues` がクロージャ本体の read-only plain-scalar 自由変数の
`GetGlobal` 読みを `GetUpvalue(i)` に書き換える (`upvalue_syms`)。`capture_upvalues` は**既に共有
`ContainerRef` セルになっている捕捉だけ**を upvalue 配列に取り込み (それ以外は `None` → env を live
で読む `name_idx` フォールバック)。`captures_env_by_name` (ForLoop/BlockScope/MakeGather/EVAL) や
サブシグネチャ捕捉引数は除外。env を捕捉源として残すため**常に健全**。これで既セル化済み
(mutated-escaping、`box_captured_lexicals` + Track C で cross-thread 対応) のキャプチャ読みが
env HashMap を経由しなくなった。フレーム upvalue 配列は `Interpreter::upvalues` で locals と同様に
保存/復元し、control-handler 経路ではクリアして env フォールバックさせる。

**残り (Phase 2+): read-only 定数キャプチャの値化・同名修正・write 経路・env コピー撤去は Track B
(§2.1) 待ち。** 試みて分かった健全性の壁:
- **値スナップショットは unsound**: 「この捕捉は不変」を健全判定できない。mutsu の compile 時
  mutation 解析は role/class メソッドの書き込みや `cas`/`is rw` 等の rw-arg sink を見落とすため、
  read-only に*見える*変数が実は変更され、凍結すると **flaky 化** する (`S12-construction/roles-6e.t`)。
  → セル (参照捕捉) が唯一の健全策。
- **read-only キャプチャの一律セル化 (#2749) は別の壁**: 型オブジェクト/Mix をセルに隠すと
  ContainerRef 非 deref 経路が壊れ (型 dispatch・不変性・`.kv` rw)、`is raw`/`cas` の by-name 書き戻しも
  壊れ、そして **per-iteration × cross-thread (`await ^4 .map: -> $n { start { $n } }`) で
  デッドロック/ハング**する。escape/whitelist/type-skip/loop-local のゲートを積めば個別には回避できるが、
  それは**場当たり (CLAUDE.md の「リスク」定義)**。
- 結論: クロージャ upvalue の残り利得は、**配列/ハッシュ/スカラのセルが cross-thread でも健全に
  共有・再入安全になること = §2.1 Track B が前提**。それが入るまで Phase 1 が健全な到達点。

なお `compute_needs_env_sync` の保守的フォールバック (ForLoop/BlockScope/MakeGather で全ローカルを
マーク) と、`box_captured_lexicals` による captured-mutated-escaping ローカルの `ContainerRef` 昇格
(兄弟クロージャ間・スレッド間共有、Track C と同基盤) は従来どおり。

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

#### Track B 着手前の設計メモ (2026-06-27 調査)

Track B は規模・難度ともに大きく、着手前に次を踏まえること:

- **規模**: `Value::Array(Arc<ArrayData>)` / `Hash(Arc<HashData>)` のコア表現変更 +
  `arc_contents_mut` 呼び出し **79 箇所 / 10 ファイル** + 全 read 経路 + Send/Sync 再設計。
- **cross-thread 共有の実体**: `clone_for_thread` が env の値 (`Arc<ArrayData>` クローン) を
  `shared_vars: RwLock<HashMap>` に入れて共有する (`runtime/mod.rs`)。同じ Arc を別スレッドが
  生ポインタ書き込み → data race。健全化には共有時の同期 (lock) が要る。
- **設計の地雷①: 再入デッドロック**。`arc_contents_mut` の契約は「借用保持中に VM を再入するな」。
  配列を `RwLock`/`Mutex` で包むと、配列操作中に VM が再入 (要素アクセスが closure/method を呼ぶ等) した
  瞬間、同じロックの再取得でデッドロック。**最ホットな型 (配列) で起きる**ため、素朴な lock 化は不可。
  「読み出して借用を落としてから再入」を全 79 箇所で保証するか、再入可能な所有モデルが要る。
- **設計の地雷②: perf**。単一スレッドの大半はロック不要なのに毎操作ロックは hot path に直撃。
  「共有 (cross-thread) のときだけ同期」には cross-thread 検出が要る。
- 上記より Track B は**再入安全なロック戦略 (または所有権モデル) の設計判断を伴う研究レベルの作業**で、
  場当たりに lock を足すと CLAUDE.md の「リスク」(flaky/ハング/ad-hoc) に直結する。設計を詰めてから着手。
- これが §1.3 クロージャ upvalue Phase 2+ の前提でもある (read-only/per-iteration/cross-thread キャプチャの
  健全なセル化が解禁される)。
- **方針決定 (ADR-0001, 2026-06-27)**: Track B は単独で着手せず **GC (cycle collector on Arc) と統合**して
  実施する (ADR 層3a)。同じ `Arc<ArrayData>`/`Arc<HashData>` 群 (79 箇所) を Track B と GC が触るため、
  `Arc → Gc<T>` 置換と要素セル化を 1 キャンペーンにまとめる (別々だと 79 箇所を 2 回触る)。再入デッドロックと
  GC セーフポイントは同じ再入境界の問題として一緒に設計する。moving GC は Rust 所有+Arc と非互換ゆえ却下、
  スカラ系は型フィルタで GC 対象外＝hot path コスト 0。Phase A 完了後に着手。詳細は
  [docs/adr/0001-gc-strategy-and-phasing.md](docs/adr/0001-gc-strategy-and-phasing.md)。

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
- **500 行規約違反 — §7-8 sweep で最悪オフェンダーを一掃**（詳細は [news/2026-06.md](news/2026-06.md)）:
  500 行超のファイルを **cohesive サブモジュールへ純粋移設**（impl ブロック→兄弟ファイル / 自由関数→facade + サブディレクトリ）。
  最大級だった `vm/vm_var_assign_ops.rs`（7267）・`runtime/mod.rs`・`runtime/methods_object.rs`・`runtime/io.rs`・
  `runtime/methods.rs`・`runtime/regex_parse.rs` 等をすべて分割し、`vm.rs` module root は 4872→**286** 行に。
  **`1000 行超` ファイルが 82→~40 件台へ激減**。`500 行超` の総数は微減（giant な単一 `match`/dispatch メソッド
  —`exec_one`・`call_method_with_values`・`register_class_decl`・`native_method_1arg` 等—が関数境界で分割できず
  residual 単独ファイルとして残るため）だが、これらは**意図的な indivisible 例外**。残: `ast.rs`・`registration_sub.rs`
  等の未分割ファイルと、giant dispatch メソッドの構造的分割（logic refactor を伴うため別軸）。

---

## 7. 推奨ロードマップ (優先度順)

初版〜rev3 ロードマップの上位 (遅延リスト・並行ライブセル・メイン/worker panic 境界・regex 統合・
`.Supply` クラッシュ・`Value` 縮小・**locals↔env 単一ストア化** #3455・**ユーザメソッド本体の tree-walk
撤去** §B #3680) はすべて完了済み。残る課題:

| # | 項目 | 区分 | 効果 |
|---|------|------|------|
| 1 | アロケーション abort ガードは **完了** (配列 autoviv・文字列リピート・shaped 宣言・`Buf.allocate`/`reallocate`・§5)。残: Supply worker panic の QUIT 伝播 | 堅牢性 (任意) | 低 |
| 2 | クロージャ upvalue: **Phase 1 完了** (#3715・index ベース `GetUpvalue` で既セル化キャプチャの読みを env HashMap から外す・§1.3)。残 (read-only 値化/同名修正/write 経路/env コピー撤去) は **#6 Track B 待ち** (cross-thread セルの再入安全・健全化が前提) | 性能 + 正しさ | 中 |
| 3 | ローカルスロットにレキシカルスコープを導入 (シャドウ衝突解消) | 正しさ + 設計 | 中 |
| 4 | 制御フローを `RuntimeError` から `enum Control` へ分離は **完了** (§2.2・#3701/#3706 ほか)。残: `RuntimeError` 本体の縮小・Box 化で `result_large_err` 23 箇所を撤去 (高 churn・別 PR 群) | 設計 | 中 |
| 5 | `.^methods`/`.can` の型別リスト: 確認済みドリフトは是正 (Str/Int/List・§4.1)。完全な実ディスパッチ表からの導出は arity-dispatch 非列挙のため別軸 | ドリフト解消 | 中 |
| 6 | **Track B**: 陳腐化 `unsafe` コメント是正は完了。残: 配列・ハッシュ要素の `ContainerRef` 化で生ポインタ unsoundness を撤廃。**GC と統合＝ADR-0001 層3a (`Arc → Gc<T>` 一斉置換・cycle collector on Arc)。単独着手しない・Phase A 完了後。** 着手前設計メモは §2.1 (79 箇所・コア表現変更・再入デッドロック/perf の地雷・cross-thread 共有は `shared_vars`)。#2 upvalue Phase 2+ の前提 | 健全性 (UB) + 性能 | 中〜大 |
| 7 | 宣言登録の bytecode 化: 冪等化 (**slice 1**) + **registry-Arc キャンペーン完了** (slice 2 `functions` / slice 3 dispatch resolution / slice 4 derive-once キャッシュ / slice 5 `MultiDispatchEntry` / slice 6 `proto_functions` / slice 7 `token_defs` / slice 8 `our_scoped_functions`)。**registry のどのクローンでも `FunctionDef` body は deep-clone されない** (§1.1)。残: method dispatch の resolution caching (multi は #3684 着手・`fast_method_cache` 済・残るは multi arg-shape 依存解決) | 設計 + 性能 | 中 |
| 8 | 巨大ファイル分割 (500 行規約): **§7-8 sweep で最悪オフェンダー一掃済み**（1000 行超 82→~40 件台・最大ファイル群を全分割・詳細は news）。残: `ast.rs`・`registration_sub.rs` 等の未分割ファイル / giant dispatch メソッドの構造的分割（別軸）。一時ファイルを `tmp/` へ (root の `bom-test-*`) | 衛生 | 低〜中 |

### Interpreter 除去という長期目標について — **達成 (2026-06)**

CP-3 collapse で "別 struct を消す" 意味で達成 (bytecode VM が `Interpreter` 構造体へ完全統合) し、
さらに §B (#3664〜#3680) で「**ユーザメソッド本体の tree-walk 実行**」を撤去 — bytecode VM がユーザ
コード本体の唯一の実行エンジンになった。残る tree-walk は**宣言登録** (`register_*_decl`) のみで、
これは本体実行ではなく登録処理。dispatch の resolver 入口 (`run_instance_method`) も本体は compiled で、
残るのは resolution overhead の削減 (perf) のみ。

---

*この分析は静的読解 + 実機再現に基づく。*
*rev3 (2026-06-17): 解決済み項目を本文から削除し、残存する設計・健全性・衛生課題のみへ整理した。*
