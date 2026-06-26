# mutsu コードベース分析 (Architecture & Quality Review)

初版: 2026-06-03 / rev2: 2026-06-15 / rev3: 2026-06-17 / **rev4: 2026-06-27 (単一ストア化 #3455・ユーザメソッド本体 tree-walk 撤去 §B #3680 を反映)**
方法: 調査エージェントによるサブシステム精読 + 主張の実機再現確認。
すべての指摘は `file:line` の根拠付き。再現可能な不具合は実際に走らせて確認した。

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
>   残る **uncatchable** な abort はユーザ指定サイズの明示的確保失敗 (shaped 配列宣言・`Buf.allocate` 等)。
>   配列 autoviv の巨大 index と文字列リピートは `try_reserve` でガード済み (§5、別課題は §7-1)。

---

## 0. サマリ

mutsu は Rust 製の minimal Raku 互換インタプリタで、roast 通過は `roast-whitelist.txt` = **1285 件**
と広く、「roast を通すためだけのハードコード出力」のような露骨な不正はほぼ無い。

初版が指摘したアーキテクチャ上の根本的なねじれ (VM シム / 遅延崩壊 / 並行共有) に加え、rev3 で
「最大の設計負債」とした **locals↔env 二重ストアは単一権威ストア化で解消** (#3455)、**ユーザメソッド本体の
tree-walk 実行も撤去** (§B #3680・bytecode VM が唯一の実行エンジン) され、いずれも解消済み。
以下に残る課題は、いずれも **正しさのクラッシュではなく設計・健全性・衛生レベル**の負債である。
catchable な Rust panic はメイン/worker いずれのスレッドでも `X::` へ変換されプロセスを落とさない。
プロセスを abort させ得るのは、catch_unwind では捕捉不能な**アロケーション失敗 abort** のみ
(配列 autoviv の巨大 index と文字列リピートは `try_reserve` でガード済み・残は shaped 配列宣言や
`Buf.allocate` 等の明示的確保。main/worker 問わず発生し、raku も同条件で `MoarVM panic`、§5)。

---

## 1. アーキテクチャ評価

### 1.1 tree-walk 実行の残存 — **ユーザコード本体は撤去済み (2026-06、§B #3664〜#3680)**

CP-3 collapse で「VM↔Interpreter の二構造体境界」は消え、さらに **§B キャンペーンで「ユーザメソッド
本体の tree-walk 実行」は完全に撤去された** (`run_instance_method_resolved` の非-delegation arm =
`run_block(method_def.body)`・~470 行を削除)。**全てのユーザメソッド/サブ本体は bytecode VM で実行される
唯一の実行エンジンになった**。残る tree-walk は「ユーザコードの実行」ではなく、以下の周辺のみ:

- **宣言登録** (`class`/`role`/`enum`/`sub`/`method` の `register_*_decl`) は依然 tree-walk で実行
  (`Register*` opcode → `register_sub_decl` / `register_class_decl`)。クラスシステム・MRO・role 合成は
  未コンパイル。ただしこれは**宣言の登録**であって本体実行ではない。
- **メソッド dispatch の resolver オーバーヘッド**: multi/submethod や `samewith`/`nextsame` は
  `run_instance_method` (resolve + frame setup) を**入口として**通る (本体は compiled)。`MUTSU_VM_STATS` の
  `resolver-path method dispatches` カウンタはこの dispatch 入口数を測る (tree-walk 実行ではない)。
  次の最適化は VM-native resolution caching (multi は #3684 で着手・`CallMethod` op が resolver を
  経ずに直接 dispatch する拡張が残)。
- **delegation forwarder** (`handles`) は `forward_resolved_delegation` で native に転送 (run_block なし)。

∴ §B の本質課題「tree-walk **実行**を bytecode 化する」は達成。残は宣言登録の bytecode 化と dispatch
overhead の削減 (いずれも perf/cleanup であって、ユーザコードの tree-walk ではない)。

### 1.2 二重変数ストア (locals ↔ env) — **単一権威ストア化で解消済み (2026-06-23・#3455)**

旧 rev3 で「最大の設計負債」とした locals↔env の二重ストア (`sync_locals_from_env` /
`ensure_locals_synced` / `env_dirty` による手動同期) は **単一ストア化キャンペーンで解消**された:
reverse pull (`sync_locals_from_env`) 撤去 (#3354) → cell-boxing 恒久 ON (#3450) →
`env_dirty` / `saved_env_dirty` / `reconcile_locals_from_env_at_site` / `blanket_reconcile_if_dirty` /
`ensure_locals_synced` / `cell_boxing_active()` の**物理削除** (#3455・604 行減)。

現在のコヒーレンス機構は **cell-boxing ＋ precise writeback の 2 つのみ**で、`locals` が**単一権威**・
`env` は派生ビュー。設計＝[docs/env-locals-coherence.md](docs/env-locals-coherence.md) /
[docs/vm-single-store.md](docs/vm-single-store.md)、詳細＝news/2026-06.md ＋ memory
`project_env_dirty_physical_removal`。残テーマは §C 第一級コンテナ Phase 3 (instance 属性セル・大半完了) に統合。

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

### 2.1 `unsafe` の "single-threaded 前提" コメントが陳腐化

`unsafe { &mut *(Arc::as_ptr(arr) as *mut ArrayData/HashData) }` で Arc 中身を生ポインタ経由で
書き換える in-place mutation が **5 件**残る (`vm/vm_var_assign_ops.rs`)。各サイトには
`Arc::strong_count` ガードが付き、ユニーク所有でなければ `Arc::make_mut` の COW パスに落ちるため
**データ競合の実発生は回避**されている。だが SAFETY コメントは依然 "mutsu is single-threaded" のままで、
Track C で `thread::spawn` 先と同じ Arc を共有する現実と矛盾している。
**改善**: コメントを「strong_count ガード前提」に書き換え、最終的には配列/ハッシュ要素も
`ContainerRef` 相当に統一して生ポインタ改竄を撤廃する。

### 2.2 `RuntimeError` god-struct が制御フローをエラーチャネルで運ぶ

`RuntimeError` (`value/error.rs`) は本来のエラー情報に加え、
`is_return/is_last/is_next/is_redo/is_goto/is_proceed/is_succeed/is_fail/is_take/is_emit/...`
**制御フロー bool を 18 個超**同居させる。`return`/`last`/`next`/`take`/`emit` を `Result::Err` で実装しており、
エラーと制御が型レベルで混線。構造体肥大のため `#[allow(clippy::result_large_err)]` が **23 箇所**に散在。
**改善**: `enum Control { Return, Last, Next, Take(Value), Emit(Value), ... }` を分離し
`RuntimeError` を純エラーに縮小・Box 化。

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
- **アロケーション失敗 abort** 【残る唯一の uncatchable なプロセス abort 系】: catchable な Rust panic
  (overflow / capacity-overflow / index-OOB) はメイン (#3045) と worker (`start{}`/`Promise` は
  `guard_worker_panic`、`hyper`/`race` は #3214) の両方で `X::AdHoc` へ変換され、もうプロセスを
  落とさない。**配列 autoviv の巨大 index と文字列リピート (`"x" x 1e15`) は `try_reserve` による
  fallible 確保でガード済み** (確保を試みる前に catchable な `X::AdHoc` を投げる・helper
  `Interpreter::autoviv_resize`)。実機: `my @a; try { @a[9999999999999]=1 }` は core dump せず捕捉可能。
  残る **catch_unwind で捕捉できない** アロケーション失敗 abort は、ユーザ指定サイズの明示的確保:
  shaped 配列宣言 (`my @a[1e15]` = `make_shaped_array` の `vec![Nil; dims[0]]`) / `Buf.allocate(BIG)` /
  その他の大量確保。これらは main/worker を問わず発生し、**raku も同条件で `MoarVM panic` で落ちる**ため
  挙動としては raku 相当。**改善 (任意・follow-up)**: 同じ `try_reserve` パターンを shaped 宣言・Buf 確保にも展開する
  (`make_shaped_array` を `Result` 化する波及が要るため別 PR)。
  Supply の detached worker での panic は握り潰されるため、QUIT への伝播は別途要検討。
- **`unwrap`/`expect`/`panic!`/`unreachable!` 約 1476 箇所**: 大半は不変条件アサート/テスト内/ガード済みで許容。
  VM の `unreachable!` はコンパイラ生成定数の不変条件でユーザ入力では到達不能。
- **`#[allow(lint)]` 138 箇所**: `result_large_err` 23 (= `RuntimeError` 肥大の表れ、§2.2 と同根)、
  `dead_code`/`too_many_arguments`/`type_complexity` ほか。

---

## 6. リポジトリ衛生

- **テストが CWD に一時ファイルを書き散らす**: ルート直下の `bom-test-*` が **248 件**まで増殖。
  原因は `roast/S16-io/bom.t` / `t/bom-stripping.t` が CWD に PID 付き一時ファイルを書いて削除しないこと。
  `.gitignore` 済みでコミットはされないがルートを汚し続ける。CLAUDE.md は「一時ファイルは `tmp/`」と規定。
  **改善**: テストを `tmp/` 配下に書かせるか LEAVE phaser で確実に削除。
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
| 1 | 残アロケーション abort のガード: shaped 配列宣言 (`my @a[1e15]`) / `Buf.allocate` を `try_reserve` 化 (配列 autoviv index ＋ 文字列リピートは **済**・§5) / Supply worker panic の QUIT 伝播 | 堅牢性 (任意) | 低〜中 |
| 2 | クロージャに upvalue を導入し env 経由捕捉を撤廃 (PLAN §G Lever 1) | 性能 | 中 |
| 3 | ローカルスロットにレキシカルスコープを導入 (シャドウ衝突解消) | 正しさ + 設計 | 中 |
| 4 | 制御フローを `RuntimeError` から `enum Control` へ分離 | 設計 | 中 |
| 5 | `.^methods`/`.can` の型別リストを実ディスパッチ表から導出 | ドリフト解消 | 中 |
| 6 | 陳腐化した `unsafe` SAFETY コメント是正 / 配列・ハッシュ要素の `ContainerRef` 化 | 健全性 (UB) | 中 |
| 7 | 宣言登録の bytecode 化 / method dispatch の resolution caching (multi は #3684 着手) | 設計 + 性能 | 中 |
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
