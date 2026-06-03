# mutsu コードベース分析 (Architecture & Quality Review)

作成日: 2026-06-03
対象: `main` (cfff80fc 時点)
方法: 4 つの調査エージェントによるサブシステム精読 + 主張の実機再現確認。
すべての指摘は `file:line` の根拠付き。再現可能な不具合は実際に走らせて確認した。

---

## 0. サマリ

mutsu は Rust 製の minimal Raku 互換インタプリタで、規模は **約 27 万行 / 307 ファイル**。
roast の通過状況は良好で (`roast-whitelist.txt` = 1238 件、`too_difficult.txt` = 40 件)、
言語機能のカバレッジは広い。**「roast を通すためだけのハードコード出力」のような露骨な不正は
ほぼ無く**、プロジェクト規約 (「test-specific hack 禁止」) は概ね守られている。

一方で、**アーキテクチャ上の根本的なねじれ**がいくつか存在する。最重要は次の 3 つ:

1. **「バイトコード VM」は実態として tree-walking interpreter の薄いフロントエンド**であり、
   `Interpreter` は「除去予定のレガシー」ではなく VM が依存する実行基盤そのものである。
2. **遅延リストが eager 実装 + 上限キャップの偽装**であり、無限リスト操作が
   誤った有限値や **Rust パニック (プロセスクラッシュ)** を生む。
3. **並行モデルが状態スナップショットコピー方式**のため、`start`/Promise 間で変数が共有されず
   意味論が壊れている。加えて配列の in-place 書き換えに使う `unsafe` の「single-threaded 前提」が
   実スレッド生成と矛盾し、**未定義動作 (データ競合) の余地**がある。

以下、実機で確認した代表的なバグ 2 件。

```raku
# (A) 並行 state 共有が壊れている
my $counter = 0; await (^4).map: { start { $counter++ } }; say $counter;
#   mutsu => 1      raku => 4
```

```raku
# (B) 無限リスト操作が Rust パニックでクラッシュ
say (1..Inf).grep(* %% 3)[^4];
#   mutsu => thread panicked: capacity overflow / exit 101
#   raku  => (3 6 9 12)
```

(注: `(1..Inf).elems` は mutsu でも正しく `Cannot .elems a lazy list` を投げる。
遅延の取り扱いはメソッドごとにまちまちで、ガードされている経路と崩壊する経路が混在している。)

---

## 1. アーキテクチャ評価

### 1.1 VM と Interpreter の結合 — 「VM はバイトコードシムである」【最重要】

CLAUDE.md は「VM はすべてをネイティブに扱うべき」「Interpreter は除去予定のレガシー」と述べるが、
コードの実態は逆である。

- VM は `Interpreter` を **所有**し (`vm.rs:83`)、共有実行状態コンテナとして使う。
  `self.interpreter.*` 参照は VM 全体で **1300 箇所超**。内訳上位は
  `env()` / `env_mut()` (= 変数ストア本体, ~480)、`type_matches_value`、`var_type_constraint`、
  `current_package`、`restore_let_saves`、`readonly_vars_mut` …。
  パッケージ状態・型検査・readonly 追跡・`let`/`temp` 復元・multi 解決・state 変数は
  すべて Interpreter 側にある。
- **本物の tree-walking 実行フォールバックが残存**している (CLAUDE.md が禁じているもの):
  `call_method_with_values` (`vm/vm_call_method_compiled.rs:123,134,229,354,576`,
  `vm/vm_call_method_mut_ops.rs:1179`, `vm/vm_data_ops.rs:301,320,357` ほか)、
  `call_function` / `call_function_fallback` (`vm/vm_call_func_ops.rs:406,671,677,704`,
  `vm/vm_call_dispatch.rs:68`)、`run_instance_method`、`run_react_event_loop` など。
  `try_compiled_method_or_interpret` (`vm/vm_call_method_compiled.rs:112`) は名前からして
  「コンパイル済みが無ければインタプリタへ落とす」もので、メソッドディスパッチは構造的に
  Interpreter なしでは完了しない。
- **すべての宣言 (`class`/`role`/`enum`/`subset`/`token`/`sub`/`method`) は Interpreter が実行**する。
  AST を "stmt pool" に複製して保持し、`Register*` opcode が `interpreter.register_*_decl()` を
  呼ぶ (`compiler/stmt.rs:2143-2167`, `vm/vm_register_ops.rs:312,1085`)。
  クラスシステム・MRO・role 合成は完全に Interpreter 側。
- 関数本体は **初回呼び出し時にオンザフライでコンパイル**され (`vm/vm_call_dispatch.rs:74`)、
  最終フォールバックは `interpreter.call_function` (`:68`)。
- これら禁止フォールバックには **規約が要求する `// TODO: compile to bytecode` が付いていない**
  (VM ツリー全体で 0 件)。負債が「可視・追跡可能」になっていない。

**評価**: コンパイラ層自体はクリーンで、リークは VM 層に押し込まれている。
「Interpreter を除去」するには env HashMap・クラスレジストリ・型検査を VM 所有データに移す
大手術が必要で、現状はアスピレーションであって実態ではない。ドキュメントが実態と乖離している。

### 1.2 二重変数ストア (locals ↔ env) と dirty 追跡

ローカル変数が `self.locals: Vec<Value>` (スロット番号引き) と `interpreter.env()` (名前引き) の
**2 箇所に二重に存在**し、手動同期が必要。VM 構造体は `env_dirty` / `locals_dirty` /
per-slot `locals_dirty_slots: Vec<bool>` (`vm.rs:108-117`) を持ち、
`sync_locals_from_env` / `sync_env_from_locals` / `ensure_env_synced`
(`vm/vm_env_helpers.rs:286,318,350`) を **65 箇所**で呼ぶ。`VmCallFrame` は 8 個の
dirty/bind フィールドをスナップショットする (`vm.rs:67-78`)。

per-slot bitmap が必要になったのは、粗いフラグが未初期化ローカルを破壊していたため
(`vm.rs:113-117` のコメント)。これは「設計された register モデル」ではなく
「同じデータを 2 箇所に持つのを糊付けしている」症状で、単一権威ストアにすれば丸ごと消える。

### 1.3 クロージャが env ベース (upvalue 不在) → フレーム全体の env 同期

クロージャは upvalue を持たず、自由変数は `GetGlobal`/`GetArrayVar`/`GetHashVar` として
Interpreter の env HashMap を引く (`compiler/expr.rs:40-58`, `helpers_sub_body.rs:461`)。
このため `compute_needs_env_sync` (`opcode.rs:1125`) は
**クロージャが 1 つでも存在すると全ローカルを env 書き戻し対象に保守的にマーク**する
(`opcode.rs:1134-1136`)。register VM のはずが、毎回全ローカルを HashMap にミラーするため
register を持つ意味が削がれている (正しさの松葉杖 + 性能税)。

### 1.4 ローカルスロットのレキシカルスコープ不在

`alloc_local` (`compiler/mod.rs:175`) は変数 **名** をキーに単調増加のフラットな `local_map` を
1 つの `CompiledCode` に持つだけで、**スコープの push/pop が一切無い**。結果:

- 死んだネストブロックのスロットが解放されずフレームが肥大。
- **内側ブロックのシャドウイングがスロット衝突**する (`my $x` が外側 `$x` と同じスロットに解決)。
  正しいシャドウイングは env フォールバックに依存しており、1.3 と悪循環。

### 1.5 最適化パスが事実上皆無 + opcode セットの肥大

定数畳み込み・DCE・peephole・レジスタ割り当ては無い。`1 + 2` は常に
`LoadConst, LoadConst, Add` を出す。唯一の "畳み込み" は `Nil.gist`→"Nil" (`expr.rs:177`) と
`start xx N` の unroll (`expr_binary.rs:75`) という超特化ケースのみ。

最適化の代わりに **opcode が 297 variant** まで肥大している (`opcode.rs`)。多くは正当な
演算子マッピング (`NumEq`/`StrEq` など `==` vs `eq`) だが、**LHS の構文形での特化**が
ad-hoc 増殖の本体: `ContainerEq{,Named,Indexed,Raw}` (`opcode.rs:110-130`)、
`IndexAssign{ExprNamed,PseudoStashNamed,ExprNested,DeepNested,Generic}` (`opcode.rs:513-549`)。
これらはパーサ形状を opcode に焼き込んでおり、1 命令 + フラグオペランドに畳めるはず。
`is rw` の書き戻しも、引数のソース変数名を**文字列で定数プールに直列化**して VM が後から
書き戻す方式 (`compiler/mod.rs:212-288`) で、IR が lvalue/参照情報を持たないことの裏返し。

### 1.6 パーサ (良好) と slang の擬似実装

手書きの scannerless 再帰下降 (nom 風 `PResult`)。**precedence 実装は教科書的で良好**
(`parser/expr/precedence.rs`、非結合/連鎖比較を X 例外で処理)。`memo.rs` は packrat 的
バックトラック緩和でポインタ恒等性キーのキャッシュ (heredoc 等の合成文字列はスキップする
防御コードあり = キャッシュの脆さの証左)。

真の slang スタックは無く、Regex/Quote/Pod それぞれが独立スキャナで擬似切替:
- Regex: パース時は `scan_to_delim` で生テキスト切り出し + 検証のみ、構造パースは実行時。
- Pod: パーサは読み飛ばすだけ、実構築は実行時に生ソース再スキャン (`runtime/io.rs:2206`)。

CLAUDE.md の「slang 未対応・将来課題」は正確。ユーザ定義 grammar/token/rule の本格対応は将来課題。

---

## 2. 確認済みバグ・重大な正しさの問題

### 2.1 遅延リストが eager + 上限キャップで、メソッドが遅延を無視して collect 【重大】

- `coerce_to_array` (`runtime/utils.rs:719-755`) は無限 Range を `MAX_ARRAY_EXPAND = 100_000`
  要素に実体化して `ArrayKind::Lazy` の **タグだけ**付ける。
- `Seq(Arc<Vec<Value>>)` (`value/mod.rs:1002`) は完全実体化済み。真の遅延は `LazyList` (gather) のみ。
- `grep` (`runtime/.../grep.rs:306`) は `Value::Range(a,b)` を遅延無視で
  `(a..=b).map(...).collect()` する。`b = i64::MAX` で **`capacity overflow` パニック → プロセスクラッシュ**
  (実機確認済み: `(1..Inf).grep(* %% 3)[^4]`)。
- これが S17 (並行/Supply) や遅延系テストの hang/panic 多発の根本原因。
  **改善**: Seq/Range を本物の `Iterator` ベース pull モデルにし、`grep/map/elems/.list` を遅延対応に。

### 2.2 並行 state 共有の意味論バグ 【重大】

`clone_for_thread` (`runtime/mod.rs:4869`) はインタプリタ状態をスレッドへ deep-copy スナップショット。
共有は `shared_vars: Arc<RwLock<HashMap>>` (`value/mod.rs:1006`) 経由だが
`entry().or_insert_with(|| val.clone())` (`mod.rs:4897`) で **各スレッドが自分のコピーで初期化**する。
そのため `start` ブロック群がライブセルを共有せず、上記 (A) で `$counter` が 1 になる (実機確認済み)。
`Promise.start/.then/.in/.at`、`.hyper/.race` は本物の `thread::spawn` を使う (偽装ではない) が、
共有モデルが壊れているため結果が非決定 or 欠落する。

### 2.3 `unsafe` の "single-threaded 前提" が実スレッドと矛盾 (UB の余地) 【重大】

`Value::Array = Arc<Vec<Value>>` (`value/mod.rs:943`、`Value: Send + Sync` をアサート) を、
`strong_count > 1` でも `unsafe { &mut *(Arc::as_ptr(arr) as *mut _) }` で
**エイリアスしたまま非アトミック書き込み**する箇所が 11 件
(`vm/vm_var_assign_ops.rs:1671-1676,1975,2007,2311,2880,2987,3200`)。
コメントは "SAFETY: mutsu is single-threaded" だが、`clone_for_thread` + `thread::spawn` で
同じ Arc が別スレッドへ渡る経路があり前提は偽。「`&mut self` で排他」と「Arc 中身の排他」を
混同している (`:2311` のコメント)。実機でクラッシュ再現はしなかった (該当経路が稀) が、
定義上はデータ競合 = UB。**改善**: 配列/ハッシュも `ContainerRef` 相当の共有セルに統一し、
ポインタ改竄による in-place mutation を撤廃。少なくとも誤った SAFETY コメントを是正。

### 2.4 `RuntimeError` god-struct が制御フローをエラーチャネルで運ぶ

`RuntimeError` (`value/error.rs:35`) は本来のエラー情報に加え、
`is_return/is_last/is_next/is_redo/is_goto/is_proceed/is_succeed/is_fail/is_take/is_emit/...`
**18 個の制御フロー bool** を同居させる (`:42-59`)。`return`/`last`/`next`/`take`/`emit` を
`Result::Err` で実装しており、エラーと制御が型レベルで混線。構造体肥大のため
`#[allow(clippy::result_large_err)]` が 17〜18 ファイルに散在。
**改善**: `enum Control { Return, Last, Next, Take(Value), Emit(Value), ... }` を分離し
`RuntimeError` を純エラーに縮小・Box 化。

---

## 3. 重複実装

### 3.1 正規表現文法の二重パース (validator vs matcher) 【高】

- `src/regex_validate.rs` (1108 行): パース時の構文検証専用。文字単位スキャンで **AST を作らず**
  `X::Syntax::Regex::*` を投げるだけ。
- `src/runtime/regex_parse.rs` (4701 行) + `runtime/regex/`: 実行時に同じ Raku regex 文法を
  `RegexToken`/`RegexAtom` へ構造パースする本物のマッチャ。

両者が `\d`、`<[...]>`、`%` 分離量化子、`«»`、`~` 平衡などの規則を**独立にエンコード**し、
相互参照/"keep in sync" コメントが 0 件。一方だけ拡張すると validator が通すのに matcher が
落ちる (逆も) 組合せが生じる二重メンテ構造。
**改善**: validator を廃し、`parse_regex` を構造を返す単一パーサに統合、検証はそのドライラン呼び出しに一本化。

### 3.2 制御構文の文 / 式 二重コンパイル

各制御構文に「文形」と「式形」の別コンパイラがある:
`compile_do_if_expr` / `compile_do_for_expr` / `compile_do_while_expr` / `compile_do_loop_expr` /
`compile_lazy_for_expr` (`compiler/helpers_do_expr.rs:162-352`) が `stmt.rs` /
`helpers_control_flow.rs` のロジックを微妙な差異つきで重複。`compile_if_value` vs `Stmt::If` など。
**改善**: 値を返す単一パスに統一。

### 3.3 sub/method 本体の二重実装

`SubDecl` は意図的に **両方** やる: `RegisterSub` (インタプリタ登録) **と** `compile_sub_body`
(`compiler/stmt.rs:1905-1953`, コメント "delegate to interpreter AND compile body")。
インタプリタ版は「除去予定」だが 1.1 のフォールバックが依存していて load-bearing。

### 3.4 メソッドディスパッチの多入口・名前マッチ散在

入口が乱立: `call_method_with_values` (`runtime/methods.rs:310`)、
`dispatch_method_by_name_{1,2,3}` (arity 別)、`run_instance_method` (`class.rs:785`)、
`native_method_{0,1,2}arg` (`builtins/`)、加えて `dispatch_*` 関数が runtime に ~131 個。
同一メソッド名の文字列マッチが分散 (`"elems"` 38 箇所、`"Str" =>` 27 箇所、`"join"` 14 箇所、
`"reverse"` 8 ファイル)。「どこに何があるか分からない」状態。
**改善**: 型 × メソッドの単一ディスパッチテーブルに集約。

---

## 4. 手抜き / ハードコード / roast 都合の実装

露骨な「テスト専用ハードコード出力」は **見つからなかった** (規約は概ね遵守)。
ただし以下は roast 都合の混入・導出を怠った直書きで、ドリフト/誤作動のリスクがある。

1. **roast fudge ロジックの核心混入 + 全入力適用**:
   `preprocess_roast_directives` (`runtime/run.rs:218`) が `run()` (`:673`) で **roast 判定なしに
   全ソース**へ行単位で適用。`#?rakudo skip/todo` 等の fudge 相当処理を、ハードコードした
   テスト関数名リスト (`["is(", "is ", "ok ", "is-approx", ...]` `:271-307`) で実装。
   本来 fudge は外部前処理ツール。ユーザコードに同形のコメントがあれば誤作動しうる。
   **改善**: fudge を roast ハーネス側 / 明示フラグ配下に分離。

2. **`.^methods`/`.can` の型別メソッド一覧が直書き**:
   `collect_builtin_type_methods` (`runtime/methods_classhow.rs:1367`) が
   `"Str" => &["chars","codes",...]` 等を実ディスパッチ表 (`builtins/methods_*`) から
   **導出せず直書き**。継承表も `"Bool" => &["Bool","Int","Cool"]` (`:1051`) と直書き。
   自認コメントあり: "We model a subset that matches what the roast tests check" (`:2063`)、
   `methods_walk.rs:16` "enough for what roast tests exercise"。メソッド追加で必ずドリフトする。
   **改善**: 実ディスパッチ表からメソッド集合を導出。

3. **パーサの roast 向け文法緩和** (軽微): `parser/.../helpers.rs:30` (`is List` 受理)、
   `misc.rs:537` (Test::Assuming 周辺)、`stmt/mod.rs:693` (`throws-like` 特例構文)。

（補足: `vm.rs:1162` や `compiler/expr_helpers.rs:94` の "special case" 群、`$?TABSTOP=8` などは
正当な仕様分岐・仕様定数であり手抜きではない。）

---

## 5. 値モデル・性能・robustness

- **Value enum の肥大 variant**: `BigRat(BigInt, BigInt)` (`value/mod.rs:947`)、
  `RegexWithAdverbs` (~13 フィールド, `:974`)、`Capture { Vec, HashMap }` (`:1023`)、`Uni` (`:1030`)
  が非 Box でインライン。全 Value が最大 variant のサイズになるため、`Vec<Value>`/`clone` のコスト増。
  **改善**: これらを Box 化。
- **状態を値の外に置く設計**: Failure の handled/pending (`value/mod.rs:553,587`) と
  pending DESTROY (`:435`) が `thread_local!`。Value がスレッド境界を越えると登録が失われる。
  Seq の consumed/lazy 状態は 7 個の `OnceLock<Mutex<Vec<Weak>>>` (`:17-43`) に Arc アドレスをキーに
  保持し、操作毎に O(n) 線形スキャン (`seq_consume` `:178`)。脆く遅い。
- **Env がレキシカルチェーン不在**: `Env` (`env.rs:15`) はフラットな COW `HashMap<Symbol,Value>`。
  スコープは `let_saves`/`caller_env_stack` で擬似。`Env::get(&str)` (`env.rs:33`) は毎回
  `Symbol::intern` し **グローバル `RwLock<SymbolTable>`** (`symbol.rs:47`) を引く →
  変数読み取り毎にグローバルロック。多スレッド時は競合点。
- **正規表現のコンパイル済みキャッシュ不在**: `Value::Regex(Arc<String>)` (`value/mod.rs:973`) は
  生文字列のみ。`parse_regex` がマッチ毎・サブパターン毎に再トークン化。ループ内 regex で
  構造パースが毎反復走る。**改善**: パターン→構造のキャッシュ。
- **`.clone()` 7293 箇所**: Value 所有渡し設計の必然コストだが、`Array(Vec<Value>)` 等は深いコピーに
  なりうる。代入/ディスパッチ毎に大配列 O(n) コピーの懸念。
- **`unwrap`/`expect`/`panic!`/`unreachable!` 計 1353 箇所**: 大半は不変条件アサート/テスト内/
  ガード済みで許容範囲。VM の `unreachable!` は「コンパイラ生成定数の不変条件」(`vm.rs:1154`,
  `vm/vm_control_ops.rs:440`) でユーザ入力では到達不能。ただし **2.1 のように
  ユーザコードからメインスレッド panic に到達できる経路がある**のは要対処。
  **改善**: `run()` (`vm.rs:386`) に panic→`X::` 変換境界を 1 つ設ける。
- **`#[allow(lint)]` 126 箇所**: `dead_code` 64 (未配線コード放置の示唆)、
  `result_large_err` 17 (= `RuntimeError` 肥大の表れ、4 章 2.4 と同根)、`too_many_arguments` 35。

---

## 6. リポジトリ衛生

- **テストが CWD に一時ファイルを書き散らす**: ルート直下に `bom-test-*` (12)、
  `tempfile_filehandles_io.*` (8)、`temp-file-RT-126006-test` が散乱。原因は
  `roast/S16-io/bom.t` / `roast/S16-filehandles/io.t` / `t/bom-stripping.t` が
  CWD に PID 付き一時ファイルを書いて削除していないこと (前日まで増殖継続)。
  `.gitignore` 済みでコミットはされないがルートを汚し続ける。CLAUDE.md は「一時ファイルは `tmp/`」と
  規定しており違反。**改善**: テストを `tmp/` 配下に書かせるか LEAVE phaser で確実に削除。
- 空ディレクトリ `t spec S22-package-format/`、`perf.data` (110MB, root 所有) も放置。
- **500 行規約の大幅違反**: `src/` に 500 行超 157 ファイル、1000 行超 76 ファイル。
  最大は `runtime/mod.rs` 5820、`vm/vm_var_assign_ops.rs` 5778、`runtime/regex_parse.rs` 4701、
  `vm.rs` 3846、`compiler/stmt.rs` 2354。規約 (500 行上限) は形骸化している。

---

## 7. 推奨ロードマップ (優先度順)

| # | 項目 | 区分 | 効果 |
|---|------|------|------|
| 1 | 遅延リストを本物の pull/Iterator モデルに (Seq/Range/grep/map/elems) | 正しさ + S17 hang 解消 | 高 |
| 2 | 並行モデルを「ライブセル共有」に (clone_for_thread スナップショット廃止) | 正しさ | 高 |
| 3 | 配列/ハッシュを共有セル化し `Arc::as_ptr as *mut` の UB を撤廃 | 健全性 (UB) | 高 |
| 4 | `run()` に panic→`X::` 変換境界を設置 (ユーザコードのクラッシュ防止) | 堅牢性 | 中〜高 |
| 5 | 制御フローを `RuntimeError` から `enum Control` へ分離 | 設計 | 中 |
| 6 | 正規表現 validator/matcher を単一パーサに統合 | 重複解消 | 中 |
| 7 | `.^methods`/`.can` の型別リストを実ディスパッチ表から導出 | ドリフト解消 | 中 |
| 8 | locals↔env 二重ストアを単一権威ストアに統合 (dirty 機構撤廃) | 設計 + 性能 | 中 |
| 9 | クロージャに upvalue を導入し全フレーム env 同期を撤廃 | 性能 | 中 |
| 10 | roast fudge を核から分離 / 一時ファイルを `tmp/` へ / 巨大ファイル分割 | 衛生 | 低〜中 |

### Interpreter 除去という長期目標について

CLAUDE.md の「Interpreter は除去予定のレガシー」は**現状そのままでは達成不能**である。
VM は env・クラスレジストリ・型検査・signature binder を Interpreter から借りており、
これらを VM 所有データに移すまでフォールバックは消せない。
当面は (a) ドキュメントを実態に合わせ、(b) 残存フォールバックに規約どおり
`// TODO: compile to bytecode` を付けて負債を可視化することを推奨する。

---

*この分析は静的読解 + 実機再現に基づく。再現コマンドは 0 章および 2 章に記載。*
