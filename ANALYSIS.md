# mutsu コードベース分析 (Architecture & Quality Review)

作成日: 2026-06-03 (初版) / **rev2: 2026-06-15 全面再監査**
対象: 初版 `main` (cfff80fc) / rev2 `main` (b7ced0fe)
方法: 4 つの調査エージェントによるサブシステム精読 + 主張の実機再現確認。
すべての指摘は `file:line` の根拠付き。再現可能な不具合は実際に走らせて確認した。
rev2 では初版の全指摘を現行コードに突き合わせ、`【✅修正済み】`/`【⚠️残存】` を更新し、
新たに §8.7 (`.Supply` 無限 Range クラッシュ) を実機特定して追加した。

> ## ✅ 進捗トラッキング (2026-06-15 rev2: 全面再監査 @ `b7ced0fe`)
> 初版 (2026-06-03 @ `cfff80fc`) の指摘を **4 つの調査エージェント + 実機再現** で再検証し直した。
> 各節の見出しに `【✅修正済み】` / `【⚠️残存】` を付記。初版から最大の構造変化は
> **§1.1 の「VM はバイトコードシム」批判が前提ごと変わった**こと: CP-3 collapse で
> bytecode VM が `Interpreter` 構造体へ**完全に溶け込んだ** (別 struct も `self.interpreter.*`
> 1300 箇所も消滅、残り 2 件はコメントのみ)。実行は今や単一 `Interpreter` 上のメソッド呼び出しで、
> 「VM↔Interpreter 境界越えフォールバック」という構図自体が無くなった (`loan_env!` も no-op 化)。
> サマリ:
> - **✅ 修正済み (実機再現で確認)**: §1.1 VM/Interpreter 分離 → **CP-3 collapse で単一 struct 化** /
>   §2.1・§8.1 遅延リスト崩壊・`grep` eager collect (`(1..Inf).grep(* %% 3)[^4]` → `(3 6 9 12)`) /
>   §8.2 無限 Range 即時展開クラッシュ (coerce/grep を `materialize_capped` 経由へ) /
>   §2.2・§8.3 並行 scalar / state 共有 (`my $c=0; await (^4).map:{start{$c++}}; say $c` → `4`、Track C
>   #2980/#2982/#3059/#3061/#3063) / §8.4 regex 毎マッチ再パース (`REGEX_PARSE_CACHE` + #3064/#3065) /
>   §3.1 regex validator/matcher 二重実装 → **`src/regex_validate.rs` を削除し単一パーサに統合** /
>   §5 panic→`X::` 変換境界 (#3045、メインスレッドのみ) / §4-1 roast fudge 誤作動 (`MUTSU_FUDGE` ゲート化) /
>   §5 `Value` enum 肥大 (`size_of::<Value>()` 72→48・guard で恒久ロック)。
> - **⚠️ 残存 / 新規**: **【新規・実機再現】§8.7 `.Supply` coercion が無限 Range を未ガード展開し
>   worker thread で `capacity overflow` abort** (§5 の worker-thread 未保護と複合、唯一の決定的残存クラッシュ) /
>   §1.2 locals↔env 二重ストア + sync 機構 (CP-3 で struct は 1 つになったが二重ストアは温存) /
>   §1.3 クロージャ env 捕捉 (upvalue 不在は不変、ただし `compute_needs_env_sync` は per-closure 精緻化済み) /
>   §1.4 ローカルスロットのレキシカルスコープ不在 / §2.3 unsafe `Arc::as_ptr as *mut` (11→4 箇所に減・
>   `strong_count` ガード追加だが SAFETY コメントは "single-threaded" のまま陳腐化) / §2.4 `RuntimeError`
>   god-struct (18 制御フロー bool 不変) / §4-2 `.^methods`/`.can` 直書きリスト / §6 衛生 (root の
>   `bom-test-*` ゴミが **254 件**に増殖)。

---

## 0. サマリ

mutsu は Rust 製の minimal Raku 互換インタプリタで、規模は **約 29.3 万行 / 331 ファイル** (rev2)。
roast の通過状況はさらに伸び (`roast-whitelist.txt` = **1270 件**、`too_difficult.txt` = 40 件)、
言語機能のカバレッジは広い。**「roast を通すためだけのハードコード出力」のような露骨な不正は
ほぼ無く**、プロジェクト規約 (「test-specific hack 禁止」) は概ね守られている。

初版が挙げた**アーキテクチャ上の根本的なねじれ 3 つは、その後の大改修でいずれも解消または
大幅前進した** (rev2 で実機再確認):

1. ~~「バイトコード VM」は tree-walking interpreter の薄いフロントエンド~~ → **【✅解消】
   CP-3 collapse で bytecode VM が `Interpreter` 構造体へ完全統合**。別 struct も
   `self.interpreter.*` 1300 箇所も消滅。「VM↔Interpreter 境界越えフォールバック」という
   構図自体が無くなった (ただし宣言実行や一部メソッドディスパッチは依然 tree-walk **ヘルパ** 経由 — §1.1)。
2. ~~遅延リストが eager + 上限キャップの偽装でクラッシュ~~ → **【✅大半解消】**
   `Seq`/`LazyList` に真の pull/coroutine モデルが入り、`grep`/`coerce_to_array` は
   `materialize_capped` 経由でキャップ。初版の代表クラッシュ (B) は解消 (§2.1/§8.1/§8.2)。
   **ただし `.Supply` coercion に未ガード展開が 1 経路残り、worker thread で abort する (§8.7 新規)**。
3. ~~並行モデルがスナップショットコピーで変数共有が壊れている~~ → **【✅解消】** Track C で
   捕捉スカラ・`state`・hash/array 要素がライブ共有セル化 (§2.2/§8.3)。
   in-place `unsafe` は 11→4 箇所に減り `strong_count` ガードが付いたが、SAFETY コメントの
   「single-threaded 前提」は陳腐化したまま (§2.3)。

以下、rev2 時点で実機確認したバグ。初版の (A)(B) は**もう再現しない** (修正済み):

```raku
# (A) 並行 state 共有 — 初版 mutsu=>1 / 現在 mutsu=>4 (✅修正済み, raku 一致)
my $counter = 0; await (^4).map: { start { $counter++ } }; say $counter;   # => 4

# (B) 無限リスト grep — 初版 panic / 現在 (3 6 9 12) (✅修正済み, raku 一致)
say (1..Inf).grep(* %% 3)[^4];                                             # => (3 6 9 12)
```

```raku
# (C) 【新規・唯一の決定的残存クラッシュ】無限 Range の .Supply 化が worker thread で abort
my $s = (1..Inf).Supply; $s.tap({ say $_; done if $_ >= 3 });
#   mutsu => thread '<unnamed>' panicked: capacity overflow （プロセス abort）
#   raku  => 1 2 3
# 原因: coercion.rs:536 が `Value::Range(a,b) => (a..=b).map(...).collect()` を i64::MAX 無ガードで実行。
# §8.2 で大半の展開サイトは塞いだが Supply 化経路が漏れており、しかも .tap が別スレッドで走るため
# §5 の panic→X:: 変換境界 (メインスレッドのみ) が捕捉できず exit が 0 になる二重の穴。
```

(注: `(1..Inf).elems` は正しく `Cannot .elems a lazy list` を投げる。遅延の取り扱いは
初版より格段に統一されたが、**未ガード展開サイトの "最後の数件" が残る** のが現状の姿である。)

---

## 1. アーキテクチャ評価

### 1.1 VM と Interpreter の関係 — 「CP-3 collapse で単一 struct に統合」【最重要・大幅更新】

> **【✅構造解消 (CP-3 collapse)】**: 初版の「VM は `Interpreter` を所有し `self.interpreter.*` を
> 1300 箇所超で引く薄いシム」という構図は**消滅した**。bytecode VM は `Interpreter` 構造体へ
> **完全に溶け込み** (`src/vm.rs:153-156` のコメント: "the bytecode VM has been fully dissolved
> into the `Interpreter` struct — the `Interpreter` *is* the bytecode VM")、`src/vm/` の `impl`
> ブロックは今や `impl Interpreter` である。`src/interpreter.rs` は
> `pub use crate::runtime::Interpreter;` の再エクスポート 1 行。`self.interpreter.*` の参照は
> 全 src で **2 件のみ (どちらもコメント)**。境界越え env 貸し出しの `loan_env!` マクロも
> no-op の自己呼び出しに退化 (`src/vm.rs:86-94`)。

統合後の構図は「単一 `Interpreter` struct のメソッド呼び出し」であり、初版が問題視した
**「VM↔Interpreter の二構造体境界」は無くなった**。ただし**実行戦略としての tree-walk ヘルパは温存**
されている (もはや別オブジェクトへのフォールバックではなく、同 struct 上のメソッド分岐):

- メソッド/関数ディスパッチのブリッジ `call_method_with_values` (`runtime/methods.rs:310`)、
  `call_function` (`runtime/builtins.rs:315`)、`call_function_fallback`、`run_instance_method`
  (`runtime/class.rs:725`) は健在で、bytecode 実行パスから呼ばれる。
- **宣言 (`class`/`role`/`enum`/`sub`/`method`) は依然 tree-walk の `register_*_decl` で実行**する。
  `Register*` opcode が AST を stmt pool から取り出して `register_sub_decl`
  (`runtime/registration_sub.rs:280`)・`register_class_decl` (`runtime/registration_class.rs:671`)
  などを呼ぶ (`vm/vm_register_ops.rs:468,1186,1254,1443`)。クラスシステム・MRO・role 合成は未コンパイル。
- 関数本体は依然**初回呼び出し時にオンザフライでコンパイル**される。
- 初版で「0 件」だった `// TODO: compile to bytecode` は **13 件**に増え、負債が可視化された
  (`vm/vm_call_method_compiled.rs:169,489,1524,1772`、`vm/vm_data_ops.rs:407,418,443`、
  `vm/vm_call_dispatch.rs:71` ほか)。多くは shaped-array / Phase 2 container-id / threaded shared-cell
  依存でブロックされている旨が明記。

**評価**: 「Interpreter を除去」という CLAUDE.md の目標は、**"別 struct を消す" 意味では達成された**
(VM struct も型エイリアスも無くなった)。残る課題は「tree-walk **実行**を bytecode 化する」こと —
宣言登録・一部メソッドフォークが未コンパイルで、これらは container-identity / shaped-array といった
下位機構に依存する。ドキュメントと実態の乖離 (初版の主因) はおおむね解消した。

### 1.2 二重変数ストア (locals ↔ env) と dirty 追跡 【⚠️残存】

> **⚠️ 残存 (CP-3 後も温存)**: CP-3 collapse で struct は 1 つになったが、
> 二重ストア自体は残っている。`Interpreter` 構造体が `env: Env` (`runtime/mod.rs:836`) と
> `locals: Vec<Value>` (`:1159`)、`env_dirty` (`:1178`)、`call_frames` (`:1177`) を
> 「merged VM execution registers」(`:1155-1216`) として同居させる。

ローカル変数が `self.locals: Vec<Value>` (スロット番号引き) と `self.env()` (名前引き) の
**2 箇所に二重に存在**し、手動同期が必要。`sync_locals_from_env` / `sync_env_from_locals` /
`ensure_locals_synced` (`vm/vm_env_helpers.rs` 周辺) を呼んで整合させ、per-slot 書き戻しは
`flush_local_to_env` (`vm/vm_env_helpers.rs:379-396`) が `code.needs_env_sync` フラグで間引く。
`VmCallFrame` は env/locals/dirty/bind を 1 フレームごとにスナップショットする (`vm.rs:143-151`)。

これは「設計された register モデル」ではなく「同じデータを 2 箇所に持つのを糊付けしている」症状で、
単一権威ストアにすれば丸ごと消える。CP-3 で `Interpreter` への統合は済んだので、次の論理ステップは
この二重ストアの一本化である。

### 1.3 クロージャが env ベース (upvalue 不在) → フレーム全体の env 同期

クロージャは upvalue を持たず、自由変数は `GetGlobal`/`GetArrayVar`/`GetHashVar` として
env HashMap を引く。捕捉時に env をフラットコピーして `Value::Sub` に持たせる
(`vm/vm_register_ops.rs:326-380`)。

> **⚠️ 一部改善**: 初版が問題視した「クロージャが 1 つでもあれば**全ローカル**を env 書き戻し対象に
> 保守的マーク」は緩和された。`compute_needs_env_sync` (`opcode.rs:1349-1429`) は
> いまや**ネストクロージャの自由変数になっているローカルだけ**を選別してマークする
> (`free_var_syms` 解析、`opcode.rs:1402-1428`)。ただし `ForLoop`/`BlockScope`/`MakeGather` を
> 含むコードは依然全ローカルを保守的にマークするフォールバックが残る (`:1385`)。
> upvalue 機構そのものは未導入なので、env 経由の捕捉という本質は変わっていない。

捕捉して書き換えられるローカルは `box_captured_lexicals` (`vm/vm_register_ops.rs:231-310`) で
`ContainerRef` セルに昇格し、兄弟クロージャ間・スレッド間で共有される (§2.2 のライブセル化と同じ基盤)。

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

### 2.1 遅延リストが eager + 上限キャップで、メソッドが遅延を無視して collect 【重大】【✅修正済み】

> **✅ 修正済み (2026-06)**: `grep`/`head`/`[]` 等の落ちる経路を遅延 pull に統一。
> `(1..Inf).grep(* %% 3)[^4]` は `(3 6 9 12)` を返す（panic しない）。§8.1/§8.2 も参照。

- `coerce_to_array` (`runtime/utils.rs:719-755`) は無限 Range を `MAX_ARRAY_EXPAND = 100_000`
  要素に実体化して `ArrayKind::Lazy` の **タグだけ**付ける。
- `Seq(Arc<Vec<Value>>)` (`value/mod.rs:1002`) は完全実体化済み。真の遅延は `LazyList` (gather) のみ。
- `grep` (`runtime/.../grep.rs:306`) は `Value::Range(a,b)` を遅延無視で
  `(a..=b).map(...).collect()` する。`b = i64::MAX` で **`capacity overflow` パニック → プロセスクラッシュ**
  (実機確認済み: `(1..Inf).grep(* %% 3)[^4]`)。
- これが S17 (並行/Supply) や遅延系テストの hang/panic 多発の根本原因。
  **改善**: Seq/Range を本物の `Iterator` ベース pull モデルにし、`grep/map/elems/.list` を遅延対応に。

### 2.2 並行 state 共有の意味論バグ 【重大】【✅修正済み】

> **✅ 修正済み (Track C)**: `start`/Promise 間で捕捉スカラ・`state` がライブ共有セル化された
> (#2980 scalar, #2982 `state $n`, #3059 compound-assign, #3061/#3063 hash/array 要素)。
> `my $counter=0; await (^4).map: { start { $counter++ } }; say $counter` → `4`。§8.3 も参照。

`clone_for_thread` (`runtime/mod.rs:4869`) はインタプリタ状態をスレッドへ deep-copy スナップショット。
共有は `shared_vars: Arc<RwLock<HashMap>>` (`value/mod.rs:1006`) 経由だが
`entry().or_insert_with(|| val.clone())` (`mod.rs:4897`) で **各スレッドが自分のコピーで初期化**する。
そのため `start` ブロック群がライブセルを共有せず、上記 (A) で `$counter` が 1 になる (実機確認済み)。
`Promise.start/.then/.in/.at`、`.hyper/.race` は本物の `thread::spawn` を使う (偽装ではない) が、
共有モデルが壊れているため結果が非決定 or 欠落する。

### 2.3 `unsafe` の "single-threaded 前提" が実スレッドと矛盾 (UB の余地) 【重大】【⚠️残存】

> **⚠️ 一部改善・残存**: in-place mutation 箇所は **11→4 件に減り**、`strong_count` ガードと
> `Arc::make_mut` フォールバックが付いて UB の実発生は塞がれた。だが SAFETY コメントは依然
> "mutsu is single-threaded" のままで陳腐化している (Track C でスレッドは実際に Arc を共有する)。
> 本格的な型レベルの是正は配列/ハッシュ**要素**の `ContainerRef` セル化（Phase 2 container-id）依存。

`Value::Array = Arc<ArrayData>` (`value/mod.rs:1533`、`Value: Send + Sync` をアサート `:3967`) に対し、
`unsafe { &mut *(Arc::as_ptr(arr) as *mut ArrayData/HashData) }` で **Arc 中身を生ポインタ経由で
書き換える** in-place mutation が **4 件**残る
(`vm/vm_var_assign_ops.rs:2101,2911,3530,4173`)。コメントは依然 "SAFETY: mutsu is single-threaded"
(`:2098,2559,2587,2908,3514` ほか) だが、Track C で `thread::spawn` 先と同じ Arc を共有するため前提は偽。
ただし初版時と違い、各サイトには `Arc::strong_count` ガードが付き
(`let use_inplace = Arc::strong_count(arr) > 1 && ...;` / `== 1`)、ユニーク所有でなければ
`Arc::make_mut` の COW パスに落ちるため、データ競合の**実発生は回避**されている。
さらに `docs/hashdata-migration-plan.md` 記載の **offset-0 UB** (型を `*mut HashMap` と誤キャストし
フィールド追加で SEGV) は `*mut HashData`/`*mut ArrayData` への是正で解消済み。
**残改善**: 陳腐化した SAFETY コメントを「strong_count ガード前提」に書き換え、最終的には
配列/ハッシュ要素も `ContainerRef` 相当に統一して生ポインタ改竄を撤廃する。

### 2.4 `RuntimeError` god-struct が制御フローをエラーチャネルで運ぶ 【⚠️残存】

`RuntimeError` (`value/error.rs:35`) は本来のエラー情報に加え、
`is_return/is_last/is_next/is_redo/is_goto/is_proceed/is_succeed/is_fail/is_take/is_emit/...`
**18 個の制御フロー bool** を同居させる (`:42-59`)。`return`/`last`/`next`/`take`/`emit` を
`Result::Err` で実装しており、エラーと制御が型レベルで混線。構造体肥大のため
`#[allow(clippy::result_large_err)]` が **11 箇所 / 5 ファイル**に散在
(`lib.rs`/`parse_dispatch.rs`/`parser/mod.rs`×3/`doc_mode.rs`×3/`value/mod.rs`×3)。
**改善**: `enum Control { Return, Last, Next, Take(Value), Emit(Value), ... }` を分離し
`RuntimeError` を純エラーに縮小・Box 化。

---

## 3. 重複実装

### 3.1 正規表現文法の二重パース (validator vs matcher) 【✅修正済み】

> **✅ 修正済み**: 初版が指摘した独立 2 パーサのうち **`src/regex_validate.rs` は削除された** (現存せず)。
> 構造パーサ `src/runtime/regex_parse.rs` (現 6171 行) の単一系統に統合され、二重メンテ構造は解消した。
> ホットマッチループは `REGEX_PARSE_CACHE` (`runtime/regex_parse.rs:23`) でパース結果を再利用する
> (§8.4 の perf 改善と同根)。検証も構造パーサのドライラン経由に一本化された。

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

1. **roast fudge ロジックの核心混入 + 全入力適用** 【✅修正済み (2026-06-15)】:
   `preprocess_roast_directives` が `run()`/parse/`require` で **roast 判定なしに全ソース**へ適用され、
   ユーザコードの `#?rakudo skip 'x'` が次の文を黙って落とす誤作動を実機確認した（`#?rakudo todo`/
   `#?DOES`/`#?v6` も同様）。
   **✅ 改善実施**: `MUTSU_FUDGE` 環境変数でゲート化（`Interpreter::maybe_preprocess_roast_directives`、
   `run.rs`）。デフォルト OFF（ユーザコードは素通し）、roast 実行時のみ ON
   （`scripts/run-roast-test.sh` と `roast-history.sh` が `export MUTSU_FUDGE=1`）。CLAUDE.md /
   `ai-run-roast.sh` の個別 roast 実行手順も `MUTSU_FUDGE=1` 付きに更新。core の fudge ロジック自体は
   roast 向けに調整済みで load-bearing のため温存。テスト `t/fudge-not-applied-to-user-code.t`。

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

- **Value enum の肥大 variant** 【✅大半修正済み (2026-06-15)】: `BigRat`/`RegexWithAdverbs`/
  `Capture`/`Uni`/`CustomType`/`CustomTypeInstance` が非 Box でインラインで、全 Value が最大 variant の
  サイズになり `Vec<Value>`/`clone` のコスト増だった。
  **✅ 改善実施**: これら 6 variant の payload を Box 化し **`size_of::<Value>()` を 72→48 (-33%)** に縮小
  （PR #3073 Capture+BigRat 72→64 / #3074 CustomTypeInstance 64→56 / #3076 Uni+RegexWithAdverbs+CustomType
  56→48）。`value_size_guard` 単体テストで <=48 を恒久ロック。残る 40B tier（`Enum`/`Proxy`）は
  **意図的に未着手**: `Enum` は `<=>`/`cmp`/`sort` の Order 結果（`Value::Enum`）で hot path のため、
  全体 Box 化は比較ごとに heap alloc を生み perf 回帰になる。perf 安全な代替（`EnumValue::Str`+`Proxy` のみ
  Box 化）は ~72 サイトで -8B と churn/benefit 比が悪く、48 で打ち切り（user 判断 2026-06-15）。
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
  **✅ 修正済み (#3045)**: VM の `run`/`run_inner` + try-body `run_range_guarded` の 2 境界で
  user-code Rust panic（overflow/capacity-overflow/OOB）を catchable な `X::AdHoc`(exit 1) に変換。
  残: `start{}` ワーカースレッドは未保護、stack-overflow/巨大 alloc abort は範囲外。
- **`#[allow(lint)]` 133 箇所** (微増): `dead_code` 66 (未配線コード放置の示唆)、
  `too_many_arguments` 38、`type_complexity` 12、`result_large_err` 11 (= `RuntimeError` 肥大の表れ、
  §2.4 と同根)。

---

## 6. リポジトリ衛生

- **テストが CWD に一時ファイルを書き散らす【⚠️悪化】**: ルート直下の `bom-test-*` が
  **254 件**まで増殖 (初版は 12)。原因は `roast/S16-io/bom.t` / `t/bom-stripping.t` が
  CWD に PID 付き一時ファイルを書いて削除しないこと。`.gitignore` 済みでコミットはされないが
  ルートを汚し続ける。CLAUDE.md は「一時ファイルは `tmp/`」と規定しており違反。
  **改善**: テストを `tmp/` 配下に書かせるか LEAVE phaser で確実に削除。(注: `perf.data` /
  `tempfile_filehandles_io.*` は現在は無い。空ディレクトリ `t spec S22-package-format/` は残存。)
- **500 行規約の大幅違反【⚠️悪化】**: `src/` に 500 行超 **164 ファイル**、1000 行超 **80 ファイル**。
  最大は `vm/vm_var_assign_ops.rs` **7188**、`runtime/mod.rs` **6556**、`runtime/regex_parse.rs` **6171**、
  `runtime/methods_object.rs` 5142、`runtime/io.rs` 4494、`vm.rs` 4354。規約 (500 行上限) は形骸化。
  二重ストア書き込みの集中点 `vm_var_assign_ops.rs` が最大肥大ファイルなのは §1.2 の負債と同根。

---

## 7. 推奨ロードマップ (優先度順)

初版ロードマップの上位項目 (遅延リスト・並行ライブセル・panic 境界・regex 統合) は**完了済み**。
rev2 で残る課題を再優先順位付けした。

| # | 項目 | 区分 | 効果 | 状態 |
|---|------|------|------|------|
| 1 | **`.Supply` 等の残り未ガード Range 展開を `materialize_capped` 経由へ** (§8.7) | 正しさ (残クラッシュ) | 中〜高 | ⚠️ 未 |
| 2 | **worker thread にも panic→`X::` 変換境界を設置** (`start{}`/`.tap` のクラッシュ防止) | 堅牢性 | 中〜高 | ⚠️ 未 |
| 3 | locals↔env 二重ストアを単一権威ストアに統合 (dirty 機構撤廃) | 設計 + 性能 | 中 | ⚠️ 未 |
| 4 | クロージャに upvalue を導入し env 経由捕捉を撤廃 | 性能 | 中 | ⚠️ 未 |
| 5 | ローカルスロットにレキシカルスコープを導入 (シャドウ衝突解消) | 正しさ + 設計 | 中 | ⚠️ 未 |
| 6 | 制御フローを `RuntimeError` から `enum Control` へ分離 | 設計 | 中 | ⚠️ 未 |
| 7 | `.^methods`/`.can` の型別リストを実ディスパッチ表から導出 | ドリフト解消 | 中 | ⚠️ 未 |
| 8 | 陳腐化した `unsafe` SAFETY コメント是正 / 配列・ハッシュ要素の `ContainerRef` 化 | 健全性 (UB) | 中 | ⚠️ 一部 |
| 9 | 宣言登録・残メソッドフォークの bytecode 化 (`// TODO: compile to bytecode` 13 件) | 設計 | 中 | ⚠️ 未 |
| 10 | 一時ファイルを `tmp/` へ (root の `bom-test-*` 254 件) / 巨大ファイル分割 (500 行規約) | 衛生 | 低〜中 | ⚠️ 悪化 |

#### 完了済み (初版ロードマップ)

- ✅ 遅延リストの pull/coroutine モデル化 (Seq/Range/grep) — 残るは §8.7 の Supply 1 経路のみ。
- ✅ 並行モデルのライブセル共有化 (Track C)。
- ✅ メインスレッドの panic→`X::` 変換境界 (#3045) — ただし worker thread は未保護 (上記 #2)。
- ✅ 正規表現 validator/matcher の単一パーサ統合 (`regex_validate.rs` 削除)。
- ✅ roast fudge の核分離 (`MUTSU_FUDGE` ゲート化)。
- ✅ `Value` enum 縮小 (72→48B)。

### Interpreter 除去という長期目標について 【大幅前進】

初版の「VM は Interpreter を借りており別 struct を消せない」という診断は、**CP-3 collapse で
"別 struct を消す" 意味では達成された** — bytecode VM は `Interpreter` 構造体へ完全統合され、
型エイリアスも `self.interpreter.*` 1300 箇所も無くなった。残る本質課題は「tree-walk **実行**を
bytecode 化する」こと: 宣言登録 (`register_*_decl`) と一部メソッドフォークが未コンパイルで、
これらは container-identity / shaped-array といった下位機構に依存する。
負債は `// TODO: compile to bytecode` 13 件で可視化されており、初版が指摘した
「ドキュメントと実態の乖離」はおおむね解消した。

---

## 8. 深掘り (Deep Dive) — 再現済み欠陥カタログと根本原因マッピング

初版の指摘を、追加の実機調査でさらに具体化した。すべて mutsu 実バイナリ vs `raku` の
実出力で確認済み。

### 8.1 「遅延崩壊」は一様ではなく、特定メソッドに隔離されている (重要な精緻化) 【✅修正済み】

> **✅ 修正済み**: 下記「崩壊するもの」3 例はいずれも現在は raku と一致する
> （`(1..Inf).grep(* %% 2)[^3]` → `(2 4 6)`、`.grep(*.is-prime)[^5]` → `(2 3 5 7 11)`）。

2.1 を切り分けた結果、無限 Range の遅延は**メソッドごとに当たり外れがある**。
正しく遅延処理されるもの:

```raku
(1..Inf).map(* * 2)[^3]      # mutsu => (2 4 6)   OK
(1..Inf).first(* %% 7)       # mutsu => 7         OK
(1..Inf)[5]                  # mutsu => 6         OK
(1..Inf).head(3)             # mutsu => (1 2 3)   OK
(1..Inf).kv.head(4)          # mutsu => (0 1 1 2) OK
(1..Inf).elems               # mutsu => "Cannot .elems a lazy list"  OK (正しく throw)
```

崩壊するもの (Rust パニック → プロセスクラッシュ):

```raku
(1..Inf).grep(* %% 2)[^3]    # mutsu => thread panicked: capacity overflow / exit 101
(1..Inf).grep(* %% 2).head(3)# mutsu => 同上クラッシュ          (raku は両方 (2 4 6))
(1..Inf).grep(*.is-prime)[^5]# mutsu => 同上クラッシュ          (raku は (2 3 5 7 11))
```

→ `map`/`first`/`head`/`[]`/`kv` は Range の遅延を尊重するが、**`grep` は Range を即時 collect** する。
原因は「遅延が未実装」ではなく「**eager 展開する経路と遅延を尊重する経路が混在し、ガードも不統一**」
であること。これは初版 2.1 より正確で、修正は「全面書き換え」ではなく「`grep` 等の落ちる経路を
遅延イテレータ経由に揃える」ことだと示唆する。

### 8.2 系統的欠陥: 無ガードの Range 即時展開が 43 箇所、ガードは 9 箇所のみ 【✅修正済み】

> **✅ 大半修正済み (PLAN.md §アーキ修正で消化)**: 無限 Range は遅延 pull モデル
> (`materialize_capped`) へ統一され、`grep`/`coerce_to_array`/`head`/`zip` 等の即時 collect 由来の
> `capacity overflow` panic は解消。`coercion.rs` の Range→list 展開も `i64::MAX` ガード付き。
> **ただし展開サイトの全数掃討は未完**: `.Supply` coercion (`coercion.rs:536-537`) に
> 未ガードの `(a..=b).map(Value::Int).collect()` が 1 経路残存し、§8.7 の決定的クラッシュを生む。

`(a..=b).map(Value::Int).collect()` という **無限 Range を `Vec<Value>` に即時展開するパターンが
src 全体で 43 箇所**存在する (`builtins/methods_0arg/coercion.rs:249,256,308,...`,
`runtime/utils.rs:630,1659,1668`, `runtime/methods_signature.rs:236` など)。一方
上限ガード `MAX_ARRAY_EXPAND` (`runtime/utils.rs:10`) の参照は **9 箇所だけ**。
つまり大多数の展開サイトは無ガードで、`b = i64::MAX` (無限 Range) が届いた瞬間に
`capacity overflow` で**メインスレッド/ワーカースレッドが panic しプロセスごと落ちる**。
これは単発バグではなく**欠陥クラス**であり、ユーザコードから容易に到達する Rust パニックは
5 章で述べた「panic→`X::` 変換境界の不在」を裏付ける。
**改善**: Range は `Value::Int` の Vec へ展開せず遅延イテレータ (pull) として扱う型を導入し、
43 箇所の即時 collect を排除する。少なくとも全展開サイトを単一ヘルパ経由にしてガードを一元化する。

### 8.3 並行: state 変数もスレッドへ反映されない (2.2 の追加確認) 【✅修正済み】

> **✅ 修正済み (Track C #2982)**: 下記の例は現在 `3`（raku 一致）。§2.2 参照。

```raku
sub f { state $n = 0; $n++ }
await (^3).map: { start f() }
say f();         # mutsu => 0     raku => 3
```

`start` ブロック内の `f()` が更新した `state $n` がスナップショットコピー
(`clone_for_thread`) のため親へ反映されず、最終的に 0。2.2 の `$counter` 問題と同根で、
レキシカル変数だけでなく state 変数でも共有が壊れている。

### 8.4 正規表現の毎マッチ再パースを実測 (5 章の数値裏付け)

`"hello world 123" ~~ /(\w+) \s+ (\w+) \s+ (\d+)/` を 20000 回ループ:

| | 実行時間 |
|---|---|
| mutsu (インラインリテラル) | **3.43 s** |
| mutsu (`my $rx = rx/.../;` で事前コンパイル) | **3.51 s** (改善せず) |
| raku | **0.40 s** |

→ (当時) mutsu は raku の約 **8.6 倍遅く**、しかも regex を変数に束ねても速くならなかった。
これは `Value::Regex(Arc<String>)` (生文字列) が**マッチのたびに `parse_regex` で
構造を再構築している**ことを定量的に裏付けていた (5 章)。
**改善 (実施済み)**: static パターン → コンパイル済み `RegexPattern` ツリーの memo キャッシュ
(`REGEX_PARSE_CACHE`) を導入済み。さらに #3064 でキャッシュヒットを `Arc<RegexPattern>` の
refcount bump 化し、毎マッチの deep tree clone を除去。現状 release ~0.56s (raku 0.36s ＝
~1.6x)。さらに #3065 で**単一マッチ経路 (`~~`/anchored) の早期終了**を追加: バックトラック DFS は
最初に見つかる完全マッチが最高優先 (greedy/leftmost) なので、`matches[0]` しか使わない caller は
そこで `break`。**catastrophic backtracking を抑制** — `"aaaa…(20)…b" ~~ /(\w+) (\w+) (\w+) b/`
×3000 が 22.8s→0.52s (~44x)。**残るギャップ**: 非曖昧パターンのトークン候補リスト構築＋量指定子
反復ごとの `RegexCaptures.clone()` (バックトラックスナップショット) アロケーション (別の深い最適化対象)。

### 8.5 根本原因 → roast ブロッカーの対応 (`TODO_roast/BLOCKERS.md`)

`BLOCKERS.md` は ~170 件を 19 機能に分類している。本分析の根本原因と対応づけると、
修正の投資対効果が見える:

| 本分析の根本原因 | 対応する BLOCKERS グループ (件数) |
|---|---|
| 8.1/8.2 遅延リスト崩壊・無限 Range 即時展開 | Threading/Async (31) の hang の一部, gather/take Laziness (2), Hyper/Meta (5) |
| 8.3/2.2 並行 state 共有・clone_for_thread | Threading/Concurrency/Async (31) の中核 |
| 2.4 制御フローを Err で運ぶ god-struct | Exception Types / throws-like (19) の一部 |
| 3.1 regex 二重実装 / 8.4 再パース | Regex/Match Advanced (12), Unicode/Collation (6) の一部 |
| 4-2 `.^methods` 直書きリストのドリフト | Traits/Metaprogramming (7), Multi Dispatch (5) の introspection 依存分 |

最大群の **Threading/Async (31 件)** は 8.2 (展開クラッシュ) と 8.3 (state 共有崩壊) の
両方が効いており、**遅延イテレータ化 + ライブセル共有の 2 つを直すことが
roast 残件の最大ボトルネックを崩す近道**である可能性が高い。

> **rev2 注記**: 本表が指す根本原因のうち 8.1/8.2/8.3 (遅延崩壊・展開クラッシュ・state 共有) と
> 8.4 (regex 再パース) は**いずれも修正済み**。`roast-whitelist.txt` は 1238→1270 件に増加した。
> 残る introspection ドリフト (§4-2) と god-struct (§2.4) は roast への影響が限定的で、優先度は中。

### 8.6 初版からの訂正

- `(1..Inf).elems` は mutsu でも正しく `Cannot .elems a lazy list` を throw する
  (初版調査の一部メモは誤り)。遅延の問題は「一様な崩壊」ではなく「**経路ごとの不統一**」
  (8.1) が正確な姿である。

### 8.7 【rev2 新規・唯一の決定的残存クラッシュ】無限 Range の `.Supply` 化が worker thread で abort

§8.2 で展開サイトの大半は塞がれたが、**掃討は全数に至っていない**。`.Supply` coercion の
`coercion.rs:536-537` だけ `i64::MAX` ガードが漏れている:

```rust
// src/builtins/methods_0arg/coercion.rs:531-539  (Supply coercion)
let values = match target {
    Value::Array(items, ..) => items.to_vec(),
    Value::Seq(items) | ... => items.to_vec(),
    Value::Range(a, b) => (*a..=*b).map(Value::Int).collect(),   // ← i64::MAX 無ガード
    Value::RangeExcl(a, b) => (*a..*b).map(Value::Int).collect(),
    _ => vec![target.clone()],
};
```

実機で決定的に再現 (2/2 回 abort):

```raku
my $s = (1..Inf).Supply; $s.tap({ say $_; done if $_ >= 3 });
#   mutsu => thread '<unnamed>' panicked: capacity overflow （プロセス abort、出力なし）
#   raku  => 1 2 3
```

これは**二重の穴**である:
1. **未ガード展開** (§8.2 の欠陥クラスの取りこぼし): 無限 Range を `Vec<Value>` へ即時 collect する。
2. **worker-thread 未保護** (§5 の panic 境界の穴): `.tap` のコールバックが別スレッドで走るため、
   §5 (#3045) で設置した panic→`X::` 変換境界 (メインスレッドのみ) が捕捉できず、`X::AdHoc` に
   変換されずプロセスごと abort する (しかも exit code は 0 になり成功に偽装される)。

**改善**: (1) `coercion.rs:536-537` を `materialize_capped` 経由に揃える。
(2) `start{}`/`.tap` のワーカースレッド本体にも panic→`X::` 変換境界を設け、worker の panic を
親 Promise/Supply の失敗として伝播させる (ロードマップ §7 の #1/#2)。

---

*この分析は静的読解 + 実機再現に基づく。再現コマンドは 0・2・8 章に記載。*
*rev2 (2026-06-15 @ `b7ced0fe`): 4 調査エージェント + 実機再現で初版を全面再検証。*
*初版の最重要 3 指摘 (VM シム / 遅延崩壊 / 並行共有) はいずれも解消し、*
*唯一の決定的残存クラッシュは §8.7 (`.Supply` の無限 Range 未ガード展開 × worker-thread 未保護)。*
