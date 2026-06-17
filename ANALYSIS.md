# mutsu コードベース分析 (Architecture & Quality Review)

初版: 2026-06-03 / rev2: 2026-06-15 / **rev3: 2026-06-17 (解決済み項目を削除し残存課題のみへ整理)**
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
>   ただし **worker thread (`start{}`/`.tap`) は依然未保護** (§5 に残課題として継続)。

---

## 0. サマリ

mutsu は Rust 製の minimal Raku 互換インタプリタで、規模は **約 29.7 万行 / 331 ファイル**。
roast 通過は `roast-whitelist.txt` = **1280 件** (`too_difficult.txt` = 40 件) と広く、
「roast を通すためだけのハードコード出力」のような露骨な不正はほぼ無い。

初版が指摘したアーキテクチャ上の根本的なねじれ (VM シム / 遅延崩壊 / 並行共有) は解消済み (上記参照)。
以下に残る課題は、いずれも **正しさのクラッシュではなく設計・健全性・衛生レベル**の負債である。
唯一プロセス abort を起こせる経路は **worker thread の panic 未保護** (§5、`start{}` 内の overflow 等)。

---

## 1. アーキテクチャ評価

### 1.1 tree-walk 実行の残存 (宣言登録・一部メソッドフォーク未コンパイル)

CP-3 collapse で「VM↔Interpreter の二構造体境界」は消えたが、**実行戦略としての tree-walk は温存**
されている (別オブジェクトへのフォールバックではなく、同 `Interpreter` struct 上のメソッド分岐):

- 宣言 (`class`/`role`/`enum`/`sub`/`method`) は依然 tree-walk の `register_*_decl` で実行する。
  `Register*` opcode が AST を stmt pool から取り出し `register_sub_decl`
  (`runtime/registration_sub.rs`)・`register_class_decl` (`runtime/registration_class.rs`) を呼ぶ。
  クラスシステム・MRO・role 合成は未コンパイル。
- 関数本体は初回呼び出し時にオンザフライでコンパイルされる。
- 負債は `// TODO: compile to bytecode` **12 件**で可視化 (`vm/vm_call_method_compiled.rs`、
  `vm/vm_data_ops.rs`、`vm/vm_call_dispatch.rs` ほか)。多くは shaped-array / container-id /
  threaded shared-cell 依存でブロックされている旨が明記。

残課題は「tree-walk **実行**を bytecode 化する」こと。下位機構 (container-identity / shaped-array) 依存。

### 1.2 二重変数ストア (locals ↔ env) と dirty 追跡 【最大の設計負債】

ローカル変数が `self.locals: Vec<Value>` (スロット番号引き) と `self.env()` (名前引き) の
**2 箇所に二重に存在**し、手動同期が必要。`Interpreter` 構造体が `env: Env`・`locals: Vec<Value>`・
`env_dirty`・`call_frames` を「merged VM execution registers」(`runtime/mod.rs`) として同居させる。
`sync_locals_from_env` / `sync_env_from_locals` / `ensure_locals_synced` を呼んで整合させ、
per-slot 書き戻しは `flush_local_to_env` (`vm/vm_env_helpers.rs`) が `code.needs_env_sync` で間引く。
`VmCallFrame` は env/locals/dirty/bind を 1 フレームごとにスナップショットする。

これは「設計された register モデル」ではなく「同じデータを 2 箇所に持つのを糊付けしている」症状で、
単一権威ストアにすれば丸ごと消える。CP-3 で `Interpreter` への統合は済んだので、次の論理ステップは
この二重ストアの一本化である。書き込み集中点 `vm_var_assign_ops.rs` が最大肥大ファイル (§6) なのも同根。

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

### 3.2 sub/method 本体の二重実装

`SubDecl` は意図的に **両方** やる: `RegisterSub` (インタプリタ登録) **と** `compile_sub_body`
(`compiler/stmt.rs`, コメント "delegate to interpreter AND compile body")。
インタプリタ版は「除去予定」だが §1.1 の tree-walk 宣言登録が依存していて load-bearing。

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
- **worker thread の panic 未保護** 【唯一プロセス abort を起こせる経路】: メインスレッドは
  panic→`X::` 変換境界 (#3045) で保護されたが、`start{}`/`.tap` のワーカースレッド本体は未保護。
  worker 内の overflow / capacity-overflow / 巨大 alloc は今も catchable にならずプロセスごと abort する
  (実機: `await (^2).map: { start { my @a; @a[9999999999999]=1 } }` → core dump, exit 134)。
  **改善**: ワーカースレッド本体にも panic 境界を設け、worker の panic を親 Promise/Supply の失敗として伝播。
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

初版ロードマップの上位 (遅延リスト・並行ライブセル・メイン panic 境界・regex 統合・`.Supply` クラッシュ・
`Value` 縮小) はすべて完了済み。残る課題:

| # | 項目 | 区分 | 効果 |
|---|------|------|------|
| 1 | worker thread (`start{}`/`.tap`) にも panic→`X::` 変換境界を設置 | 堅牢性 (唯一の abort 経路) | 中〜高 |
| 2 | locals↔env 二重ストアを単一権威ストアに統合 (dirty 機構撤廃) | 設計 + 性能 | 中 |
| 3 | クロージャに upvalue を導入し env 経由捕捉を撤廃 | 性能 | 中 |
| 4 | ローカルスロットにレキシカルスコープを導入 (シャドウ衝突解消) | 正しさ + 設計 | 中 |
| 5 | 制御フローを `RuntimeError` から `enum Control` へ分離 | 設計 | 中 |
| 6 | `.^methods`/`.can` の型別リストを実ディスパッチ表から導出 | ドリフト解消 | 中 |
| 7 | 陳腐化した `unsafe` SAFETY コメント是正 / 配列・ハッシュ要素の `ContainerRef` 化 | 健全性 (UB) | 中 |
| 8 | 宣言登録・残メソッドフォークの bytecode 化 (`// TODO: compile to bytecode` 12 件) | 設計 | 中 |
| 9 | 一時ファイルを `tmp/` へ (root の `bom-test-*` 248 件) / 巨大ファイル分割 (500 行規約) | 衛生 | 低〜中 |

### Interpreter 除去という長期目標について

CP-3 collapse で "別 struct を消す" 意味では達成済み (bytecode VM は `Interpreter` 構造体へ完全統合、
型エイリアスも `self.interpreter.*` 参照も消滅)。残る本質課題は「tree-walk **実行**を bytecode 化する」こと:
宣言登録 (`register_*_decl`) と一部メソッドフォークが未コンパイルで、これらは container-identity /
shaped-array といった下位機構に依存する。負債は `// TODO: compile to bytecode` 12 件で可視化されている。

---

*この分析は静的読解 + 実機再現に基づく。*
*rev3 (2026-06-17): 解決済み項目を本文から削除し、残存する設計・健全性・衛生課題のみへ整理した。*
