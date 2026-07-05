# mutsu コードベース分析

この文書は、mutsu のコードベースを読んだうえで、
「設計上どこまで整理できていて、何がまだ負債として残っているか」をまとめたもの。
バグ票の一覧ではなく、**アーキテクチャと健全性のレビュー**として読む想定。

初版: 2026-06-03 / rev2: 2026-06-15 / rev3: 2026-06-17 / rev4: 2026-06-27 (単一ストア化 #3455・ユーザメソッド本体 tree-walk 撤去 §B #3680) / rev5: 2026-06-27 (クロージャ upvalue Phase 1 #3715・Track B 着手前設計メモ) / rev6: 2026-06-27 (GC 方針確定 ADR-0001) / rev7: 2026-06-28 (§7-8 巨大ファイル分割 sweep 完了・完了詳細の news 移動) / **rev8: 2026-07-06 (VM 品質の全体再評価: GC 層3a 完了＝cycle collector default-on (ADR-0002/0003)・`arc_contents_mut` は dead 化し生ポインタ書き込みは `gc::gc_contents_mut` へ移行・Track B slice 1-3・`RuntimeError` 縮小で `result_large_err` 23→0・opcode 監査 #4279 (192→48B・per-opcode 統計)・ADR-0004 (JIT) Accepted を反映)**

方法:
- 調査エージェントによるサブシステム単位の精読
- 主張ごとの実機再現確認

すべての指摘には `file:line` の根拠を付けている。
再現可能な不具合は、実際に走らせて確認した。

> ## 解決済み (過去 rev の主要指摘 — 本文からは削除)
> 過去 rev の最重要指摘は実機確認でいずれも解消したため、本文から除去した。記録としてのみ列挙する:
> - **VM はバイトコードシム** → CP-3 collapse で bytecode VM が `Interpreter` 構造体へ完全統合。
> - **遅延リスト崩壊・無限 Range 即時展開クラッシュ** → pull モデルへ統一 (`materialize_capped` キャップ)。
> - **並行 scalar / state 共有崩壊** → Track C + Track B slice 2/3 でライブ共有セル化。
> - **regex 毎マッチ再パース / validator・matcher 二重実装** → `REGEX_PARSE_CACHE`・単一パーサへ統合。
> - **roast fudge の全入力適用誤作動** → `MUTSU_FUDGE` でゲート化。
> - **`Value` enum 肥大** → Box 化で 72→48B (`value_size_guard` でロック)。
> - **メイン/worker スレッドの panic→`X::` 変換境界不在** → `run`/`guard_worker_panic`/#3214 で変換。
>   ユーザ指定サイズの確保失敗は `try_reserve` でガード済み (§5)。
> - **`locals ↔ env` 二重ストア** → 単一権威ストア化 (#3455)。
> - **ユーザメソッド本体の tree-walk 実行** → §B (#3680) で撤去。
> - **GC 不在（サイクルリーク）** → cycle collector on Arc（ADR-0001 層3a）が **default-on**
>   (ADR-0003・2026-07-05)。§2.1 に現状評価。
> - **`RuntimeError` の `result_large_err` 23 箇所** → cold フィールドの Box 退避で <128B 化し
>   attribute 全撤去 (§2.2 に縮約)。
> - **`OpCode` 192B ストライド + dead opcode** → `ForLoop(Box<ForLoopSpec>)` 化で 48B・dead 3 variant
>   削除・per-opcode ヒストグラム導入 (#4279・§1.5)。

---

## 0. サマリ

mutsu は Rust 製の minimal Raku 互換インタプリタで、`roast-whitelist.txt` は **1364 / 1463 (93.2%)**
まで伸びている。テスト通過のためだけに出力を偽装しているような、露骨なハードコードは見当たらない。

rev8 時点の全体評価:

- **実行エンジンは単一 bytecode VM に一本化済み**で、主要な設計破綻（シム構造・二重ストア・
  tree-walk 本体実行）はすべて解消済み。残る tree-walk は宣言登録と dispatch resolver のみ (§1.1)。
- **GC が入った**。ADR-0001 層3a（cycle collector on Arc）が完了し default-on。サイクルリークという
  「欠陥品」条件は外れた。ただし旧 `arc_contents_mut` の生ポインタ unsoundness は**消えたのではなく
  `gc::gc_contents_mut` に引っ越した**（§2.1）— 完全解消は Track B 残スライス (T4-T6)。
- **品質ゲートが厚い**: CI は `test`（clippy -D warnings・cargo test・t/ TAP・roast 1364 件）＋
  **`gc-stress`**（GC on + VERIFY + stress knob で test/roast 全走・blocking）＋ `wasm-e2e` の 3 ジョブ。
  サイズガード（`Value` ≤48B・`OpCode` ≤48B）とドリフトテスト（`t/can-methods-drift.t`）で
  構造的後退も機械検出される。
- **計測基盤が揃った**: `MUTSU_VM_STATS` が dispatch fallback・dual-store・GC・**per-opcode
  実行ヒストグラム (#4279)** を出す。命令セットや GC の意思決定がデータ駆動でできる状態。
- **負のトレンド**（GC/Track B の churn 由来・要クリーンアップ）: `.clone()` 7700→**9022**、
  `unwrap/expect/panic!/unreachable!` 1476→**1643**、`#[allow(` 138→**157**。
  `runtime/mod.rs` も 1932→2118 行に再肥大 (§6)。
- perf（対 raku）: fib 1.0x・起動 0.04x は互角以上だが、**bench-fib 3.2x・method-call 2.7x・
  bench-class 2.3x** が残る。回収レバーは NaN-boxing（層3b）→ JIT（層4・ADR-0004 Accepted）に確定。

そのため、ここで挙げる残課題はどれも
**「基本設計が破綻している」という種類ではなく、設計・健全性・保守性の負債**に寄っている。

### 先に結論だけ知りたい場合

- §1: 主要なアーキテクチャ課題はどこまで片づいたか
- §2: いま残っている正しさ・健全性の論点（GC 導入後の姿）
- §4: ハードコードやドリフトの危険箇所
- §7: 優先度つきの実行順

---

## 1. アーキテクチャ評価

### 1.1 tree-walk 実行の残存 — ユーザコード本体は撤去済み

CP-3 collapse で VM/Interpreter 二重構造が、§B キャンペーンで**ユーザメソッド本体の tree-walk 実行**が
撤去され、**ユーザコード本体の実行エンジンは bytecode VM に一本化済み**（詳細は news）。
いま残る tree-walk は本体実行ではなく次の周辺処理だけ（2026-07-06 再確認済み）:

- **宣言登録** (`register_class_decl` `runtime/registration_class_decl.rs:159`・`register_sub_decl`
  `runtime/registration_sub.rs:406`・`register_role_decl` `runtime/registration_role.rs:210` ほか) は
  依然 tree-walk (`Register*` opcode 経由・`opcode.rs:1233-1241`)。クラスシステム・MRO・role 合成は
  未コンパイル。ただし本体実行ではなく登録処理。
- **メソッド dispatch の resolver オーバーヘッド**: multi/submethod や `samewith`/`nextsame` は
  `run_instance_method` (`runtime/class_dispatch.rs:10`) を**入口として**通る（本体は compiled）。
  残る最適化は VM-native resolution caching（multi は #3684 で着手）。
- **delegation forwarder** (`handles`) は `forward_resolved_delegation`
  (`runtime/class_dispatch.rs:366`) で native 転送（run_block なし）。
- **モジュール sub の OTF コンパイルゲート** (`def_is_otf_compilable_module_single`
  `vm/vm_call_func_ops.rs:1724`): `state`/`EVAL`/`EVALFILE`/`start`/`CATCH`/`CONTROL`/phaser/
  ネスト宣言/sigilless `\x`/戻り型 coercion が残ゲート。**`is rw`/`is raw` はゲートから外れた**
  （rw-arg 書き戻しがコンパイル時 caller slot を持つようになったため・#4091、同ファイル :1725-1729）。

→ 残りは宣言登録の bytecode 化と dispatch overhead 削減で、いずれも性能/後片づけ。本体の tree-walk 実行ではない。

### 1.2 二重変数ストア（`locals ↔ env`）— 解消済み

rev3 で「最大の設計負債」としていた `locals ↔ env` 二重ストアは単一権威ストア化で解消済み
（reverse pull 撤去 #3354 → cell-boxing 恒久 ON #3450 → `env_dirty` 物理削除 #3455）。いまは
`locals` が単一権威・`env` は派生ビューで、整合性維持は cell-boxing と precise writeback の 2 本だけ。
詳細は [docs/env-locals-coherence.md](docs/env-locals-coherence.md) /
[docs/vm-single-store.md](docs/vm-single-store.md)。

### 1.3 クロージャの upvalue — Phase 1 完了、残りは Track B 残スライス待ち

クロージャの自由変数は元々 env HashMap を引き、捕捉時に env をフラットコピーして `Value::Sub` に
持たせていた。**Phase 1 (#3715) 完了**: index ベース `OpCode::GetUpvalue` (`opcode.rs:216`・rewrite は
`compute_upvalues` `opcode.rs:2321`) で、**既に共有 `ContainerRef` セルになっている捕捉だけ**を
upvalue 配列で読む（それ以外は env を live で読む `name_idx` フォールバック＝常に健全・
`vm/vm_register_ops.rs:390-420`）。

**残り (Phase 2+): read-only 定数キャプチャの値化・同名修正・write 経路・env コピー撤去は
Track B の一般キャプチャセル化待ち。** 注意: **Track B slice 1-3（#4241/#4245/#4251）は
atomic 要素セルと `state` 集約セルであり、一般 captured-lexical のセル化には未到達** —
Phase 2 の前提はまだ揃っていない。試みて分かった健全性の壁（rev5 の調査結論・依然有効）:
- **値スナップショットは unsound**: mutsu の compile 時 mutation 解析は role/class メソッドの
  書き込みや `cas`/`is rw` 等の rw-arg sink を見落とすため、凍結すると **flaky 化**する
  (`S12-construction/roles-6e.t`)。→ セル (参照捕捉) が唯一の健全策。
- **read-only キャプチャの一律セル化 (#2749) は別の壁**: ContainerRef 非 deref 経路
  （型 dispatch・不変性・`.kv` rw）と `is raw`/`cas` の by-name 書き戻しが壊れ、
  per-iteration × cross-thread でデッドロックする。ゲート積みは場当たり＝リスク。

なお `compute_needs_env_sync` の保守的フォールバックと `box_captured_lexicals` による
captured-mutated-escaping ローカルの `ContainerRef` 昇格は従来どおり (`opcode.rs:1476-1591`)。

### 1.4 ローカルスロットのレキシカルスコープ不在 — **キャンペーン部分着手**

`alloc_local` (`compiler/mod.rs:386`) は変数**名**をキーに get-or-create するだけで、
**内側ブロックのシャドウイングがスロット衝突**する（`my $x` が外側 `$x` と同じスロットに解決）。
正しさは `BlockScope` が毎回 `locals` 全体を clone/restore すること (`vm/vm_misc_scope.rs:169-170`)
で担保されており、この clone こそ最終的に消したいコスト。

rev5-7 の調査結論（依然有効・詳細は git 履歴の rev7 版参照）: 分離スロットの素朴導入は
**writeback 系が実行時に「名前 → slot」を再解決している**5 つの独立機構
（`find_local_slot`・`SmartMatchExpr.lhs_var`・RMW チョークポイント
`store_named_scalar_rmw_result`・`:=` バインド+EVAL・role mixin）で破綻する。
`position`/`rposition` のどちらでも別のサブセットが割れ普遍解はなく、
**唯一の正解はコンパイル時に確定した slot を writeback IR に載せること** — つまり §1.4 は
§1.5 の「名前ベース slot 解決の撤廃」と同一キャンペーンでしか解けない。

**rev8 更新: そのキャンペーンが部分着手済み**（[docs/lexical-scope-slot-campaign.md](docs/lexical-scope-slot-campaign.md)）:
- scope frame push/pop scaffolding (`push_local_scope`/`pop_local_scope` `compiler/mod.rs:522/530`)、
  宣言の単一入口 `declare_local` (`mod.rs:461`)、シャドウ用 primitive `alloc_fresh_local` (`mod.rs:397`)。
  分離スロット本体は `MUTSU_SHADOW_SLOTS` ゲート下 (`mod.rs:462-517`) で、既定は従来どおり
  外側 slot 共有＝挙動不変。
- **writeback IR への slot 焼き込みが進行中**: `SmartMatchExpr` に `lhs_slot: Option<u32>`
  (`opcode.rs:416`)、`store_named_scalar_rmw_result` に slot 引数 (`vm_var_assign_typed.rs:763`)、
  rw-arg ソースは `Pair(name, Int(slot))` として定数プールへ (`compiler/mod.rs:665-691`)、
  実行時は `resolve_local_slot` (`vm_env_helpers.rs:1041`) が焼き込み slot を優先し
  by-name はフォールバックに降格。
- 残: 名前ベース解決の完全撤廃（by-name フォールバックはまだ load-bearing）と、
  `MUTSU_SHADOW_SLOTS` の既定 ON 化・`BlockScope` 全 clone の撤去。

### 1.5 最適化パスの不在 + opcode セット — **監査済み・形状は健全 (2026-07-06)**

定数畳み込み・DCE・peephole・レジスタ割り当ては無い。`1 + 2` は常に `LoadConst, LoadConst, Add` を
出す（唯一の "畳み込み" は `Nil.gist`→"Nil" `compiler/expr.rs:356`）。これは依然事実だが、
**命令セット自体は 2026-07-06 に設計監査を実施し、評決は「形状は健全」**
（[docs/opcode-design-review.md](docs/opcode-design-review.md)・#4279）:

- **variant 数は ~340**（rev7 の「~297」は過小だった）。大半は Raku の意味論表面 + slot 焼き込み/
  fused 特化で正当。定数プールオペランド・複合ループ op（`ForLoop` 等・iteration 毎の jump 往復なし）・
  fused hot op は **JIT Tier A（ADR-0004）が opcode 列を helper call 列に直訳する計画とそのまま整合**
  し、保存すべき資産と判定。
- **修正済み (#4279)**: `size_of::<OpCode>()` が 192B（`ForLoop` の 21 フィールド inline が主因）
  → `ForLoop(Box<ForLoopSpec>)` 化で **48B**（命令ストリーム 4 分の 1・ループ entry 毎の heap clone も
  消滅）。`opcode_size_guard` (`opcode.rs:1387-1405`) が ≤48 を pin。dead variant 3 つ
  （`IsNil`/`JumpIfNil`/`IndexAutovivify`）を削除。**per-opcode 実行ヒストグラム**を
  `MUTSU_VM_STATS` に追加（stats off 時は cached bool 1 load のみ）。
- **未解決（計測駆動で対処予定）**: ①ラベル等の inline `Option<String>` payload
  （`Last`/`Next`/`Redo`/loop 系・`SmartMatchExpr.lhs_var`）の定数プール化。
  ②per-instruction の定数コスト（`current_code` 生ポインタ store・`trace_log!` チェック）。
  ③`Jump(i32)` が絶対 index を運ぶ紛らわしい encoding。
  ④**LHS 構文形での特化 op の統合**（`ContainerEq`×4・`IndexAssign*`×6 等）— ad-hoc 増殖の
  本体だが、統合は美学でなく新ヒストグラムのデータで駆動する方針。
- `is rw` の書き戻しは依然ソース変数名を定数プールに直列化する方式だが、§1.4 キャンペーンで
  slot が併記されるようになった（`Pair(name, Int(slot))`）。IR が lvalue 情報を持たない問題の
  本質は残る。

### 1.6 パーサ (良好) と slang の擬似実装

手書きの scannerless 再帰下降。**precedence 実装は教科書的で良好**
(`parser/expr/precedence.rs`)。`memo.rs` は packrat 的バックトラック緩和。

真の slang スタックは無く、Regex/Quote/Pod それぞれが独立スキャナで擬似切替（機構は rev7 から不変）:
- Regex: パース時は `scan_to_delim` (`parser/primary/regex/scan.rs`) で生テキスト切り出し + 検証のみ、
  構造パースは実行時。
- Pod: パーサは読み飛ばすだけ (`parser/helpers.rs:4,165`)、実構築は実行時に生ソース再スキャン
  (`runtime/io_pod_blocks.rs:4`・§7-8 分割で `io.rs` から移動)。

ユーザ定義 grammar/token/rule の本格対応は将来課題 (CLAUDE.md の認識どおり)。

---

## 2. 正しさ・健全性の残課題

### 2.1 GC (層3a) 導入後の生ポインタ健全性 — **unsoundness は解消ではなく移動**

**完了したこと（ADR-0001 層3a・2026-07-03〜05）**: コンテナ系 `Value`
（Hash/Array/ContainerRef/Set/Bag/Mix/Sub/Instance/Promise/Channel/LazyList）は `Arc<T>` から
`Gc<T>` (`gc/gc_ptr.rs:165-167`・Arc-backed、GC 可視 `header.strong` を Arc カウントと別建て) へ移行。
Bacon-Rajan 型 trial-deletion collector・cooperative cross-thread STW (`gc/stw.rs`)・
safepoint 網（backedge/call/return/await/… `gc/safepoint.rs:45-60`）・`Trace::finalize` による
DESTROY-on-reclaim・`MUTSU_GC_VERIFY`/`MUTSU_GC_LOG` デバッグ・**blocking な CI `gc-stress` ジョブ**
（GC on + VERIFY + `EVERY_CANDIDATE=1024` で test/roast 全走）が揃い、**default-on**
（ADR-0003・トリガは candidate バッファサイズ閾値 + adaptive backoff、`safepoint.rs:28-37`）。
perf コストは bench-class +7.5-8.3%（ユーザ判断で許容・回収は層3b NaN-boxing）。
スカラ系は型フィルタで GC 対象外＝数値/文字列 hot path はコスト 0（ADR-0001 どおり）。

**旧 §2.1 の主張の現状**: 「生ポインタ書き込みは `value::aliased_mut::arc_contents_mut` の
単一チョークポイント」はもう正しくない。`arc_contents_mut` は **dead 化**（`#[allow(dead_code)]`
で温存・`aliased_mut.rs:63-67`）し、本番経路は **`gc::gc_contents_mut`** (`gc/gc_ptr.rs:555-558`) と
`Gc::get_mut`/`make_mut` (`gc_ptr.rs:289-302`/`384-402`) に移った。いずれも
`Arc::as_ptr as *mut` 由来の raw 書き込みで、`aliased_mut.rs:26-35` が明記するとおり
**provenance 違反 + 共有時 data race の本質は未解消のまま引っ越した**。呼び出しサイトは
`vm/vm_var_assign_index_named.rs` (18)・`value/value_methods_a.rs` (7)・
`runtime/methods_mut_dispatch.rs` (5) など。完全解消は Track B の一般要素セル化
（残スライス **T4** multidim cas / **T5** typed-constraint 透過 / **T6** 非 state escaped aggregate —
[docs/gc-post-3a-roadmap.md](docs/gc-post-3a-roadmap.md) §2）。slice 1-3（atomic 要素・state 集約）は
完了済みで、「構造は COW スナップショット・要素値のみセル」が一般化テンプレート。

**rev8 で新たに見つけた GC 側の懸念（いずれも挙動でなく監査性/将来リスク）**:
1. **モジュールヘッダのドキュメントドリフト**: `gc/mod.rs:18-19`・`gc_ptr.rs:22-33`・
   `collect.rs:14-18,41-44` が default-on 切替後も「default off / no production caller yet」と
   主張したまま。健全性監査の読者が本番挙動を誤認する。要 doc sweep。
2. **`gc_ptr.rs:39`・`collect.rs:45` のモジュール全体 `#![allow(dead_code)]`** が「step 8 で
   配線されたら外す」と書かれたまま残存 — step 8 は完了済みで、いまは真の dead surface を
   隠しうる。
3. **`get_mut`/`make_mut` の uniqueness 判定 (`header.strong == 1`) は、candidate バッファが保持する
   *カウント外* の `Arc` clone と共存する**。buffered clone が collect safepoint でしか deref
   されないこと・`Gc::drop` が `collecting()` 中に early-return することに依存する load-bearing な
   不変条件で、散文でしか主張されていない（機械検査は `MUTSU_GC_VERIFY` 時のみ）。
4. **`Gc::make_mut` が `header.strong` を手動で付け替える**（旧ノード `fetch_sub`・新ノード fresh、
   全て `Relaxed`）。同一ノードへの並行 clone/drop と競合した場合に GC 可視カウントが歪む
   最有力経路。

`unsafe` は src/ 全体で ~120 箇所（GC 内部・`gc_contents_mut` 呼び出しサイト・FFI/OS が主）。
`Gc<T>` の `Send`/`Sync` は `Arc` からの自動導出で、手書き `unsafe impl` は `WeakGc` の 2 つのみ
（値を持たないため正当・`gc_ptr.rs:243-244`）。

### 2.2 `RuntimeError` god-struct — **サイズ問題は解消、制御チャネル同居は残置**

`RuntimeError` (`value/error.rs`) が `return`/`last`/`next`/`take`/`emit` を `Result::Err` で運ぶ
構造は不変（`error.rs:33-41`）。ただし rev7 の残課題だった肥大は解消:

- 制御フロー bool 群は **`enum Control`（14 variant・`error.rs:43-74`）+ 単一
  `Option<Control>` フィールドに統合済み**。独立 bool は `fail_handled`/`is_leave` のみ
  （制御シグナルに重畳する性質のため意図的に分離）。
- **cold な routing/診断フィールドを `cold: Option<Box<RuntimeErrorCold>>` へ退避**
  (`error.rs:83-104`) し、本体は 272B → **~120-128B**（clippy 閾値 128B 未満）。
  `#[allow(clippy::result_large_err)]` は **23 → 0**（残る grep hit は説明 doc コメントのみ）。
- 残る size driver は `return_value: Option<Value>`（`Value` 48B）で、さらなる縮小は
  層3b NaN-boxing（`Value` 8B 化）に自然に相乗りする。エラーと制御の**チャネル分離**自体は
  未着手だが、サイズ・可読性の実害は消えたため優先度は低下。

---

## 3. 重複実装

### 3.1 制御構文の文 / 式 二重コンパイル

各制御構文に「文形」と「式形」の別コンパイラがある:
`compile_do_if_expr` / `compile_do_for_expr` / `compile_do_while_expr` / `compile_do_loop_expr` /
`compile_lazy_for_expr` (`compiler/helpers_do_expr.rs:95-354`) が `stmt.rs:1492-1840` /
`helpers_control_flow.rs:113` のロジックを微妙な差異つきで重複。#4279 の `ForLoopSpec` Box 化は
**同一の 21 フィールド構築を両サイト (`helpers_do_expr.rs:227` と `stmt.rs:1755`) に二重適用**する
形になり、重複の実在をむしろ再確認した。**改善**: 値を返す単一パスに統一。

### 3.2 sub/method 本体の二重実装 — 本体実行は単一化済み、宣言登録のみ二重

メソッド/サブの**本体実行**は §B (#3680) で bytecode 単一化された。残るのは**宣言登録**の二重性:
`SubDecl` は `RegisterSub` (インタプリタ登録・`stmt.rs:2650`) **と** `compile_sub_body`
(`stmt.rs:2691-2761`) を両方やる（`stmt.rs:2603` のコメントが明言）。登録側は §1.1 の宣言登録が
依存して load-bearing。本体は登録後にコンパイル済み bytecode で走る。

### 3.3 メソッドディスパッチの多入口・名前マッチ散在

入口が乱立（§7-8 分割後のパスで再確認・統合はされていない）:
`call_method_with_values` (`runtime/methods_call_dispatch.rs:14`)、
`dispatch_method_by_name_{1,2,3}` (`runtime/methods_dispatch_match*.rs`)、
`run_instance_method` (`runtime/class_dispatch.rs:10`)、`native_method_{0,1,2}arg` (`builtins/`)。
同一メソッド名の文字列マッチが分散（`"elems"` が 8+ ファイル、`"join"` 5・`"reverse"` 6 ファイル、
`compiler/mod.rs` にまで）。**改善**: 型 × メソッドの単一ディスパッチテーブルに集約。

---

## 4. ハードコード / ドリフトリスク

露骨な「テスト専用ハードコード出力」は見つからなかった（rev8 で 2026-06-25 以降の parser 変更を
全数レビューし、新規のテスト特化ハックが増えていないことも確認）。以下は導出を怠った直書きで
ドリフトリスクがある。

1. **`.^methods`/`.can` の型別メソッド一覧が直書き** 【単一の正典モジュールに集約済み】:
   ネイティブメソッドディスパッチは `match method { "chars" => ... }` のアーム実装で**列挙可能な
   レジストリが無く**、実行時に型→メソッド集合をリフレクションで導出できない。現状の対応:
   型別メソッド表と組込 MRO を唯一の正典モジュール `builtins/builtin_type_methods.rs` に集約し、
   `.^methods`/`.^can`/`.^mro` が全てそこから読む単一情報源化。構造テストと
   `t/can-methods-drift.t` がドリフトを検出。
   **残**: §3 の統合ディスパッチ表が入れば、この表自体を撤去してディスパッチから真に導出できる。

2. **パーサの roast 向け文法緩和** (軽微・現存): `parser/stmt/decl/helpers.rs:30` (`is List` 受理)、
   `parser/primary/misc/colonpair.rs:73` (Test::Assuming 周辺)、`parser/stmt/stmtlist.rs:337,394`・
   `parser/stmt/control.rs:72` (`throws-like` 特例構文)。

---

## 5. 値モデル・性能・robustness

- **状態を値の外に置く設計（残存）**: Failure の handled/pending レジストリは依然 `thread_local!`
  (`value/mod.rs:521-557`) で、Value がスレッド境界を越えると登録が失われる。pending DESTROY の
  キューも `thread_local` のまま (`value/mod.rs:341`) — ただし**発火トリガは GC の
  `Trace::finalize` に移行済み**（refcount 死・cycle reclaim の両方で発火・二重発火は
  `Instance.finalized: AtomicBool` でガード）。Seq の consumed/lazy 状態は
  `OnceLock<Mutex<Vec<Weak>>>` の O(n) 線形スキャンのまま (`value/mod.rs:18-42`)、Gc 移行した
  `LazyList` 用に `CONSUMED_LAZYLISTS` (`WeakGc` の Vec・`value/mod.rs:76-79`) が**増えた**。脆く遅い。
- **Env のレキシカルチェーン**: rev7 の「フラット COW HashMap」は部分的に古い。
  `Env` は COW (`Arc<FxHashMap<Symbol,Value>>` + `make_mut`) のまま、**scoped parent-overlay
  チェーン**（`parent`/`tombstones`/`depth`、`MAX_OVERLAY_DEPTH=16`・`env.rs:174-198`）を獲得。
  また「変数読み取り毎にグローバル `RwLock<SymbolTable>`」は **Symbol キーの hot path
  (`get_sym`) には当たらない** — intern するのは文字列キーの便宜 API (`get`/`contains_key`/
  `remove`・`env.rs:314-379`) のみ。dispatch hot path の Symbol↔String round-trip は PLAN §5 の
  残レバー。
- **`.clone()` 約 9022 箇所（rev7 比 +1322・増加傾向）**: Value 所有渡し設計の必然コストに
  GC/Track B キャンペーンの churn が乗った。bench-class の ~50% が clone/drop であり、
  根本解は層3b NaN-boxing。
- **アロケーション失敗 abort**: ユーザ指定サイズの確保（配列 autoviv 巨大 index・文字列リピート・
  shaped 配列宣言・`Buf.allocate`）は全て `try_reserve` でガード済み。残る理論上の abort は
  正当な操作からの真の OOM のみ。Supply の detached worker での panic は握り潰されるため、
  QUIT への伝播は別途要検討。
- **`unwrap`/`expect`/`panic!`/`unreachable!` 約 1643 箇所（+167）**: 大半は不変条件アサート/
  テスト内/ガード済みで許容だが、増加傾向は監視対象。
- **`#[allow(lint)]` 157 箇所（+19）**: `result_large_err` は 0 になった一方（§2.2）、
  GC モジュールの `#![allow(dead_code)]` 2 つ（§2.1 懸念②）など新規が増えた。

---

## 6. リポジトリ衛生

- **テストが CWD に一時ファイルを書き散らす — 根本原因を修正済み**: `roast/S16-io/bom.t` の
  `LEAVE unlink` を落としていた tail-block インライン化バグを修正（pin=`t/tail-block-leave-phaser.t`）。
  残: bare block での KEEP が raku では発火しないのに mutsu は発火する（別軸）。
- **500 行規約**: §7-8 sweep（rev7）後の現状は **>1000 行が 51 ファイル・>500 行が 197 ファイル**。
  最大は `vm/vm_exec_dispatch.rs` 3967・`runtime/methods_call_dispatch.rs` 3361・
  `compiler/stmt.rs` 3219・`runtime/regex_parse_core.rs` 3005・`opcode.rs` 2875（#4279 で
  `ForLoopSpec` を収容して肥大）。giant な単一 `match`/dispatch メソッドは**意図的な indivisible
  例外**だが、`runtime/mod.rs` が 1932→**2118 行に再肥大**しており、facade の再スリム化が要る。
  `vm.rs` module root は 292 行を維持。

---

## 7. 推奨ロードマップ (優先度順)

過去 rev の上位課題（遅延リスト・並行ライブセル・panic 境界・regex 統合・`Value` 縮小・
単一ストア化・tree-walk 撤去・**GC 層3a**・**`RuntimeError` 縮小**・**OpCode 縮小/監査**）は
すべて完了済み（詳細は news）。大型の順序は ADR-0001/0004 で確定:
**層3a GC ✅ → 層3b NaN-boxing → 層4 JIT (Cranelift・Tier A→B・Lever 3 凍結)**。残る課題のみ:

| # | 残課題 | 区分 | 効果 |
|---|------|------|------|
| 1 | **Track B 残スライス T4-T6**（multidim cas / typed-constraint 透過 / 非 state escaped aggregate — `gc_contents_mut` の生ポインタ書き込み撤廃の残り・§2.1、gc-post-3a-roadmap §2） | 健全性 (UB) | 中〜大 |
| 2 | **GC 監査性 sweep**（stale な "default off" ヘッダ是正・モジュール全体 `#![allow(dead_code)]` 撤去・buffered-clone 不変条件の debug assert 化・`make_mut` の strong 手動付け替えの ordering 検討・§2.1 懸念①-④） | 健全性 + 衛生 | 中（低コスト） |
| 3 | ローカルスロットのレキシカルスコープ完成（slot 焼き込みの残り→ by-name フォールバック撤廃 → `MUTSU_SHADOW_SLOTS` 既定 ON → `BlockScope` 全 clone 撤去・§1.4） | 正しさ + 性能 | 中 |
| 4 | **層3b NaN-boxing**（`Value` 48→8B・clone/drop コストと GC +8% の回収・JIT の前提・gc-post-3a-roadmap §3） | 性能 | 大 |
| 5 | クロージャ upvalue Phase 2+（#1 の一般キャプチャセル化が前提・§1.3） | 性能 + 正しさ | 中 |
| 6 | opcode 残件（ラベルの定数プール化・per-instruction 定数コスト・`Jump` encoding・ヒストグラム駆動の特化 op 統合・§1.5、opcode-design-review §2/5/6） | 性能 + 設計 | 低〜中 |
| 7 | method dispatch の resolution caching + 多入口統合（§1.1/§3.3。統合表は §4-1 のドリフト解消も兼ねる） | 設計 + 性能 | 中 |
| 8 | Supply worker panic の QUIT 伝播（§5） | 堅牢性 (任意) | 低 |
| 9 | 衛生: `runtime/mod.rs` 再スリム化・`.clone()`/`unwrap` 増加トレンドの棚卸し・巨大ファイル分割の残（§5/§6） | 衛生 | 低〜中 |

---

*この分析は静的読解 + 実機再現に基づく。*
*rev8 (2026-07-06): GC 層3a 完了後の全体再評価。解消済み項目（RuntimeError 縮小・OpCode 監査）を
冒頭リストへ移し、§2 を GC 導入後の健全性評価に書き換えた。*
