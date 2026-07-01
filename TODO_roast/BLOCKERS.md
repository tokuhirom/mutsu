# roast ブロッカー再評価メモ

この文書は、roast の失敗を「テストファイル単位」ではなく
**根本原因単位**で追うための索引。
個々の失敗を片端から潰すためではなく、
**今どこを直せば何がまとめて動くか**を判断するために使う。

**最終更新 2026-07-01**（2026-06-29〜07-01 の大量の完了項目を `news/2026-06.md` /
`news/2026-07.md` に移し、残件のみを再集計した版）

## この文書の読み方

- ファイル名の羅列ではなく、**根本原因単位**でブロッカーを追う。
- 各ブロッカーは次の粒度で書く:
  - **根本原因**: 何の抽象が欠けているか
  - **変更レイヤ**: parser / compiler / VM / runtime / value 表現 / scheduler のどこを触るか
  - **Next slice**: 次に何を切り出して着手するか
  - **完了条件**: どこまで行けば、その類の roast がまとめて動くか
- **whitelist 済みになった項目はこの文書から削除し、詳細は `news/` に移す。**
  このファイルは常に「現在まだ開いている残件」だけを持つ。
- 個別の per-file 詳細ログは `TODO_roast/S*.md` を参照。このファイルはそちらの要約と
  優先順位づけに徹する。
- **このファイルは頻繁に複数セッションから並行更新される。** 着手前に必ず
  `MUTSU_FUDGE=1 prove -e target/debug/mutsu roast/<path>.t` で実際の pass/fail 数を
  取り直すこと。ここに書かれた数字は執筆時点のスナップショットであり、数時間で
  古くなることがある。

## 現在の前提

- whitelist は **1338**（2026-07-01 時点、`4c311c71`、`wc -l roast-whitelist.txt`）。
- 安い 1 ファイル勝ちはほぼ枯渇している。残件の大半は少数の根本原因に集約される:
  1. **第一級コンテナ / container identity** — 配列・ハッシュ要素・属性 slot の書き戻し、
     BEGIN/EVAL 時の lexical 永続化。
  2. **真の lazy 配列 / 無限列** — 残りは `S09-subscript/slice.t` のみ。
  3. **dispatch / 演算子 sugar の desugar surface** — 残りは `hyper.t` のみ
     （`assign.t` は完了、詳細は `news/2026-07.md`）。
  4. **並行実行基盤（S17）** — 大半は片づいたが、`Lock.protect` に本物の
     race condition が残っている。

---

## 1. Hard — 単体では動かせない、大きな下位基盤待ち

### 1.1 Unicode / RakuAST / Collation（低 ROI・据え置き）

- `S32-str/CollationTest_NON_IGNORABLE-3.t`（1367/1369、test 1161, 1171 が失敗）
  - **根本原因**: ICU4X が BMP センチネル noncharacter（U+FFFE/U+FFFF）を特別扱いする一方、
    UCA-17/MoarVM は全 noncharacter に符号位置由来の implicit weight を付与するため。
    mutsu は符号位置を正しく保持しており、差異は ICU4X collator 内部のみ。
  - **評価**: 正しい修正には DUCET ベースの collation 実装差し替え（pure Rust の `feruca` 移行
    または自前実装）が必要。2 ケースのために大仕事かつ per-strength API の互換性リスクもあり、
    低 ROI のため据え置き。着手するなら `feruca` を throwaway で実測してから判断する。
- `S32-str/format.t`（reachable 26/49 は全 pass、test 27-29 で中断）
  - **根本原因**: `Formatter::Syntax.parse`→Match、`Formatter.CODE`→Callable、
    `Formatter.AST`→`RakuAST::Node` を要求し、ここで runtime error 中断するため以降が到達不能。
    本質は **RakuAST サブシステム不在**（参照 raku 本体すら `Format` 6.e 未対応）。
  - **評価**: RakuAST 本実装なしには whitelist 不可。stub 化は禁止のため据え置き。

---

## 2. 局所修正で進む残り

### 2.1 `S04-declarations/my-6e.t`

- **現状**: 108/109（test 61 のみ失敗）。
- **論点**: 直近の一斉修正（block-local scope leak 修正、our-sub block-lexical capture 等）で
  99→106→108 まで前進。残る test 61 は個別に最小再現を取って調査する。
- **Canary**: `roast/S04-declarations/my-6e.t`

### 2.2 `S02-types/generics.t`

- **現状**: 0/1（変化なし）。
- **論点**: 6.e の coercion type 項 + `Array[T]` サブクラス化が必要。ローカル raku
  (v2022.12=6.d) でもこのテスト自体がコンパイルに失敗するため、参照実装での検証すらできない。
- **評価**: 深い機能待ちで、局所修正では進まない。優先度は低い。

### 2.3 multislice lvalue（array/hash）

- **対象**: `S32-array/multislice-6e.t`（784/812、28 失敗）、
  `S32-hash/multislice-6e.t`（318 実行時点で plan mismatch、90 失敗——ファイルが途中で中断）。
- **論点**: `@a[0;0;0] = 999`、`%h<a;b;c> = 999` の lvalue 書き戻し経路。
- **根本原因**: single subscript と multislice が同じ「書き戻し可能 target 集合」抽象を
  返していない。array 側はかなり前進した（605→157→28 まで縮小）が、hash 側はまだ
  Track B（要素 cell 化・ADR-0001 層3a・GC と統合キャンペーン）待ちで大きく残っている。
- **変更レイヤ**: `vm_var_index_ops` / `vm_var_assign_ops` / lvalue binding surface
- **Next slice**: array 側の残り 28 件から着手し、hash 側の plan mismatch（中断の原因）を
  先に切り分ける。
- **Primary files**: `src/vm/vm_var_index_ops.rs`, `src/vm/vm_var_assign_ops.rs`

### 2.4 `S06-operator-overloading/infix.t`

- **現状**: 35/42（7 失敗: test 22-23, 28, 31-32, 35, 40）。
- **経緯**: `import ClassName` 経由の演算子メソッド export（#3982）で 14→35 まで前進したが、
  直近のコミットメッセージが示唆する「ほぼ完了」には未達。test 11 は rakudo 自身が
  `#?rakudo todo` で divergent としている既知の1件で、それとは別に 7 件が未解決。
- **評価**: whitelist する前にこの 7 件を個別に潰す必要がある。安易に whitelist しないこと。
- **Canary**: `roast/S06-operator-overloading/infix.t`

---

## 3. 第一級コンテナ / container identity

`S32-array/splice.t` の self-splice、`S02-types/whatever.t` の container preservation、
`S14-traits/attributes.t` の per-attribute Scalar container など、
element/attribute slot の書き戻しが未整備な項目が集まっている。
すでに whitelist 済みの項目（`temp.t`、`adverbs.t` の typed-hash default、`capture.t`、
`is_default.t`、`walk.t` など）は `news/2026-06.md` を参照。

### 3.1 残っている対象

- `S02-names-vars/variables-and-packages.t`（30/39、9 失敗: test 29-34, 37-39）
  — BEGIN-time compile execution、forward-reference detection が根本原因。
- `S14-traits/attributes.t`（4/8、bad plan：8 planned に対し 4 で中断）
  — `Attribute.container.VAR does Role` に必要な「scalar 属性が Scalar コンテナを VAR として
  持ち、合成時に role を mixin し、それがインスタンスの per-attribute コンテナに伝播する」
  という深いコンテナ表現。mutsu の scalar 属性は値を直接保持し VAR は Any を返すため、
  属性 slot の cell 化（本項の Primary files 参照）待ち。`$my_ref := $obj.attr`
  （scalar accessor への `:=` 束縛）も同根の残課題。
- `S12-subset/subtypes.t`（83/90、7 失敗: test 25, 68, 77, 83, 87-89）
  — where-block placeholder、expr-position constraint persistence、capture where の一部。
- `S02-types/whatever.t`（122/130、8 失敗: test 111, 119-124, 126）
  — WhateverCode の over-currying（CallOn-arg branch で握りすぎる）。他ケースを壊さない
  narrow fix が必要。
- `S32-array/splice.t`（357/392、35 失敗）
  — self-splice / push-replace-self が true first-class element container を要求する
  container identity の canary。
- `S26-documentation/12-non-breaking-space.t`（1/2、test 2 が失敗）
  — Pod テーブル中の non-breaking space 文字を網羅列挙してテストする subtest。
  過去の分析では「BEGIN 中の配列 lexical 変更が外へ永続化しない」ことが疑われていたが、
  専用の再調査はまだ未実施。個別の最小再現から始める必要がある。

### 3.2 サブキャンペーンと choke point

1. **配列/ハッシュ要素 cell 化**
   - 依存文書: `docs/container-identity.md`
   - 変更レイヤ: `value/mod.rs`、`vm_var_assign_ops.rs`、`vm_var_index_ops.rs`
   - 対象: `splice.t`, multislice（§2.3）
   - 完了条件: element bind / take-rw / deep nested write が post-call writeback なしで成立
2. **属性 accessor を value copy ではなく slot 経由にする**
   - 変更レイヤ: attribute read/write path、instance attr storage
   - 対象: `S14-traits/attributes.t` の残り（`Attribute.container.VAR does Role`）
3. **BEGIN/EVAL/lexical 配列変更の永続化**
   - 変更レイヤ: env/local coherence、block escape、BEGIN 実行時 lexical carrier
   - 対象: `S26-documentation/12-non-breaking-space.t`、
     `S02-names-vars/variables-and-packages.t`

コード choke point:

- 要素 write 集約: `src/value/mod.rs::assign_element_slot`, `src/value/mod.rs::hash_insert_through`
- 要素 read decont: `resolve_array_entry`, `resolve_hash_entry`
- whole-container / env-local coherence: `docs/env-locals-coherence.md`,
  `src/vm/vm_env_helpers.rs`, `SetLocal` / `flush_local_to_env` / bound-container metadata

- **Next slice**: `splice.t` の self-splice ケースから、配列 write を helper 経由へさらに
  寄せて post-call writeback 依存を 1 段減らす。
- **Canary tests**: `roast/S32-array/splice.t`, `roast/S14-traits/attributes.t`,
  `roast/S02-names-vars/variables-and-packages.t`

---

## 4. 真の lazy 配列 / 無限列

`eqv` の both-lazy ガードと `+a`/`+@a` の Seq single-pass consumption は完了済み
（詳細は `news/2026-06.md`）。残るのは 1 ファイルのみ。

### 4.1 `S09-subscript/slice.t`

- **現状**: 32/56（24 失敗）。以前は途中で中断していたが、周辺の一般修正で最後まで
  実行が通るようになった。
- **論点**: sequence-literal-in-index、slice adverbs、lazy semantics が絡む。
  `docs/lazy-arrays.md` の L2b-L4 と同じ話 — Array が lazy を保ったまま
  mutation / slice / slurpy に耐えられない。
- **変更レイヤ**: `LazyList` 表現、`@`-assign preserve、mutation 時 reify、slice/index
- **Primary files**: `src/runtime/resolution_lazy.rs`, `src/runtime/methods_call_dispatch.rs`,
  `src/runtime/methods_type_coerce.rs`
- **Next slice**: 24 件の失敗を種類ごとに分類し、`docs/lazy-arrays.md` に沿って
  reify/mutation を進める。

---

## 5. dispatch cluster

`wrap.t` / `dispatching.t` / `qualified.t` / `assign.t` は完了済み（詳細は
`news/2026-06.md` / `news/2026-07.md`）。残るのは演算子 sugar が dispatch へ落ちるまでの
surface のうち 1 ファイル。

### 5.1 `S03-metaops/hyper.t`

- **現状**: 420 planned 全実行、5 失敗（test 77, 362, 407-408, 411）。plan mismatch は解消。
- **今回の進捗**: plan mismatch の原因は `<<op>>` 系ハイパーメタ演算子の閉じ `>>` 探索が
  演算子文字列自体と重なるケース（`<<=>>>` = `<<`+`=>`+`>>`）で誤って最短一致を採用し、
  以降のファイル全体を静かに打ち切っていたパーサバグだった（`hyper_concat.rs`）。
  副次的に `set_shared_var`（`@`/`%` への env 書き込み全経路を通る、hyper 専用ではない
  一般関数）が非スレッド文脈でも List→Array を無条件正規化しており、`:=` で束縛した
  List の kind を最初の env sync で破壊していた一般バグも発見・修正。詳細は
  `TODO_roast/S03.md` の hyper.t エントリと news 参照。
- **残り 5 件**: (1) test 77 `.i` の dotted-vs-backslash 区別（AST 未分化）、
  (2) test 362 ユーザ定義 custom infix 演算子とビルトイン演算子文字の字句衝突
  （`infix:<+-*/>` の `*` が Whatever と誤認）、(3)(4) test 407-408 `».+`/`».*` の
  all-candidates MRO dispatch 未実装、(5) test 411 `xx` の per-element hyper 分配。
  いずれも局所修正では閉じない、それぞれ独立した深掘りが必要な項目。
- **変更レイヤ**: `src/parser/expr/precedence_meta_ops/hyper_concat.rs`,
  `src/vm/vm_hyper_method_ops.rs`, `src/vm/vm_hyper_ops.rs`,
  `src/runtime/runtime_shared_vars.rs`
- **Next slice**: 残り 5 件のうち (3)(4) の all-candidates dispatch から着手するのが
  最も汎用的（他の `».+`/`».*` 系テストにも波及しうる）。

---

## 6. 並行・非同期（S17）

大半は完了した（semaphore.t、then.t、scheduler/basic.t、supply/migrate.t、supply/stable.t、
S17-promise/start.t、S07-hyperrace/basics.t、S17-lowlevel/cas-int.t —
詳細は `news/2026-06.md` / `news/2026-07.md`）。残るのは次の 2 ファイル。

### 6.1 `S17-lowlevel/lock.t` — 本物の race condition

- **現状**: 3 planned に対し TAP 順序異常＋2 失敗。`Thread .join` を実スレッド join に
  ルーティングする修正（#4026）が landed した後も再現する。
- **症状**: `Lock.protect` 配下で複数スレッドが共有カウンタを increment するテストで、
  期待列（10, 20, 30, ..., 1000）に対し実際の出力から特定のインクリメントが欠落したり
  重複したりする（例: `420` が `0` になる、`10` が丸ごと消える、`710` が二重に出る）。
  これは shared-scalar coherence の未解決部分であり、`S17-lowlevel/semaphore.t` で行った
  `call_protect_block` 型の reconcile/writeback への寄せが `Lock` 自体にはまだ
  適用されていない可能性が高い。
- **変更レイヤ**: `shared_vars` / `shared_vars_dirty`、`Lock.protect` の critical section
  entry/exit
- **Primary files**: `src/runtime/native_methods/state_lock.rs`,
  `src/runtime/runtime_shared_vars.rs`
- **Next slice**: `Lock.protect` を `call_protect_block`（既存の resync/writeback
  chokepoint）へ実際に寄せられているか確認し、寄せられていなければ semaphore.t の修正
  （#3992）と同じパターンを適用する。TAP 順序異常は別に fire-and-forget スレッド出力の
  drain タイミングの問題の可能性がある。

### 6.2 `S17-supply/syntax.t`

- **現状**: 88/90（test 75, 90 が失敗）。根本原因分析は `TODO_roast/S17.md` に記録済み:
  - test 75: nested `whenever`（on-demand supply が `whenever Supply.interval {}` の
    内側にある場合）は react loop でなく非-react tap 経路（`native_supply_mut`
    tap/act）を通るため、その async body 完了時の `on_close_callbacks` 発火が
    react-loop 側の仕組みに乗らない。
  - test 90: 2 つの `whenever` がそれぞれ `last` する cross-whenever ordering — hard case、
    据え置き。
- **Canary**: `roast/S17-supply/syntax.t`

### 6.3 async IO / Proc::Async / socket supplies（変化なし）

- **対象**: `procasync/*`、`IO-Socket-Async*`
- **根本原因**: encoding、stdin/stdout/stderr supply bridge、socket/listener 駆動が未成熟。
- **変更レイヤ**: `methods_object_native_ctors_io.rs`、`native_methods/socket_*`、encoding layer
- **Next slice**: encoding 無しの `Proc::Async` happy-path を supply bridge と done/quit
  delivery まで揃える。
- **Primary files**: `src/runtime/methods_object_native_ctors_io.rs`,
  `src/runtime/native_proc_async.rs`, `src/runtime/native_methods/socket_async_conn.rs`

---

## 7. whitelist を目標にしない項目

### 7.1 rakudo 側も失敗する、または roast 側の問題が強いもの

ここは mutsu 側で一般改善が入るのはよいが、
**そのファイルを whitelist すること自体を目標にしない**。

- `S05-nonstrings/basic.t`
- `S05-metasyntax/angle-brackets.t`
- `S05-mass/rx.t`
- `S06-advanced/caller.t` — stale test・over-stated plan（22 planned vs 実質 19 assertion）。
  rakudo 自身の期待値と乖離しており unpassable と確定済み（#3975）。
- `S06-advanced/return_function.t`
- `S10-packages/require-and-use--dead-file.t`
- `S12-traits/parameterized.t`
- `S12-meta/exporthow.t`
- `S12-class/open_closed.t`
- `S32-str/sprintf.t`

### 7.2 低 ROI で後回しにすべきもの

- `S32-str/CollationTest_NON_IGNORABLE-3.t` — 2 ケースのために実装が重すぎる（§1.1 参照）。
- `S32-str/format.t` — `RakuAST` が無い限り最後まで通しにくい（§1.1 参照）。

---

## 8. 今のおすすめ着手順

「次に何をやるか」を 1 本だけ選ぶなら、順番はこう見るのが妥当。

1. **`S06-operator-overloading/infix.t` の残り 7 件**（§2.4）— 局所修正で閉じられる可能性が高い。
2. **`S04-declarations/my-6e.t` の test 61**（§2.1）— 残り 1 件。
3. **`S17-lowlevel/lock.t` の race condition**（§6.1）— semaphore.t で確立した
   reconcile/writeback パターンを `Lock` にも適用できれば早い。
4. **第一級コンテナ campaign**（§3）— `docs/container-identity.md` に沿って
   splice.t / attributes.t / multislice hash 側の slot identity を前に進める。
   これは腰を据えた基盤工事で、個々のテストを直接潰すより効果が大きい。
5. **`S03-metaops/hyper.t`**（§5）— plan mismatch の原因を先に特定してから、
   残りの失敗を分類して潰す。
6. **`S09-subscript/slice.t`**（§4）— 24 件を種類ごとに分類し、lazy array 基盤工事の
   一環として進める。

whitelist を目標にしない §7 の項目は、mutsu 側の一般改善のついでに触れるのはよいが、
そのファイル単体を通すことを目的にしない。
