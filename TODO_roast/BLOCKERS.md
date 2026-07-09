# roast ブロッカー再評価メモ

この文書は、roast の失敗を「テストファイル単位」ではなく
**根本原因単位**で追うための索引。
個々の失敗を片端から潰すためではなく、
**今どこを直せば何がまとめて動くか**を判断するために使う。

**最終更新 2026-07-09**（旧 §4「並行・非同期（S17）」をセクションごと削除。S17 配下の全 99 ファイル
（S17-supply / S17-lowlevel / S17-promise / S17-procasync / S17-scheduler ほか）と socket 系
（`S32-io/IO-Socket-Async*`・`socket-recv-vs-read.t` 等）がすべて whitelist 済みになり、
cross-thread plain-scalar lexical writeback（旧 §4.1）も escape 解析→ContainerRef セル昇格の
キャンペーン（#4336/#4340/#4345/#4348）で解消。詳細は `news/2026-07.md`）

## この文書の読み方

- ファイル名の羅列ではなく、**根本原因単位**でブロッカーを追う。
- 各ブロッカーは次の粒度で書く:
  - **根本原因**: 何の抽象が欠けているか
  - **変更レイヤ**: parser / compiler / VM / runtime / value 表現 / scheduler のどこを触るか
  - **Next slice**: 次に何を切り出して着手するか
  - **完了条件**: どこまで行けば、その類の roast がまとめて動くか
- **whitelist 済みになった項目はこの文書から削除し、詳細は `news/` に移す。**
  このファイルは常に「現在まだ開いている残件」だけを持つ。
- **あるセクションの残件がゼロになったら、セクションごと削除する。** 完了報告は `news/` に残す。
- 個別の per-file 詳細ログは `TODO_roast/S*.md` を参照。このファイルはそちらの要約と
  優先順位づけに徹する。
- **このファイルは頻繁に複数セッションから並行更新される。** 着手前に必ず
  `MUTSU_FUDGE=1 prove -e target/debug/mutsu roast/<path>.t` で実際の pass/fail 数を
  取り直すこと。ここに書かれた数字は執筆時点のスナップショットであり、数時間で
  古くなることがある。

## 現在の前提

- whitelist は **1372**（2026-07-09 時点、`wc -l roast-whitelist.txt`）。
- 安い 1 ファイル勝ちはほぼ枯渇している。残件の大半は
  **第一級コンテナ / container identity** — 配列・ハッシュ要素・属性 slot の書き戻し（§3）—
  に集約される。
- かつてここにあった「真の lazy 配列 / 無限列」「dispatch / 演算子 sugar の desugar surface」
  「並行・非同期（S17）」はいずれも完了済み（詳細は `news/2026-06.md` / `news/2026-07.md`）。

---

## 1. Hard — 単体では動かせない、大きな下位基盤待ち

### 1.1 Unicode / RakuAST（低 ROI・据え置き）

- `S32-str/format.t`（reachable 26/49 は全 pass、test 27-29 で中断）
  - **根本原因**: `Formatter::Syntax.parse`→Match、`Formatter.CODE`→Callable、
    `Formatter.AST`→`RakuAST::Node` を要求し、ここで runtime error 中断するため以降が到達不能。
    本質は **RakuAST サブシステム不在**（参照 raku 本体すら `Format` 6.e 未対応）。
  - **評価**: RakuAST 本実装なしには whitelist 不可。stub 化は禁止のため据え置き。

---

## 2. 局所修正で進む残り

### 2.2 `S02-types/generics.t`

- **現状**: 0/1（変化なし）。
- **論点**: 6.e の coercion type 項 + `Array[T]` サブクラス化が必要。ローカル raku
  (v2022.12=6.d) でもこのテスト自体がコンパイルに失敗するため、参照実装での検証すらできない。
- **評価**: 深い機能待ちで、局所修正では進まない。優先度は低い。

### 2.3 multislice lvalue（hash 側の残件）

- **対象**: `S32-hash/multislice-6e.t`。array 側（`S32-array/multislice-6e.t`）は
  **812/812 全通過・whitelist 済み**（この文書の旧「28 失敗」は stale だった）。
- **進捗（2026-07-09 container-identity slice）**: hash 側 269 fails + test 318 で
  中断 → **32 fails・535/549 実行**まで前進。直ったもの:
  - Test 関数の `&` 参照（`my &fn = &is-deeply`）が Nil だった → Routine 値化
    （318 中断の正体は block 3 冒頭の `(?? &is-deeply !! &non-assignable-ok)(...)`）。
  - hash multislice の Whatever / key-list 次元 × 全 adverb（:k/:kv/:p/:v/:exists/
    :delete）— leaf 収集を `Value` パス化し hash 対応。
  - `\target` bind：hash root の cell 昇格解禁・欠落キーは deep-path `HashEntryRef`、
    materialize を AssignExprLocal / AssignExpr / SetGlobal / boxed-cell
    write-through（`Value::store_through_cell`）に配線。
  - boxed captured `@a`/`%h` の whole-container 再代入が container identity を保持
    （`cell_store_preserving_container_identity`）。
  - Pin: `t/hash-multislice-container.t`。
- **残る根本原因（32 fails・multislice ではない）**: sigilless map param を入れ子
  呼び出しの引数に渡すと（`.map(-> \k, \v { Pair.new(k,v) })`）、varref の名前ベース
  再解決が stale env を読む。値が List のとき `k` が最初の chunk の値を繰り返す
  （文をまたいでも漏れる）。再現:
  `((1,2),"A",(3,4),"B").map(-> \k, \v { Pair.new(k,v) })` →
  `((1,2)=>"A", (1,2)=>"B")`。plain read（`k.raku`）は正しく、call-arg 位置のみ壊れる。
  main でも再現する pre-existing。**Next slice = この varref-by-name 再解決の修正**
  （`compile_call_arg` の WrapVarRef / binding_signature の
  `resolve_sigilless_alias_source_name` 系）。plan mismatch（535/549）も残件。

---

## 3. 第一級コンテナ / container identity

かつて element/attribute slot の書き戻しが未整備だった項目が集まっていたが、大半は
whitelist 済み（`splice.t`・`whatever.t`・`S14-traits/attributes.t`（#4314）・`temp.t`・
`adverbs.t` の typed-hash default・`capture.t`・`is_default.t`・`walk.t` など。詳細は
`news/2026-06.md` / `news/2026-07.md`）。残るのは multislice hash 側（§2.3）と、
下記サブキャンペーンの深い残課題。

### 3.1 完了済み（whitelist 済み・詳細は news）

この節の対象はすべて whitelist 済みになった。詳細は各 news を参照:

- `S14-traits/attributes.t`（#4314・`Attribute.container.VAR does Role`）→ `news/2026-07.md`
- `S32-array/splice.t`（whole-container 代入の in-place 化）→ `news/2026-07.md`
- `S02-types/whatever.t`（#4067・WhateverCode over-currying）→ `news/2026-07.md`
- `S12-subset/subtypes.t`（92/92）→ `news/2026-07.md`
- `S26-documentation/12-non-breaking-space.t`（top-level BEGIN compile-time hoist）→ `news/2026-07.md`
- `S02-names-vars/variables-and-packages.t`（nested-block BEGIN hoist）→ `news/2026-07.md`

**残る別軸の残課題**（属性 slot の完全な cell 化待ち・§3.2 item 2）:
`$my_ref := $obj.attr`（scalar accessor への `:=` 束縛）・`$obj.attr.VAR.role = v`
（mixin accessor への rw 書込）。closure-captured shared-cell を list へ by-value 捕捉した
ときのスナップショット追従（return-writeback 系・`splice.t` の `ident4`）も同族。

### 3.2 サブキャンペーンと choke point

1. **配列/ハッシュ要素 cell 化**
   - 依存文書: `docs/container-identity.md`
   - 変更レイヤ: `value/mod.rs`、`vm_var_assign_ops.rs`、`vm_var_index_ops.rs`
   - 対象: `splice.t`, multislice（§2.3）
   - 完了条件: element bind / take-rw / deep nested write が post-call writeback なしで成立
2. **属性 accessor を value copy ではなく slot 経由にする**
   - 変更レイヤ: attribute read/write path、instance attr storage
   - 対象: `Attribute.container.VAR does Role` は #4314 で完了（§3.1 参照）。残りは
     `$my_ref := $obj.attr`（scalar accessor への `:=` 束縛）・`$obj.attr.VAR.role = v`
     （mixin accessor への rw 書込）
3. **BEGIN/EVAL/lexical 配列変更の永続化** — **DONE**。
   - top-level BEGIN 版（`S26-documentation/12-non-breaking-space.t`、`run_toplevel_begin_phasers`）・
     nested-block BEGIN 版（`S02-names-vars/variables-and-packages.t`、`reorder_at_level`）とも
     whitelist 済み（§3.1 参照）。

コード choke point:

- 要素 write 集約: `src/value/mod.rs::assign_element_slot`, `src/value/mod.rs::hash_insert_through`
- 要素 read decont: `resolve_array_entry`, `resolve_hash_entry`
- whole-container / env-local coherence: `docs/env-locals-coherence.md`,
  `src/vm/vm_env_helpers.rs`, `SetLocal` / `flush_local_to_env` / bound-container metadata

- **Next slice**: `splice.t` の self-splice ケースから、配列 write を helper 経由へさらに
  寄せて post-call writeback 依存を 1 段減らす。
- **Canary tests**: `roast/S32-array/splice.t`,
  `roast/S02-names-vars/variables-and-packages.t`

---

## 4. whitelist を目標にしない項目

### 4.1 rakudo 側も失敗する、または roast 側の問題が強いもの

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

### 4.2 低 ROI で後回しにすべきもの

- `S32-str/format.t` — `RakuAST` が無い限り最後まで通しにくい（§1.1 参照）。

---

## 5. 今のおすすめ着手順

「次に何をやるか」を 1 本だけ選ぶなら、これ:

1. **第一級コンテナ campaign**（§3）— `docs/container-identity.md` に沿って
   multislice hash 側の slot identity を前に進める。
   これは腰を据えた基盤工事で、個々のテストを直接潰すより効果が大きい。
   **進捗（2026-07-08）**: whole-container 代入の container-identity（`splice.t`）と
   属性 container mixin（`S14-traits/attributes.t`・#4314）は完了・whitelist 済み
   （§3.1 参照）。残るは closure-captured shared-cell を list へ by-value 捕捉したときの
   スナップショット追従（return-writeback 系）と、属性 slot の完全な cell 化
   （`:=` 束縛・mixin accessor への rw 書込）。

かつて 2 番手だった cross-thread lexical writeback campaign（旧 §4.1）は完了
（`S17-lowlevel/lock.t`・`S32-io/socket-recv-vs-read.t` とも whitelist 済み、
詳細は `news/2026-07.md`）。

whitelist を目標にしない §4 の項目は、mutsu 側の一般改善のついでに触れるのはよいが、
そのファイル単体を通すことを目的にしない。
