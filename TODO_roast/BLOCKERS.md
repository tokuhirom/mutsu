# roast ブロッカー再評価メモ

この文書は、roast の失敗を「テストファイル単位」ではなく
**根本原因単位**で追うための索引。
個々の失敗を片端から潰すためではなく、
**今どこを直せば何がまとめて動くか**を判断するために使う。

**最終更新 2026-07-08**（whitelist 済みになった `S14-traits/attributes.t`（#4314）と
`S32-str/CollationTest_NON_IGNORABLE-3.t`（#4282）を削除し `news/2026-07.md` へ移動）

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

- whitelist は **1369**（2026-07-08 時点、`wc -l roast-whitelist.txt`）。
- 安い 1 ファイル勝ちはほぼ枯渇している。残件の大半は少数の根本原因に集約される:
  1. **第一級コンテナ / container identity** — 配列・ハッシュ要素・属性 slot の書き戻し
     （属性 slot 側は `S14-traits/attributes.t` の container mixin が #4314 で完了）。
  2. **真の lazy 配列 / 無限列** — 完了（`S09-subscript/slice.t` も whitelist 済み・§4 参照）。
  3. **dispatch / 演算子 sugar の desugar surface** — 完了（`hyper.t`・`assign.t`
     とも whitelist 済み、詳細は `news/2026-07.md`）。
  4. **並行実行基盤（S17）** — `Lock.protect` の race condition は解決済み
     （§6 参照）。残るのは `S17-supply/syntax.t` のみ。

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
- **調査結果（2026-07-02）**: array 側の 28 失敗（例: test 245, line 120）を再現したところ、
  直接代入 `@array[0;0;3] = 999`（境界外インデックスへの自動延伸）自体は正しく動く：
  ```
  my @array = [[[42,666,[314]],],];
  @array[0;0;3] = 999;  # OK: [[[42, 666, [314], 999],],]
  ```
  失敗するのは、この**境界外・自動延伸が必要な多次元インデックス式を sigilless bind
  （`\target`）でサブルーチン引数として渡し、後から `target = 999` で代入する**ケース
  （テストの `assignable-ok(\target, \values, @result)` ヘルパーが使うパターン）：
  ```
  sub f(\target, \values) { target = values }
  f(@array[0;0;3], 999);  # mutsu: 何もしない（Nil を返す）; raku: 正しく延伸して代入
  ```
  つまり根本原因は「lvalue 書き戻し経路」一般ではなく、**§3 のコンテナ id キャンペーン
  （配列/ハッシュ要素の cell 化）そのもの**——多次元インデックス式が返す「書き込み可能な
  参照」が、値としてキャプチャされた後もオリジナルの配列への自動延伸込みの書き込みを
  実現できる第一級コンテナ（cell）になっていない。§2 の「局所修正」の範疇を超え、§3.2 の
  「配列/ハッシュ要素 cell 化」（`docs/container-identity.md`、対象に `splice.t` と並記）と
  同一の基盤工事が前提条件。単体の parser/dispatch 修正では閉じない。
  次に着手するなら §3 のキャンペーンとして本腰を入れる想定で計画すること。

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

## 4. 真の lazy 配列 / 無限列

`eqv` の both-lazy ガードと `+a`/`+@a` の Seq single-pass consumption は完了済み
（詳細は `news/2026-06.md`）。lazy 配列そのものの基盤（L1-L2b、`docs/lazy-arrays.md`
参照）も完了済み。**`S09-subscript/slice.t` は 2026-07-04 に 56/56 で whitelist 済み**
（残っていた test 35 = nested slice adverbs を解決。詳細は `news/2026-07.md` /
`TODO_roast/S09.md`）。このセクションに開いている残件はない。

---

## 5. dispatch cluster

`wrap.t` / `dispatching.t` / `qualified.t` / `assign.t` / `hyper.t`（#4067 前後・420/420、
test 407 builtin-MRO all-candidates dispatch / test 408 atomicint クロージャ内リセット書き戻し）
は完了済み。詳細は `news/2026-06.md` / `news/2026-07.md`。演算子 sugar が dispatch へ落ちるまでの
surface はすべて whitelist 済みで、このクラスタに残件はない。

---

## 6. 並行・非同期（S17）

大半は完了した（semaphore.t、then.t、scheduler/basic.t、supply/migrate.t、supply/stable.t、
S17-promise/start.t、S07-hyperrace/basics.t、S17-lowlevel/cas-int.t、S17-lowlevel/lock.t —
詳細は `news/2026-06.md` / `news/2026-07.md`）。残るのは次の 1 ファイル。

### 6.2 `S17-supply/syntax.t`

- **現状**: 88/90（test 75, 53 が失敗。**test 90 は 2026-07-08 に修正済み**）。
  根本原因分析は `TODO_roast/S17.md` に記録済み:
  - **test 90 (FIXED)**: live `Supply.grep`/`.map` 転送タップ + react loop の
    supplier pre-drain（emit-before-done 順序保証）+ グローバル emit 順マージ
    （`emit_seqs` で sibling `whenever $s.grep(...)` を emit 順に interleave）で
    決定的に通過。pin=`t/supply-live-grep-map-react-order.t`。
  - test 75: nested `whenever`（on-demand supply が `whenever Supply.interval {}` の
    内側にある場合）の `closing => { $closed++ }` が cross-thread plain-scalar
    writeback（worker→main lexical）で伝播しない別軸（§4.1/§4.2 thread-capture gap、
    lock.t 10-24 と同じ）。cross-thread-lexical campaign 待ち。
  - test 53: `supply {}` の whenever 相互排他順序 race（full-file parallel load 下のみ
    flaky、単体では 6/6 pass）。
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

- `S32-str/format.t` — `RakuAST` が無い限り最後まで通しにくい（§1.1 参照）。

---

## 8. 今のおすすめ着手順

「次に何をやるか」を 1 本だけ選ぶなら、順番はこう見るのが妥当。

1. **第一級コンテナ campaign**（§3）— `docs/container-identity.md` に沿って
   multislice hash 側の slot identity を前に進める。
   これは腰を据えた基盤工事で、個々のテストを直接潰すより効果が大きい。
   **進捗（2026-07-08）**: whole-container 代入の container-identity（`splice.t`）と
   属性 container mixin（`S14-traits/attributes.t`・#4314）は完了・whitelist 済み
   （§3.1 参照）。残るは closure-captured shared-cell を list へ by-value 捕捉したときの
   スナップショット追従（return-writeback 系）と、属性 slot の完全な cell 化
   （`:=` 束縛・mixin accessor への rw 書込）。
2. **`S17-supply/syntax.t`**（§6.2）— test 75/90 は個別に深掘りが必要（hard case）。

whitelist を目標にしない §7 の項目は、mutsu 側の一般改善のついでに触れるのはよいが、
そのファイル単体を通すことを目的にしない。
