# SlotRef removal — design plan (cells-everywhere の最終課題)

> Prep for a dedicated PR series. The remaining `HashSlotRef` / `ArraySlotRef` /
> `DeferredHashAccess` machinery is the last `Value`-variant survivor of the
> pre-cell era. Read alongside `docs/container-identity.md`
> (Phase 2 Stage 2「残スライスのマップ」1-4) and `docs/element-element-bind-plan.md`.
>
> This is the **「設計を要する cells-everywhere 課題」** flagged in the 2026-06-13
> session note. The variants are concentrated in the hottest write-autoviv files
> (`vm_var_assign_ops.rs` 28 / `value/mod.rs` 21 / `vm_var_index_ops.rs` 19), and a
> naive "turn every slot into a cell" approach **will regress perf and identity**.
> The whole point of this doc is to settle the representation *before* touching
> code so the implementation PR is *apply*, not *re-derive*.

---

## 0. Definition of done

すべての `SlotRef` 系 variant を `Value` enum から削除し、以下を満たす:

1. `git grep 'SlotRef\|DeferredHashAccess' src/` がゼロ（enum 定義・serde・display・
   introspect の全 arm 込み）。
2. `make test` 全緑、**release roast の main-vs-branch 比較で回帰ゼロ**（要素変更は
   release でしか出ない leak がある — #2898 の轍）。
3. `roast/S03-binding/nested.t` を含む binding 系が現状維持以上。
4. `int.t` の wall-clock が perf 崖を引かない（write-autoviv は最ホット経路）。

---

## 1. 現状の地図: SlotRef が担う **2 つの役割**

`SlotRef` 系は歴史的に 1 つの variant で 2 つの異なる責務を兼ねている。これが
撤去を難しくしている根本原因。役割を分離すれば、各々に正しい後継表現がある。

### 役割 A — write-autoviv カーソル（**transient・plain assignment**）

`%h<a><b> = 5` / `@a[3] = 9` の代入で、中間コンテナを autoviv しながら終端 slot を
指す一時ポインタ。**bind ではない**。VM スタック上を 1 命令だけ流れ、書き込み直後に
捨てられる。決して `Value` として永続化されない（永続化すれば「代入した要素は全部
`ContainerRef`」になる = cells-everywhere 崖）。

- 生成: `hash_autovivify`（`value/mod.rs:2856`）。
- 消費: `exec_index_autovivify_op`（`vm_var_index_ops.rs:75-152`）が
  push → 次の index-assign op が `hash_slot_read`/`hash_slot_write` で読み書き。
- 終端 array は既に `array_slot_ref` が `ContainerRef` を返すが、これは **bind 専用**で
  autoviv 経路（`exec_index_autovivify_op` の array arm, vm_var_index_ops:120-127）は
  `array_slot_ref(idx, false)` を **autoviv カーソル**として使い回している点に注意。

### 役割 B — bind alias（**persistent・`:=` 束縛**）

`my $x := %h<a>` で変数 `$x` が要素を別名参照し続ける。書きが COW を跨いで届く必要が
ある。**これは既に Stage 2 で `ContainerRef` cell へ移行済み**（既存 scalar leaf）。
残るのは:

- 生成 `vm_var_assign_ops.rs:4256/4291` — `:=` の source 変数 env slot に SlotRef を
  書く旧経路。**bind の cell 経路（slice 3/4 で landed）と二重化**している疑いが濃い。
  まず「この経路は現役か / cell 経路に subsume 済みか」を確定するのが先決
  （§4 slice 1）。
- 欠落 key の bind: `hash_slot_ref` の None arm（`value/mod.rs:2918`）+
  `hash_slot_ref_lazy`（2871）+ `DeferredHashAccess`（vm_var_index_ops:214）。
  エントリが無いので cell 化できない（`:exists` を汚さない遅延が要件）。**phantom
  entry 相当の設計が要る** — 最難・最後に回す。

### 役割の対応表

| 役割 | 現表現 | 生成サイト | 後継表現 | 難度 |
|------|--------|-----------|---------|------|
| A. write-autoviv カーソル | `HashSlotRef` (transient) | `hash_autovivify` 2856 | **非 Value の WriteCursor**（§3） | 中 |
| A. array autoviv カーソル | `array_slot_ref(_,false)` | mod.rs array arm | 同上に統合 | 中 |
| B. bind（既存 leaf） | `ContainerRef` cell | slice 3/4 (landed) | （済） | — |
| B. bind source slot | `HashSlotRef`/`ArraySlotRef` | 4256/4291 | cell 経路に subsume or 削除 | 低〜中 |
| B. bind（欠落 key） | `hash_slot_ref_lazy`+`DeferredHashAccess` | 2871/2918/214 | phantom-entry cell（§3.3） | 高 |

---

## 2. なぜ naive な cell 化が失敗するか（core hazard）

「全 slot を `ContainerRef` cell にすれば SlotRef は消える」は **誤り**。理由:

1. **Perf 崖**: write-autoviv は最ホット経路（`%h{$k} = $v` のループ）。終端を毎回
   `Arc::new(Mutex::new(..))` で包むと、代入のたびに heap alloc + lock 取得が乗る。
   `int.t` wall-clock が即座に劣化する。
2. **Identity / eqv 汚染**: cell 化された要素は `eqv` / `===` / `.WHICH` / hash-key
   encoding / serde で **decont を全経路に強制**する（"deref everywhere" 回帰）。プレーン
   代入した `%h<a>` がユーザーから見て cell であってはならない。
3. **Assignment-creates-new-container 違反**: `%new = %hash` はスナップショットを作る
   （S03-binding/hashes.t 30）。終端が cell だと alias が漏れる。

→ **不変条件**: cell 昇格は *bind が実際に起きたとき* だけ。plain assignment の
autoviv カーソルは cell を残してはならない。これが §3 の設計を貫く制約。

---

## 3. 設計

> ### ⚠️ 実装で判明した訂正（2026-06-13, slice 1/2 着手後）
>
> 当初の §3.1「役割 A = plain write-autoviv カーソル」前提は **コードと食い違っていた**。
> slice 1/2 の実コード調査で確定した事実:
>
> 1. **plain `%h<a><b> = 5` は SlotRef を一切作らない** — named target の deep nested
>    assign は `OpCode::IndexAssignDeepNested`（→ `exec_index_assign_deep_nested_op`、
>    独自の `assign_into_nested_container` で autoviv）を通り、`hash_autovivify` も
>    SlotRef も経由しない。つまり「役割 A の write-autoviv カーソル」は **実在しない**。
> 2. **非 lazy `OpCode::IndexAutovivify` は `#[allow(dead_code)]`・compiler 発行ゼロ**＝デッド。
>    その handler `exec_index_autovivify_op` は lazy bind op の fallback としてのみ生きる。
> 3. **残る SlotRef 生成は全て `:=` bind 周辺 + 数個の niche autoviv**:
>    - `HashSlotRef`: `hash_autovivify`（junction-key bind の 97 / autoviv-op hash arm
>      113・140 / **`is raw` reduce の lvalue-read autoviv** 811-868）＋ `hash_slot_ref`
>      None arm（欠落 key terminal bind 2918）＋ `hash_slot_ref_lazy`（lazy bind）。
>    - `DeferredHashAccess`: 欠落 key の lazy nested bind（vm_var_index_ops:214）。
>    - **`ArraySlotRef`: slice 1 が最後の生成サイト（4291）を cell 化したことで生成ゼロ
>      ＝完全デッド** → slice 2 で variant 丸ごと削除済み。
> 4. **「非 Value WriteCursor」案は不採用**: `exec_index_autovivify_op` が返す cursor は
>    連続する `IndexAutovivifyLazy` op の **間**でスタックに載る（op 境界を跨ぐ）ので、
>    単一 op 内 thread には収まらず op 融合が要る。だが残った生成はほぼ全て bind 経路
>    なので、**cell 化（slice 3/4 と同型）で個別に枯らす方が正攻法**。よって以降の
>    スライスは「§3.1 WriteCursor」ではなく「bind 生成サイトの cell 化」で進める。
>
> 下の §3.1 は当初案の記録として残すが、**採用しない**（§4 の更新スライスを見よ）。

### 3.1 役割 A: write-autoviv カーソルを非 Value 化（WriteCursor）〔当初案・不採用〕

`HashSlotRef`/`ArraySlotRef` を「VM スタックを流れる `Value`」として使うのをやめ、
**index-assign opcode の中で `(Arc<HashData>, String)` / `(Arc<ArrayData>, usize)` を
直接 thread する**か、`Value` ではない専用 enum を VM ローカルに持つ。

候補 2 案:

- **案 A1（推奨）— opcode 内 thread**: `exec_index_autovivify_op` +
  後続 index-assign op を**融合**し、autoviv の中間結果を `Value` に箱詰めせず VM
  メソッドの戻り値（Rust の `(Arc, key)` tuple か小さな `enum WriteCursor`）として
  受け渡す。`Value::HashSlotRef` variant は消える。autoviv 連鎖（`%h<a><b><c> = 5`）は
  カーソルを段階的に深掘りするループとして書ける（中間は `hash_autovivify` の
  inner-Arc を辿る、終端だけ `hash_insert_through`）。
  - 利点: cell ゼロ・alloc ゼロ・identity 汚染ゼロ。perf 中立（むしろ Arc clone 減）。
  - コスト: `exec_index_autovivify_op` / `exec_index_assign_*` のリファクタ。compiler が
    autoviv→assign を別 op で出している場合は op の融合 or `WriteCursor` を
    VM フィールドに退避する必要（スタックには載せない）。
- **案 A2 — `Value` 内に残すが非公開化**: 変更最小だが variant が残るので DOD 未達。不採用。

**決定ポイント（実装前に確定）**: compiler が `%h<a><b> = 5` をどの op 列に落とすか。
`--dump-ast` ではなく bytecode dump で確認し、autoviv カーソルが op 境界を跨いで
スタックに残るか（= 融合が要るか）を判定する。`grep IndexAutovivify src/compiler/`。

### 3.2 役割 B: bind source slot（4256/4291）の整理

slice 3/4 で BOUND_*_REF sentinel を cell へ移したので、4256/4291 の SlotRef-into-env
経路が **まだ呼ばれるか**を実測する（`MUTSU_TRACE` か一時 `dbg!`、または到達性 grep）。
2 つのシナリオ:

- **(i) cell 経路に subsume 済み** → 4256/4291 はデッドコード。削除するだけ（slice 1）。
- **(ii) まだ現役**（特定の bind 形がここを通る）→ slice 3/4 と同じく source 変数へ
  `ContainerRef` cell を書く形へ置換。`bind_cell` pre-read（既存 cell 再利用）の
  ヘルパが既にあるので流用。

どちらでも `hash_slot_read`/`array_slot_read`/`hash_slot_write`/`array_slot_write` の
consumer（4303-4318・4987-5004・5750-5776）は順に消える。

### 3.3 役割 B: 欠落 key の lazy bind（最難・phantom entry）

`my $b := %h<a><b>` で `%h<a>` も `<b>` も未存在のとき、`:exists` を汚さずに束縛したい。
現状は `hash_slot_ref_lazy` + `DeferredHashAccess` で「書くまでエントリを作らない」を
実現。cell 化の障害は「指すべき物理 slot がまだ無い」こと。

設計候補:

- **phantom-entry cell**: 欠落 key に対し「親 Arc + key path」を保持する **lazy cell**
  を作り、最初の write で初めて親に物理エントリを materialize してから cell を
  そこへ結びつける。`:exists` には見えない（materialize 前は親 map に key が無い）。
  read は Any/Nil を返す。これは実質 `DeferredHashAccess` の cell 版だが、**cell
  identity で alias する**点が違う（COW staleness が消える）。
- **代替**: 欠落 key bind は頻度が低いので、最後まで `DeferredHashAccess` 1 variant
  だけ残し、A/B の他経路を全部消してから単独で対処する判断もあり。DOD は「全 SlotRef
  削除」なので最終的には phantom-entry が要る。

⚠️ この slice は `roast/S03-binding/nested.t 11-12`（`<key>()` mid-path）とも絡む。
element-element-bind-plan.md と重複領域があるので、その slice を先に着地させてから。

---

## 4. 段階スライス（各 CI gate・big-bang 回避）

> 順序の原則: **生成サイトを 1 つずつ枯らし、最後に variant と consumer を一括削除**
> （container-identity.md 残マップ-4 と同じ畳み方）。read/write helper は生成が
> 消えれば自然に dead になる。

1. **slice 1 — computed-target `:=` element bind の cell 化【DONE, PR #2974】**
   `exec_index_assign_generic_op`(IndexAssignGeneric) の `HashSlotRef`/`ArraySlotRef`-
   into-env（4256/4291）を共有 `ContainerRef` cell へ。到達性実測で **現役 かつ バグ**
   （write-through alias 非伝播）と判明し cell 化でバグ同時修正。pin `t/generic-bind-cell.t`。

2. **slice 2 — デッド化した `ArraySlotRef` variant の全削除【DONE】**
   slice 1 が `ArraySlotRef` の最後の生成サイト（4291）を消したため variant は生成ゼロ
   ＝完全デッド（`array_slot_ref` メソッドは `ContainerRef` cell を返す別物）。variant 定義・
   `array_slot_read/write` メソッド・全 consumer arm（types/utils/display/introspect/serde/
   vm_env_helpers/vm_call_func/vm_var_index/vm_var_assign）を削除。`cargo build` が網羅
   チェックリスト。挙動不変（bind 系 t/ + roast S03-binding 全 PASS）。
   〔当初の「write-autoviv カーソル非 Value 化」は §3 冒頭の訂正どおり実在せず不採用〕

3. **slice 3 — 欠落 key の terminal bind（`hash_slot_ref` None arm 2918）**
   §3.3 phantom-entry の前哨。エントリ有り leaf は既に cell なので、None arm だけが
   対象。lazy でない terminal bind を phantom-entry cell へ。

4. **slice 4 — `hash_slot_ref_lazy` + `DeferredHashAccess` 全廃（§3.3 本体）**
   最難。element-element-bind-plan.md の nested.t 11-12 着地後に。

5. **slice 5 — `HashSlotRef` 生成の残（junction-key bind 97 / `is raw` reduce の
   lvalue-read autoviv 811-868）を cell 化 → variant・helper 一括削除**
   全生成が枯れたら `HashSlotRef`/`DeferredHashAccess` を enum から削除。
   連動削除: `hash_slot_read/write`・`deferred_hash_read/write`・`hash_slot_ref_lazy`・
   serde arm・display arm・truthy/isa arm（types.rs）・introspect・vm_env_helpers の
   skip arm・vm_var_assign_ops の consumer 群。`cargo build` のコンパイルエラーが網羅的
   チェックリストになる。

---

## 5. Hazard 監査ポイント（要素変更は release でしか出ない leak がある）

各 slice で**手動監査必須**（過去の轍）:

- **unsafe Arc cast**: `Arc::as_ptr(arc) as *mut HashData/ArrayData` の周囲で
  `&HashMap`/`&Vec` の immutable borrow が生きていないこと。WriteCursor 化で借用が
  op 境界を跨ぐ新しい経路が生まれていないか。
- **Arc identity / `ptr_eq`**: bind alias の同一性判定（`exec_container_eq_indexed_op`
  系）が cell `Arc::ptr_eq` を見るので、新カーソルが偽の identity を作らないこと。
- **iterator 共有**: `resolve_hash_for_iteration` / `resolve_array_entry` の decont が
  WriteCursor を **読み経路に漏らさない**こと（カーソルは write 専用）。
- **env writeback**: `env_dirty` フラグ。bind source slot を env に書く経路を消すと
  flush 挙動が変わりうる（dual-store）。`MUTSU_VM_STATS` の `env_flushes` で確認。
- **COW staleness**: phantom-entry が materialize 後に正しい物理 Arc を掴むこと。深い
  `%g<outer><inner>` で outer が COW clone された後の write が source に届くか
  （nested.t の deep bind が回帰カナリア）。
- **serde / display / `.raku` / `.gist`**: variant 削除で arm が消えるが、削除前に
  「これらの経路に SlotRef が漏れていない」前提が崩れていないか（漏れていたら隠れ
  バグの露呈なので fix-forward）。

---

## 6. 検証プロトコル（各 slice）

1. `cargo build`（debug）→ `prove -e target/debug/mutsu roast/S03-binding/nested.t` +
   touch した binding 系。
2. counter 系（`MUTSU_VM_STATS=1`）は debug で確定（perf 崖の早期検知）。
3. `make test` 全緑（`tmp/make-test.log` を Grep）。
4. **release roast の main-vs-branch 比較**（必須・要素変更の leak は release のみ）。
5. perf 最終測定は **release で `int.t` wall-clock**（slice 2 必須）。
6. PR → auto-merge → background CI watch（CLAUDE.md PR workflow）。

---

## 7. 参照

- `docs/container-identity.md` — 第一級コンテナ台帳（Phase 2 Stage 2 残マップ 1-4 が
  この doc の上位）。
- `docs/element-element-bind-plan.md` — element-element bind（nested.t 11-12/32-37）。
  slice 3/4 の前提。
- `docs/is-rw-shared-cell-plan.md` — `is rw` 共有 cell。同じ ContainerRef 基盤。
- 過去スライス: #2964（deepmap）/#2966（中間 SlotRef 撤去）/slice 3/4
  （BOUND_*_REF sentinel 全廃）/#2902-#2925（要素 cell 基盤）。
