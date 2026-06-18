# env↔locals コンテナ coherence — Slice F の真の本丸（設計）

> **Status:** DESIGN (2026-06-18). コードなし。`docs/vm-single-store.md`（二重ストア統合）と
> `docs/container-identity.md`（第一級コンテナ）が **Slice F で収束する** と確定した
> 2026-06-18 の結論を受け、その収束点＝「`env` と `locals` が同一コンテナで乖離しないこと」
> をどう保証するかを設計から問い直す。Slice E（closure upvalue・#3245/#3247）で single-store の
> *独立* 前提は片付いたので、残るのはこの coherence のみ。

---

## 0. TL;DR

Slice F（`env_dirty` / `sync_locals_from_env` / `ensure_locals_synced` / `saved_env_dirty` の削除＝
`locals` を真の単一権威化）の**唯一残る前提**は:

> **同一の論理コンテナ（`:=` cell を含む Array/Hash）について、`env` と `locals` が
> *別の outer `Arc`* を持って構造的に乖離してはならない。**

第一級コンテナ campaign（`docs/container-identity.md` Phase 2）の `ContainerRef` cell は
**1 ストア内**の COW 跨ぎ leaf 生存を解決済みだが、**dual-store の env↔locals 乖離は別レイヤ**として残る。
これが `pairs`/`slip` carrier-drop を `t/element-bind-cell.t` で壊す根本（cell 機構があっても残る）。
本ドキュメントはこの乖離の正確な地図・なぜ cell だけでは閉じないか・設計選択肢・段階スライス・hazard を残す。

---

## 1. 二つのストアと、コンテナが *通常は* coherent に保たれる仕組み

| store | 形 | 役割 |
|---|---|---|
| `Interpreter::locals` | `Vec<Value>`、slot index | hot path、`GetLocal`/`SetLocal` |
| `Interpreter::env` | `Env` = `Arc<HashMap<Symbol,Value>>`（COW・scoped overlay 連鎖） | 名前キーの全消費者 |

同期プリミティブ（`src/vm/vm_env_helpers.rs`）:
- **forward**（locals→env）= `flush_local_to_env(code, idx)`: `set_env_with_main_alias(name, self.locals[idx].clone())`。
  `Value::clone` は **Array/Hash では `Arc` bump ＝ outer Arc を共有**。`needs_env_sync[idx]` ＋
  `simple_locals||bare_param` でゲート。
- **reverse**（env→locals）= `sync_locals_from_env(code)`: 各 local 名で `self.locals[i] = env.get(name).clone()`
  （同じく Arc bump ＝共有）。`HashSlotRef` / `!attr` は skip。`env_dirty` ゲートで `ensure_locals_synced` から呼ばれる。

**∴ 通常、slot を持つコンテナは env と locals で *同一 outer `Arc`* を共有する**（clone はすべて Arc bump）。
この共有が崩れる＝乖離が起きるのは次の §2。

---

## 2. 乖離はいつ起きるか（outer `Arc` が env と locals で別物になる瞬間）

`Arc::make_mut`（`Env::cow_mut` / コンテナ要素の in-place 変異）は **strong_count>1 のとき deep copy** して
**新しい Arc** を作る。これが乖離の源:

1. **片側 store だけが make_mut**: `env` 側のコンテナを `env_mut()` 経由で変異すると（carrier の by-name write、
   scoped overlay への insert）、env の outer Arc が複製され locals と別物になる。逆に locals 側を直接変異しても同様。
   次の reverse pull（`sync_locals_from_env`）が走れば locals が env の新 Arc を取り直して**再収束**する——
   これが `env_dirty` の役目。**`env_dirty` を落とす＝この再収束を飛ばす**＝乖離が残る。
2. **carrier が fresh env を builds**: EVAL/pairs/slip carrier は scoped overlay や flatten 上で実行され、
   コンテナが COW-detach されうる。carrier-return の `writeback_carrier_writes` は **scalar のみ** reconcile し
   **コンテナは決して上書きしない**（COW-detached env が内部 `:=` cell を壊す S03-binding/nested.t hazard ゆえ・#3227）。
   つまりコンテナは barrier pull（env_dirty）に委ねられている。drop するとコンテナ乖離が残る。
3. **scoped_child / flattened**: 深い再帰で chain が `MAX_OVERLAY_DEPTH` を超えると parent を flatten（`env.rs`）。
   flatten 自体は Arc bump だが、その後の overlay write で make_mut が走ると同上。

**leaf cell は乖離しない**: `ContainerRef(Arc<Mutex<Value>>)` は COW deep-copy でも **`Arc<Mutex>` がクローン間で共有**
（Arc bump）されるため、leaf cell の identity は任意深度の経路を跨いで生存する（container-identity Phase 2 の核心）。
**乖離するのは outer container の *構造*** — 「どの index にどの cell が居るか・長さ・非 cell 要素」。

---

## 3. なぜ cell（Phase 2）だけでは閉じないか — 具体ケース

`t/element-bind-cell.t` の deep bind（テスト 39-44）:

```raku
my $struct = [ "ignored", { key => { subkey => [10, 42] } } ];
my $abbrev := $struct[1]<key><subkey>[1];   # leaf 要素を cell 昇格、$abbrev は同 cell
$struct[1]<key><subkey>[1] = 43;            # 要素 write → cell → $abbrev も 43（COW 跨ぎ OK）
$abbrev = 44;                                # cell write → $struct の leaf も 44
```

これは **単一ストア内**では cell 共有で成立する（#2922/#2925、nested.t 43/43）。
しかし `pairs`/`slip` carrier が `env_dirty` を drop すると壊れる（memory 第13/14 セッション・テスト 9/28/41/44/46/47）:

- carrier 内で `$abbrev = 44`（cell write）は **leaf cell に届く**（cell は共有・乖離しない）。
- だが carrier 中に `env` 側の `$struct` outer Arc が COW-detach されると、carrier-return で **コンテナは
  writeback されず**（§2-2）、かつ drop で **barrier pull も飛ぶ**ため、`locals` 側 `$struct` は
  **古い outer Arc**（cell を別 index に持つ／別構造）を読み続ける。
- 結果、`$struct[1]<key><subkey>[1]` を locals 経由で読むと **stale な経路**を辿り `Nil` / 旧値になる
  （leaf cell は生きているのに、そこへ至る outer 構造が env と locals で食い違う）。

**∴ cell は leaf identity を保証するが、env↔locals 間の *outer 構造の同一性* は保証しない。**
これが「別レイヤ」の意味であり、Slice F = この outer 同一性の保証。

---

## 4. 設計選択肢

### (A) コンテナを env でも cell（`ContainerRef`）として持つ＝**env↔locals が outer cell を共有**（推奨の軸）

instance attr が Phase 3 で `Arc<RwLock<HashMap>>` cell 一本化したのと同型。slot を持つ Array/Hash を
**`ContainerRef` cell に包み、env と locals が *同じ cell* を保持**する。すると:
- forward/reverse とも cell の Arc bump ＝ outer 構造も常に共有（make_mut で片側が detach しても、両者が同 cell を
  指すので detach 自体が起きない／起きても cell 内 1 箇所）。
- `env_dirty`/`sync_locals_from_env` の **再収束が不要**になる（乖離が構造的に発生しない）→ Slice F が解禁。
- carrier-drop も安全化（コンテナは cell 共有なので pull に依存しない）→ `pairs`/`slip` 一般化が同時に成立。

**コスト/hazard**:
- **perf 崖リスク**: 全コンテナを cell 化すると、値 op（算術 fold・iteration・native method の raw-items 読み）の
  消費面が ContainerRef を毎回 decont する必要＝"deref everywhere"。**escape-aware が必須**（捕捉/エイリアス/`:=`/
  `is rw`/`.VAR` されない裸ローカルは cell 化しない＝#2746 の轍を踏まない）。Phase 2 の単一 decont チョークポイント
  （`docs/container-identity.md` §3）が前提。
- **leak 硬化**: slice/`.kv`/`.pairs`/`.raku`/native method の raw-items 読みで cell が漏れない監査（Phase 2 既知課題）。
- **書込チョークポイント**: 配列/ハッシュ要素 write は既に `assign_element_slot`/`hash_insert_through` に集約済み
  （Phase 2 Stage 0）。outer cell 化はその上に乗る。

### (B) coherence invariant ＝「slot コンテナの outer Arc を片側だけ make_mut しない」

env と locals が常に同 Arc を指すよう、**どちらかを変異する直前に必ずもう一方を同 Arc に再リンク**する不変条件。
forward/reverse を「Arc 同一性を壊さない write-through」に限定。**(A) の弱い版**だが、make_mut が
strong_count>1 で必ず detach する以上「片側だけ変異」を完全に防ぐのは難しく、結局 cell 一本化（A）に収束する。
記録用に残すが本命ではない。

### (C) carrier がコンテナ write も精密ログ＝carrier-drop を安全化（局所対症）

`pairs`/`slip` carrier がコンテナ変異も `carrier_writes` に記録し、carrier-return で cell-aware reconcile。
memory 第14セッションで **deep `:=` cell coherence を壊すと実証済み**（write-logging 完全化しても outer 乖離は残る）。
= 対症療法で根本（outer 同一性）を解かない。**不採用**（記録のみ）。

---

## 5. 推奨＝(A) を Phase 2 完了の上に段階導入

> 北極星（PLAN.md §🟣）= 最もクリーン×最速。進捗メトリクスは **削除した重複/特例メカニズムの数**。
> (A) は `env_dirty` 系 4 機構 ＋ carrier コンテナ reconcile 特例 ＋ pairs/slip carrier-drop 抑止を一掃する勝ち筋。

big-bang 不可。Phase 2（要素 cell）が leaf を解いたのと同じく、**outer コンテナ cell 化も
「チョークポイント先行（挙動不変）→ escape-aware 昇格 → 機構削除」** の順:

- **Stage 0 — outer 読み/書きチョークポイント棚卸し（挙動不変）**: slot コンテナの read（GetLocal/GetGlobal/
  GetArrayVar/GetHashVar/Index）が単一 decont を通り、write が単一 write-through を通ることを確認・補完。
  Phase 0/2 Stage 0 の延長。roast 完全一致が合格条件。
  - **[~] 棚卸し結果（2026-06-18）**: **read 側は既に単一 decont チョークポイント化済み**＝想定より良好。
    `GetLocal`/`GetGlobal`/`GetArrayVar`/`GetHashVar` は全て `into_deref()`（`value/mod.rs`）で top-level
    `ContainerRef` を剥がしてから push（`vm_var_assign_ops.rs:5388`・`vm.rs:1347/1439/1521`）、要素 read は
    `resolve_array_entry`/`resolve_hash_entry`（`vm_var_ops.rs`）で cell を decont。**outer cell の値消費面
    （open-q#1）は slot-read 境界で既に閉じている**＝Stage 1 の主リスクが想定より小さい。write 側は要素 write が
    `assign_element_slot`/`hash_insert_through`（`value/mod.rs`）に集約済み、whole-container write-through も
    `SetLocal` は既存 `ContainerRef` を維持（`vm_var_assign_ops.rs:5543`）。
  - **[x] write チョークポイント gap 1 件を補完 — `our %g := %h`（global hash bind）の readonly 誤発火**:
    `our` の `:=` 束縛は var を readonly マーク（bind signal）するが、`SetGlobal` がそれを真の RO と誤認し
    "Cannot assign to a readonly variable (%g)" で bind 自体が abort（`my %g := %h` は動作・global hash 特有・
    array global は別機構で無害）。修正＝bound container vardecl の `SetGlobal` 直前に `MarkBindContext` 再 emit
    （`compiler/stmt.rs`）＋ `SetGlobal` が bind context のとき readonly チェックをスキップ（`vm.rs`）。bind 後は
    var が readonly マークを保持＝後続の `%g<k>=v` が named index-assign の `is_bound_hash_var` 経路で in-place
    変異し双方向 alias 成立（local と同型）。`t/our-hash-bind.t`（9）。
- **Stage 1 — escape-aware に outer コンテナを cell 化**: 「捕捉/`:=`/`is rw`/`.VAR`/cross-frame 変異」される
  slot コンテナのみ outer `ContainerRef` 昇格し env↔locals が同 cell を共有。裸ローカルは従来どおり Arc 共有
  （perf 崖回避）。escape 解析（#2758）の判定を outer コンテナへ拡張。
- **Stage 2 — `env_dirty` 依存の除去**: 乖離が構造的に消えたコンテナについて reverse pull を不要化。まず計測
  （`MUTSU_VM_STATS` reverse-sync の container stale が 0 になることを確認）。
- **Stage 3 — Slice F 本体 ＋ pairs/slip carrier-drop 一般化**: `env_dirty`/`ensure_locals_synced`/
  `sync_locals_from_env`/`saved_env_dirty` 削除（`docs/vm-single-store.md` Slice F）。同時に `pairs`/`slip`
  carrier-drop を有効化（outer cell 共有で安全）。

各 Stage は **`t/element-bind-cell.t`（54）/ `t/array-bind-cell.t` / `t/hash-bind-cell.t` / nested.t 43 ＋
release roast の main-vs-branch 比較 ＋ int.t/method-call の wall-clock**（#2746 教訓: perf 回帰は make test で
検出不能・CI release roast の timeout でのみ顕在化）で固める。

---

## 6. 着手前に解く open questions

1. **outer コンテナ cell の decont 消費面の網羅**: 値 op で ContainerRef-outer が漏れる経路の完全棚卸し。
   Phase 2 の leaf cell より広い（outer は iteration/slice/native method が直接 items を舐める）。
2. **escape 判定の outer 拡張**: 既存 `captured_mutated_locals`/`needs_cell_locals`（scalar 中心）を Array/Hash
   ローカルへ広げる。`@a` を closure 捕捉＋変異する頻出ケースの perf を timed 確認。
3. **scoped overlay chain との相互作用**: outer cell が parent tier に居るとき scoped_child/flatten が cell を
   どう扱うか（cell は Arc bump で生存するはずだが flatten の merged map で要確認）。
4. **cross-thread**: `clone_for_thread` / `shared_vars` と outer cell（`Arc<Mutex>`）の整合（Track C と重なる）。
5. **`env_dirty` の非コンテナ依存**: Slice B で判明した「`:=` bind 等の暗黙 reconcile 依存」（scalar 側）が
   outer cell 化後も残るか。残るなら Slice F は scalar 側の精密化（Slice C/C′ の延長）も要する。

---

## 7. 参照

- `docs/vm-single-store.md` — 二重ストア統合の forward plan（Slice A–G）。本書は Slice F の前提条件の設計。
- `docs/vm-dual-store.md` — 機構マップと撤回試行のアーカイブ。
- `docs/container-identity.md` — 第一級コンテナ実装台帳（Phase 0/1/2/3）。outer cell 化は Phase 2 の延長。
- `docs/slotref-removal-plan.md` — SlotRef 撤去（leaf cell 化の周辺）。
- 失敗の実証: `t/element-bind-cell.t`（pairs/slip carrier-drop で 9/28/41/44/46/47 が壊れる）。
