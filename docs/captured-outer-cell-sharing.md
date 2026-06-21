# Captured-outer lexical cell 共有 — 実装プラン（Sub-slice 1b+ / env_dirty 削除への substrate）

> **Status:** SLICE 1 DONE（2026-06-21・第41セッション）。§6 の第1スライス（named-sub 捕捉
> scalar の decl-site cell 化）を実装。`t/captured-outer-cell-sharing.t`(12) と
> `t/multi-frame-accumulation-coherence.t`(10) が **blanket reconcile ON/OFF 両方で PASS**＝この
> サーフェスは cell 共有で解けた。次＝§7 スライス2（捕捉 container `@`/`%`）。
> 関連: [docs/env-locals-coherence.md](env-locals-coherence.md)（§7.3 = env_dirty 削除の壁）/
> [docs/container-identity.md](container-identity.md)（cell インフラ）/ PLAN.md §1-A・§2-C・§2-E。
>
> **★設計判断（第41セッション）— boxing は blanket reconcile と相互排他・toggle gated**:
> cell boxing と blanket reconcile は **どちらか一方のみ active**（`box_decl_local_cell` は
> `cell_boxing_active()` = `MUTSU_NO_BLANKET_RECONCILE` 時のみ発火）。理由＝reconcile ON のまま boxing
> すると、reconcile が一部 carrier（`lives-ok {…}` 等）で **落とす** captured-outer write を cell が
> 正しく伝播 → その伝播が **無関係な潜在バグを露出**（例: `&foo = &foo` の self-ref 型チェック欠落＝
> roast `S06-signature/code.t` test 8 が main では write-loss で偶然 PASS していた）。∴ デフォルト
> （shipping/CI）は reconcile が coherence を担い main と byte-identical、toggle 時のみ boxing を
> exercise（pin が named-sub accumulation surface を担保）。env_dirty 削除時に boxing を恒久 ON 化＋
> 露出バグを順次修正する。`let`/`temp` の cell-aware restore（`exec_let_save_op` deref +
> `restore_let_value` write-through）も同 toggle gated。
>
> **実装メモ（第41セッション）**: 検出＝compile-time。`CompiledCode.named_sub_captures:
> Vec<(free_var_writes, needs_cell_named_sub_free)>` に直接子 named sub の **write 集合**のみ畳む
> （`compile_sub_body` が cf 構築後 push）。`compute_free_vars` が own local への named-sub write を
> `needs_cell_named_sub` に、非 own を `needs_cell_named_sub_free`（祖先へ bubble）に計上。**closure 駆動の
> `needs_cell_locals` とは完全分離**＝closure は creation op で精密 box されるので、それを decl-site で
> 流用すると無関係な同名 local（同名 `my` は同一 slot 共有）を over-box し `let`-restore 等を壊す
> （当初これで roast `let.t` 4/9/12 回帰＝修正済）。ボックス化＝**decl-site**だが捕捉 scalar は
> `needs_env_sync`（non-simple）のため `exec_set_local_op` の **fast/slow 両 inner path をラップする
> 外側**で box（fast path 内だけだと non-simple var に効かない＝当初の誤り）。検証トグル＝
> `MUTSU_NO_BLANKET_RECONCILE`、6 つの env_dirty-gated reconcile を `blanket_reconcile_if_dirty(code)` に
> 集約（将来の env_dirty 削除を 1 メソッドの空洞化で行えるように）。教訓: ①top-level/bare-block の
> `my` は env(SetGlobal)行き＝slot box 不可だが、捕捉は **block-scoped か sub-body** の local で起きる
> ので問題なし。②local 名は **sigil なし**（"acc" 等）で `free_var_writes`/`locals` 一貫。
> ③同名 `my` は `alloc_local` で **同一 slot 共有**＝by-name box は scope を跨ぐので write 集合で精密化必須。

---

## 0. TL;DR — 配管は全て揃っている。欠落は「検出＋ボックス化」だけ

`env_dirty` を削除するには env と locals が乖離しないことが必要。第40セッションの調査で、**それを実現する
ランタイム配管は 100% 既存**と確定した。唯一の欠落は:

> **nested callee（closure **だけでなく** named sub）に捕捉＋変異される lexical を、
> owner の local slot と env エントリの両方で同一 `ContainerRef` cell にボックス化する**こと。

`:=` bind が既にこの「同一 Arc を slot + env + saved frames に置く」パターンを完全実装している（§3）ので、
それを **`:=` の無い暗黙キャプチャにも適用する**のが本作業。

---

## 1. なぜこれが env_dirty 削除の本丸か（§7.3 の壁の正体）

env_dirty の唯一残る load-bearing な役割は「multi-frame captured-outer accumulation」を支えること:

```raku
my $acc = 0;
sub bump-outer() { $acc = $acc + 10 }   # $acc は free var・SetGlobal で env を名前書き
sub via()        { bump-outer() }
via(); via();                            # raku=20・env_dirty 削除すると 10（accumulation 失敗）
```

**壁の機構（agent 調査で確定）**: named sub `bump-outer` は fresh compiler で別個にコンパイルされ
（`src/compiler/helpers_sub_body.rs:100`）、`$acc` を **free var として `GetGlobal`/`SetGlobal` で env 名前引き**する。
top-level の escape 解析（`src/opcode.rs:1576-1690` `compute_free_vars`）は **closure（`closure_compiled_codes`）しか
見ず named sub を見ない**ので `$acc` を `needs_cell` に flag しない → `$acc` は cell 化されず、env writeback +
（かつては reverse pull、今は）env_dirty-gated blanket reconcile に依存する。

`$acc` が **共有 cell** なら、`bump-outer` の `SetGlobal("$acc")` は cell を通して書き込み（§2-Q1）、top の
slot も同じ cell を読む（§2-Q2）→ **reconcile 不要**で multi-frame accumulation が成立 → env_dirty のこの役割が消える。

---

## 2. Make-or-break 配管（全て確認済 ✓）

| # | 経路 | 挙動 | 場所 |
|---|------|------|------|
| Q1 | **`SetGlobal`** | env が `ContainerRef` のとき **cell を通して書込**（Arc 置換しない）`arc.lock().clone_from(&val)` | `src/vm.rs:1999-2005` |
| Q2 | **`GetGlobal`** | `into_deref()` で cell の内側を読む（Arc 非 detach） | `src/vm.rs:1387` / `value/mod.rs:3070` |
| Q3 | **`$x++`/`$x+=n`（RMW）** | cell を通して RMW（atomic 対応・Track C） | `src/vm/vm_var_assign_ops.rs:1889-1902` |
| — | **`SetLocal`** | 既存 `ContainerRef` slot を **通して書込** `arc.lock().clone_from(&val)`（非 rebind 時） | `src/vm/vm_var_assign_ops.rs:5837-5852` |
| — | **`GetLocal`** | `into_deref()` で cell を読む | `value/mod.rs` `into_deref` |
| Q4 | **`flush_local_to_env`** | `locals[idx]` が `ContainerRef` なら同一 Arc を env へ伝播（`set_env_with_main_alias`） | `src/vm/vm_env_helpers.rs:587-603` |

**∴ 読み書き両側（Local/Global）が cell を透過し、slot→env 伝播も Arc を保つ。配管に欠落なし。**

---

## 3. 従うべき precedent — `:=` bind の cell 共有（既存・動作中）

`:=` bind は **まさに目標の「同一 cell を slot+env+saved frames に配置」を実装済**。これをテンプレにする。

### scalar `:=`（`my $a := $b`）— `src/vm/vm_var_assign_ops.rs:6633-6689

```rust
let container = if let Value::ContainerRef(ref arc) = val {
    Value::ContainerRef(arc.clone())          // 既に cell なら再利用（第3の alias が同 cell に join）
} else {
    val.clone().into_container_ref()           // 値を新 cell に包む
};
self.locals[idx] = container.clone();          // target slot
if let Some(source_idx) = code.locals.iter().rposition(|n| n == &resolved_source) {
    self.locals[source_idx] = container.clone(); // source slot も同 cell
    self.flush_local_to_env(code, source_idx);
}
self.env_mut().insert(resolved_source.clone(), container.clone()); // source env
for frame in self.call_frames.iter_mut().rev() {                   // ★saved frames へ伝播
    if frame.saved_env.contains_key(&resolved_source) { frame.saved_env.insert(...); }
    for (i, local_name) in code.locals.iter().enumerate() {
        if local_name == &resolved_source && i < frame.saved_locals.len() {
            frame.saved_locals[i] = container.clone();
        }
    }
}
self.set_env_with_main_alias(name, container.clone());  // target env
self.flush_local_to_env(code, idx);
```

### container `:=`（`my @a := @b`）— `src/vm/vm_var_assign_ops.rs:6534-6612`（同型・Array/Hash 用）

**重要な教訓（saved-frame 伝播）**: cell を slot+env に置くだけでなく **`call_frames` の `saved_env`/`saved_locals`
にも同 Arc を伝播**しないと、メソッド return（env 復元）で cell が stale 値に巻き戻る。新規ボックス化でも必須。

---

## 4. 現状の coverage と gap

### 既存の cell 化（done）
- `:=` bind scalar / array / hash（§3）= env↔locals 同一 cell・**完了**。
- **escaping closure に捕捉＋変異される scalar local**: `box_captured_lexicals`（`src/vm/vm_register_ops.rs:302-395`）が
  `needs_cell_locals`（escape 解析・`closure_escapes[i]==true`）の **scalar** を cell 化。

### gap（env_dirty 削除に必要な未カバー）
`box_captured_lexicals` の skip ロジック（vm_register_ops.rs:334-376）と escape 解析の限界:
- **(a) named sub に捕捉される scalar**（`$acc` ケース）= **未カバー**。`box_captured_lexicals` は closure
  （`MakeLambda`/`MakeBlockClosure`）でしか呼ばれず、named sub 登録（`RegisterSub`）では呼ばれない。
  escape 解析も named sub を `closure_compiled_codes` に含めない。← **multi-frame の壁・最初のスライス**
- **(b) 捕捉される container `@`/`%`**（closure / named sub 双方）= **未カバー**。box は `@`/`%`/`&` sigil を skip。
- **(c) type/where 制約付き scalar** = **意図的に skip**（cell write-through が制約再チェックを迂回するため・維持）。
- **(d) blanket reconcile を外して surface する他のケース**（scoped overlay / carrier 等）。

---

## 5. 検証法 — 「blanket reconcile を外して壊れるもの」が残サーフェス

`drain_and_reconcile_after_cached_call`（`src/vm/vm_env_helpers.rs`）の `if env_dirty { reconcile_locals_from_env_at_site }`
を一時的に外し（＋他の env_dirty consumer 6 サイト）、`make test` + roast を回す。**壊れる各テスト = まだ cell 共有
されていない captured-outer ケース**。これを 0 にしてから env_dirty を削除する。

> 第40セッションで実証済: 外すと `t/multi-frame-accumulation-coherence.t` の 4 subtest が壊れる（= (a) のケース）。
> consumer サイト一覧: `drain_and_reconcile_after_cached_call`（vm_env_helpers.rs:651）・`vm_call_func_ops.rs:605`・
> `vm_call_method_ops.rs:1253`・`vm_call_method_mut_ops.rs:1549`・`vm_closure_dispatch.rs:745`・`vm_helpers.rs:662`。

---

## 6. ★第1スライス（推奨）— named-sub 捕捉 scalar の cell 化（`$acc` ケース）

env_dirty 削除に向けた最初の検証可能な一手。multi-frame の壁を直接攻める。

### 6.1 検出（detection）— 2案

**案A（compile-time・推奨）**: `compute_free_vars`（opcode.rs:1576-1690）を拡張し、**同スコープで宣言された
named sub の `free_var_syms`/`free_var_writes` を、closure と同様に enclosing scope の capture 解析へ畳む**。
これで `$acc` が `needs_cell_locals` に乗る。
- 利点: 既存の `box_captured_lexicals` 機構に自然に乗る（escape 解析の一貫拡張）。
- 要調査: top-level の `compute_free_vars` 実行時点で named sub の `CompiledCode`（free_var_writes 済）に
  アクセスできるか。named sub は `sub` 宣言文のコンパイル時に compile される（`helpers_sub_body.rs`）ので、
  enclosing body のコンパイル中に既に存在するはず。配線箇所を特定する。

**案B（runtime・代替）**: `RegisterSub` op 実行時に、登録する sub の `free_var_writes` を走査し、登録フレームの
local slot に一致する名前を §3 パターンで cell 化。
- 利点: 配線が局所的。
- 欠点: ordering（forward-declared/hoisted sub は `$acc=0` の前に登録され得る）。ただし slot は Nil で既存なので
  Nil cell を box → 後続 `$acc=0`（SetLocal）が cell を通して書く（vm_var_assign_ops.rs:5837 で確認済）ので可。

→ **まず案A を調査し、配線可能なら採用。困難なら案B。**

### 6.2 ボックス化（boxing）
検出した lexical を §3 の scalar `:=` パターンで cell 化（同一 Arc を slot + env + **saved frames** へ）。
`box_captured_lexicals` を named-sub 捕捉でも呼ぶか、decl サイト（`$acc` の SetLocal で `needs_cell` 時）で box する。

### 6.3 ガード（perf 崖回避・既存ルール踏襲）
- **捕捉 AND 変異**のみ（`free_var_writes` に含まれる＝書かれる free var のみ）。読むだけは box しない。
- **scalar のみ**（第1スライス）。container `@`/`%` は別スライス（§7）。
- **type/where 制約付きは skip**（既存ルール・vm_register_ops.rs:349-352）。
- escape-aware。`fib`/`method-call` は captured-mutated lexical を持たないので無影響（要 timed 確認・#2746 教訓）。

### 6.4 検証
1. 第1スライス後、`drain_and_reconcile_after_cached_call` の blanket reconcile を一時的に外して
   `t/multi-frame-accumulation-coherence.t` が **通る**ことを確認（= (a) が cell 共有で解けた証拠）。
2. blanket を戻して byte-identical（make test 988/9634 維持）。
3. pin = `t/captured-outer-cell-sharing.t`（multi-frame scalar accumulation・nested named sub・再帰・
   ON/OFF reconcile 同値）。

---

## 7. 後続スライス（env_dirty 削除まで）

- **スライス2: 捕捉 container `@`/`%` の cell 化**。`box_captured_lexicals` の `@`/`%` skip を escape-aware に緩和。
  **前提**: outer container の decont 消費面の網羅監査（env-locals-coherence §6 open-q#1・iteration/slice/native
  method が raw items を舐める経路）。Sub-slice 1b の本来の「container」部分。
- **スライス3+**: §5 の検証で surface する残ケース（scoped overlay / carrier 等）を順次 cell 化。
- **最終**: 残サーフェス 0 を確認 → `env_dirty` / `ensure_locals_synced` / `saved_env_dirty` 削除
  （PLAN.md §2-E）。`pairs`/`slip` carrier-drop も同時に安全化。

---

## 8. Hazard チェックリスト

- [ ] **saved-frame 伝播必須**（§3 教訓）。slot+env だけだとメソッド return で cell が巻き戻る。
- [ ] **type/where 制約付き scalar は box 禁止**（制約再チェック迂回）。
- [ ] **non-scalar Value 種（Package/Sub/Instance/Proxy）は box 禁止**（identity/メタ破壊・既存 skip 維持）。
- [ ] **forward-declared sub の ordering**（案B の場合）。Nil cell box → SetLocal 透過で吸収できるか確認。
- [ ] **perf**: 該当 var の毎アクセスに Mutex lock。escape-aware で captured-mutated に限定。timed roast / fib・method-call 確認。
- [ ] **container decont 漏れ**（スライス2）: cell が slice/`.kv`/`.pairs`/native raw-items 読みで漏れない監査。
- [ ] **cross-thread**（Track C）: `Arc<Mutex>` cell と `shared_vars`/`clone_for_thread` の整合。

---

## 9. 着手手順（次セッション用クイックスタート）

1. このプラン §6.1 案A の配線可否を調査（`compute_free_vars` から named sub の `CompiledCode` 到達可能か）。
2. `t/captured-outer-cell-sharing.t` を先に書く（multi-frame `$acc` accumulation 他・raku 期待値で固定）。
3. 検出＋ボックス化を実装（§6.1-6.2）。
4. §6.4 で検証（blanket 外して pin が通る → 戻して make test）。
5. PR（feature branch・auto-merge）。CI 後、§7 スライス2へ。
