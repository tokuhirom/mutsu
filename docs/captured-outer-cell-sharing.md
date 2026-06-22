# Captured-outer lexical cell 共有 — 実装プラン（Sub-slice 1b+ / env_dirty 削除への substrate）

> **Status:** SLICE 1〜1.19 DONE（〜2026-06-21・第41〜47セッション）。§6 第1スライス（named-sub 捕捉
> scalar decl-site cell 化）＋§7.1〜7.1l（metaop-thunk `Mu`／carrier single-frame／EVAL carrier multi-frame／
> captured-outer container `@`/`%` cell 化／`X`-cross metaop thunk／nested-method capture／cross-thread shared-var／
> object 添字代入 invocant slot／substr-rw・subbuf-rw／zip short-circuit／map LAST phaser／undefine() lvalue）
> ＋slice 1.17（proto state-%cache）／1.18（caller-frame by-name write）／**1.19（param default self-scoping＝code.t 8）**
> を実装。各 pin が **blanket reconcile ON/OFF 両方で PASS**。**OFF roast survey（authoritative・§7.2a）= 初回 13 → 残 1**。
> **destruction.t 3 = slice 1.20（#3400）で解決済**（cross-thread writeback の retain-on-miss・§7.2c）。
> **lazy-lists.t 24-26 = 2026-06-22 で解決済**（`.kv`/`.pairs`/`.antipairs` を lazy index-pipe 化＝
> `MapGrepSpec.index_transform`・lazy ソースを eager force しない）。**∴ OFF roast survey の決定的サーフェスは
> IO-Socket-Async.t の reactive 並行 flaky のみ（決定的 pin 不可）に枯渇。**
> 次＝env_dirty 削除の最終段（§7.4・`blanket_reconcile_if_dirty` 空洞化）。
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

### 7.0 ★残サーフェス地図（slice 1 後・2026-06-21 計測）

slice 1 マージ後に `MUTSU_NO_BLANKET_RECONCILE=1 prove -e target/debug/mutsu t/` で計測した、
**まだ blanket reconcile に依存する（= cell 共有未到達の）23 file**。全て **default build では PASS**（reconcile
が担う）。container `@`/`%` は Arc 共有で既に成立済（survey に container 単独 test なし）。slice 1 の named-sub
scalar accumulation は boxing で解決済（pin 2 本が toggle 下 PASS）ので地図に出ない。

| クラスタ | files | 性質・着手難度 |
|---|---|---|
| **並行(~13)** | `supply-{act,close-phaser,elems,multi-whenever-done,quit-handler,sync-infinite-emit,syntax-basic}`・`whenever-{last,quit}-phaser`・`promise-combinator`・`proc-async`(4)・`scheduler-cue-times`・`concurrency-basic` | cross-thread cell 共有の壁。`Arc<Mutex>` cell と `shared_vars`/`clone_for_thread` 整合（§8）。**最難・後回し**。 |
| **carrier(~5)** | `metaop-thunk-captured-outer`(4)・`subst-closure-writeback`(3)・`eval-carrier-precise`(2)・`lazy-reify-captured-outer`(2)・`require-expression`(1) | 捕捉 outer write を carrier 経由で運ぶが multi-frame で cell 未共有。 |
| **attr/method(~3)** | `attribute-trait-mod`(5・単一最大)・`methods-instance-regressions`(1)・`cross-metaops-regressions`(1) | method dispatch 経由の捕捉。 |
| **misc** | `gather-lazy`(2)・`env-dirty-reconcile-coherence`(1=reconcile 自体の test) | |

> 地図は `tmp/blanket-reconcile-surface.txt`（揮発）にも出力。再計測コマンドは上記。

### 7.1 ✅ スライス1.5（DONE・第42セッション）= `metaop-thunk` の typed-scalar（`Mu` universal 緩和）

`metaop-thunk-captured-outer`(4 fail) は `my Mu $s; 1 Zand ($s++,)` 系＝**thunk（closure）に捕捉される
typed scalar**。untyped `my $s` は toggle 下 PASS（`box_captured_lexicals` が box）だが、`Mu` 制約付きは
§6.3/§8 の「type/where 制約 skip」で box されず fail。**`Mu` は universal type（全値マッチ）なので write-through が
制約再チェックを迂回しても無害**＝`box_captured_lexicals`（vm_register_ops.rs:343-353）の type-skip を
`tc != "Mu"` のときのみ発火するよう緩和。subtest 4/6/8/9（`Zand`/`Zor`/`Z&&`/`Z||` with `my Mu $s`）が
toggle 下でも PASS に。pin=`t/metaop-thunk-captured-outer-coherence.t`（12・ON/OFF 両 PASS）。make test 9656 /
make roast 1285 回帰なし。**注意**: `Any` は Junction を弾くので universal でない＝緩和は `Mu` のみ。closure 駆動
なので `box_captured_lexicals` 側の緩和のみで足り、`box_decl_local_cell`（named-sub path）は今回不要だった。

### 7.1b ✅ スライス1.6（DONE・第42セッション）= carrier cluster の single-frame 部（lazy map / gather / subst）

carrier cluster のうち **single-frame**（書く callee が caller と同一フレーム or 直接呼ばれた callee）の3機構を
precise writeback 化（`pending_rw_writeback_sources` に積み、既存の call-site/force-site drain が処理）＝blanket
reconcile 非依存に。**この3つは cell 化ではなく precise-writeback で解けた**（map/grep が既に使う #3335/#3307 の機構を
横展開）:

- **lazy map（`@a.map({$c++}).eager`）**: `try_native_array_map` の `classify_body` が `$c++`/`$c--`（topic 以外の
  **plain named** scalar inc/dec）を過剰に escape（`None`）扱いし slow path へ落とし、slow path は captured write を
  記録しなかった。`Expr::Var(_) => Some(false)`（topic を変異しないので simple）に緩和＝native loop が処理し
  `$c=$c+1` 同様に free-var write を記録（vm_native_map.rs:303）。indexed `@a[$i]++`/attr `$o.x++` は引き続き fall back。
- **gather（`gather { ...; $c++; take $_ }`）**: forcing 時の `reconcile_caller_after_lazy_force` が gather body を
  `blanket_reconcile_if_dirty`（OFF 無効）でしか reconcile していなかった。両 force inner（`force_lazy_list_vm_inner` /
  `_n_inner`）の env-merge 直後に gather body の `free_var_writes` を `record_eager_block_free_var_writeback` で記録
  （vm_helpers.rs）＝force-site の既存 `apply_pending_rw_writeback` が precise drain。
- **subst（`.subst(/../, { $n++ })`）**: `eval_subst_replacement_cased` は捕捉 lexical write を env へ伝播するが
  pending に積まず blanket 依存だった。伝播箇所で名前を `pending_rw_writeback_sources` に push（methods_string.rs）＝
  `.subst` call-site が drain。

pin（既存）=`t/{lazy-reify-captured-outer,gather-lazy,subst-closure-writeback}.t`（ON/OFF 両 PASS）。make test 9672 /
make roast 1285 回帰なし。

### 7.1c ✅ スライス1.7（DONE・第42セッション）= EVAL carrier の multi-frame cell 共有

carrier cluster の **multi-frame** 部（`eval-carrier-precise` subtest 3/5・`require-expression` subtest 3）＝
**descendant frame から ancestor lexical を書く EVAL carrier**（`sub set_x(){ EVAL '$x = 5' }; set_x()`）。
EVAL 文字列内の write は **静的 `free_var_writes` 解析が見えず**、owner の decl-site で cell 化できない。precise
single-frame writeback も**不可**＝値が callee の env restore で失われ、drain は restore 後の stale env を読む（実証済）。
∴ **EVAL 実行時に cross-frame cell 化**: `box_carrier_free_var_writes`（vm_env_helpers.rs）が EVAL'd code の
`free_var_writes` の captured-outer scalar を、live `env` + 全 `call_frames.saved_env` で同一 `ContainerRef` cell 化
→ EVAL の by-name write が cell を通り、owner frame の env restore 後も cell が 5 を保持。**ガード**:
①`cell_boxing_active()` gated（default は no-op・byte-identical）②`__mutsu_in_eval` 限定（supply/whenever/gather body も
eval_block_value 経由だが Track C 並行 cell が別管理＝触れない）③sigilless alias（`__mutsu_sigilless_alias::x`）は
skip（`\x`→`$a` の alias chain を cell が detach するため）④type/where 制約 skip（`Mu` 除く）。
呼び出し＝`eval_block_value`（resolution.rs・compile 後・run 前）。pin（既存）=`t/{eval-carrier-precise-writeback,
require-expression}.t`（ON/OFF 両 PASS）。make test 9679 / make roast 1285 回帰なし。OFF survey 20→18。
**注意**: 当初 `__mutsu_in_eval` 無し（broad）版は supply/whenever 14 file を OFF-clean 化したが
`concurrent-cell-writeback` subtest4 / `sigilless-params` subtest3 を**回帰**＝並行 cell 機構との衝突。EVAL 限定で解消。

### 7.1d ✅ スライス1.8（DONE・第43セッション）= captured-outer container `@`/`%` cell 化（`attribute-trait-mod` 5 fail）

`attribute-trait-mod`（OFF 5 fail・単一最大）の真因＝**captured-outer container の COW detach + slot-restore clobber**。`my @noted-names` を
ユーザ `trait_mod:<is>` が class composition 中に `push` するが、①class registration の `saved_env = self.env.clone()` が
配列 Arc の refcount を上げ、push の `Arc::make_mut` が COW で detach（env=新 Arc B(3)、slot=旧 Arc A(0)）。②その後の
`$obj ~~ Type` smartmatch が **env を local slot から復元**するため、stale な slot A(0) が env を 0 に巻き戻す（blanket
reconcile ON なら smartmatch 前に slot を env から pull するので顕在化しない）。∴ `@noted-names` を **whole-container cell**
にして slot==env を保てば、push が cell を通り、`saved_env.clone()` は cell（`Arc<Mutex>`）を共有し COW しない、smartmatch の
slot-restore も同一 cell を保つ。

- **検出（compile-time）**: container の in-place mutation（`push`/`append`/element-assign）は `SetGlobal` name-write では
  **ない**ので `free_var_writes` に乗らない。新フィールド `CompiledCode.free_var_container_writes`（`op_container_mutate_const_idx`
  が `CallMethodMut`＋mutating method 名／`IndexAssign*Named`／`ArrayPush` から `@`/`%` non-own 名を抽出）に分離計上し、
  `compile_sub_body` が named sub の `free_var_writes ∪ free_var_container_writes` を `named_sub_captures` に push →
  既存の fold が `needs_cell_named_sub` に乗せる。**`free_var_writes` 本体は不変＝default-build の precise-writeback drain に
  無影響。**
- **boxing**: `box_decl_local_cell` の `@`/`%` skip を解き `box_decl_local_container_cell`（Array/Hash を `ContainerRef` cell に
  包み slot+env に配置・typed container は skip）へ dispatch。write-back（`try_native_array_mut`/`try_native_hash_mut_bound`/
  `env_root_descended_mut`）と read（`GetArrayVar`/`GetHashVar` の `into_deref`）は `:=` bound container 用に既に cell 対応済。
- **★broad boxing は不可（decont 漏れ）**: spike で「全 `@`/`%` decl を box」したところ aliased-container-mutation /
  constant-hash-element-ro / hash-push-seq-of-pairs / quanthash-hyper-funcop など ~12 file が OFF 回帰（cell が slice/`.kv`/
  raw-items 読みで漏れる）。∴ **precise 検出（named-sub captured-and-mutated container のみ）が必須**。precise 版は decont 漏れ 0。
- pin=`t/captured-outer-container-cell-sharing.t`（9・trait_mod push/hash-elem・named-sub accumulation・ON/OFF 両 PASS）。
  OFF survey 17 file（attribute-trait-mod の 5 fail 解消）。

### 7.1e ✅ スライス1.9（DONE・第43セッション）= `X`-cross metaop thunk の captured-outer scalar write（`cross-metaops` subtest 2）

`Nil Xorelse ($t = $_,)`（`X` cross meta over short-circuit op）は右辺を `__mutsu_cross_shortcircuit("orelse", Nil,
AnonSub{$t=$_})` の **immediately-invoked thunk** にコンパイルする。call arg として渡される closure は escape 解析が
non-escaping 扱い→`box_captured_lexicals` が box しない→thunk の captured-outer write（`$t`）が OFF で caller slot に
届かない。**cell 化でなく precise-writeback で解決**（slice 1.6 の lazy-map/gather/subst と同型）: `builtin_cross_shortcircuit`
が thunk 実行後（`thunk_ran` 時のみ）に thunk の `compiled_code.free_var_writes` を `record_eager_block_free_var_writeback`
で `pending_rw_writeback_sources` に積む→`__mutsu_cross_shortcircuit` の call-site が既存 `apply_pending_rw_writeback` で
drain。**非gated（ON/OFF 両対応・reconcile と idempotent）**。pin=`t/cross-metaop-thunk-captured-writeback-coherence.t`
（6・Xorelse/Xandthen/Xor・ON/OFF 両 PASS）。make test 回帰なし。OFF survey 17→16。
**注意（範囲外の別バグ）**: ①`(1,2,3) Xandthen (...)` の list 反復 count（raku=3・mutsu=2）は cross_shortcircuit ループの
別バグ（ON でも fail・captured-write 無関係）。②`$x + 1`（`$x=11` 後）が 2 を返すパーサ quirk も別件。両方とも本スライス対象外。

### 7.1f ✅ スライス1.10（DONE・第44セッション）= nested-method capture（`methods-instance` subtest 3）

`method foo { my $a = 42; method bar { $tracker = $a } }` の **method body 内で宣言された method** `bar` を
`.foo` 後に `.bar` で呼ぶケース。`bar` の captured-outer write（`$tracker`）が OFF で caller slot に届かず stale。
**真因＝dispatch path**: nested-declared method は foo 実行時に RegisterSub で **captured env 付き `&bar` sub** として登録され、
`$d.bar` は compiled method merge path（`call_compiled_method` / `merge_method_env`）でも closure dispatch
（`call_compiled_closure_with_topic`）でもなく、slow path `call_method_with_values` → **`call_sub_value(merge_all=true)`**
（tree-walk）を通る。`call_sub_value` は captured-outer scalar write を `merged`（caller env）へ正しくマージするが、
**caller の local slot を refresh しない**＝blanket reconcile ON 時のみ slot が env から pull される。
（class-level method の `$Foo++` は compiled path = #3322 の `merge_method_env` `changed_caller_locals` 記録で OFF も成立済。
gap は nested-declared method の interpreter dispatch path だけ）。

- **修正（precise-writeback・非gated）**: ①`call_sub_value`（resolution.rs）の merge ループで、body-entry snapshot
  （`body_entry_env`）から **変化した captured-outer scalar**（Bool/Int/Num/Str/Rat・caller env に存在）を収集し
  `pending_rw_writeback_sources` に push（merge_all / non-merge_all 両 branch・既存の precise scalar-changed 条件と同型）。
  ②CallMethod op（vm_call_method_ops.rs）の call-site tail を `blanket_reconcile_if_dirty(code)` →
  `drain_and_reconcile_after_cached_call(code)`（= `apply_pending_rw_writeback` + blanket）に変更。**この op は従来
  pending を drain しておらず**（`merge_method_env` のコメント「CallMethod op が slot へ書き戻す」が示す意図に実装が
  追いついていなかった＝latent gap も同時に解消）。ON では blanket が superset なので byte-identical、OFF では
  precise drain のみが効く。
- pin=`t/nested-method-captured-writeback-coherence.t`（9・given/topic・explicit invocant・RMW 累積・+=・string・
  enclosing lexical read＋outer write・複数 outer・後続式コヒーレンス・intervening call・ON/OFF 両 PASS）。
  make test 9727 / make roast 回帰なし。OFF survey 16→15（並行 ~13 残）。

### 7.1g ✅ スライス1.11（DONE・第44セッション）= cross-thread shared-var writeback（並行 cluster の決定的部）

並行 cluster ~13 のうち、`promise-combinator`・`scheduler-cue-times` は **flaky でなく決定的**な OFF 依存（OFF=1 が 4/4 再現・
ON=0）。`my $seen = []; Promise.allof(start { cas $seen, -> @c { flat @c, 1 } }).result; is ~$seen, '1'` ＝worker
`start` block の `cas`（atomic array CAS・`__mutsu_atomic_arr::$seen` shared store に書く）が `.result`（await）後に
parent の `$seen` **local slot** に届かない。`sync_shared_vars_to_env`（runtime/mod.rs）は cross-thread の dirty key を
env に書き戻すが（`__mutsu_atomic_arr::`/`__mutsu_atomic_hash::`/`__mutsu_atomic_name::` を解決）、**caller slot は
refresh しない**＝blanket reconcile ON のみ pull。

- **修正（precise-writeback・非gated）**: `sync_shared_vars_to_env` の `updates` 反映ループで各 synced key を
  `pending_rw_writeback_sources` に push → `.result`/await の **CallMethod call-site が drain**（slice 1.10 で
  `drain_and_reconcile_after_cached_call` 化済みなので追加配線不要）。ON は env_dirty→blanket superset=byte-identical、
  OFF は precise drain のみ。env が cross-thread sync 後の source of truth なので slot=env は常に coherence 方向＝安全。
- **★cross-thread cell（doc §8）の本格実装は不要だった**: 基本 cross-thread captured-write（`start { $x = v }; await`）は
  既に shared_vars で ON/OFF 両成立（決定的 probe で確認）。決定的 OFF 依存は **atomic CAS の sync→slot writeback** のみで、
  slice 1.10 の drain 基盤に sync 名を載せるだけで解けた。残る並行 cluster の OFF 依存は **flaky な reactive 系**
  （supply/whenever の timing 依存・`^not ok` に出ない）で、決定的 substrate ではない。
- pin=`t/cross-thread-shared-var-writeback-coherence.t`（6・cas array/scalar/hash・累積・後続式・intervening・ON/OFF 両 PASS）。
  make test 9739 / make roast（要確認）。**決定的 OFF-only fail（`^not ok`）= 0 に到達**。

### 7.1h ✅ スライス1.12（DONE・第45セッション）= object 添字代入の invocant slot writeback（`parametric-role-of-type`）
`parametric-role-of-type`（OFF で決定的 abort・ran 11/14・test 12）を解消。真因は当初の「typed array 属性の COW detach」
診断ではなく **object 添字代入の invocant slot 未 refresh** だった。最小再現＝
`role ER[::T] does Positional { has ER[T] @!c handles <AT-POS ASSIGN-POS BIND-POS> }; my $e = ER[Int].new; $e[0] = ER[Int].new; say $e[0].WHAT`
が ON=`(ER)`／OFF=`(Any)`。`$e[0] = v` は `exec_index_assign_expr_named_op` → ASSIGN-POS dispatch →
`assign_method_lvalue_with_values`（methods_mut.rs:1813 の handles delegation 経路）が delegate container を変異させ
`env[$e]` に更新済 instance を `write_back_sharing` で書くが、**caller の local slot は refresh しない**。default build は
blanket reconcile が `$e` を env から pull するので顕在化せず、OFF（cell boxing）では slot が stale instance のまま →
AT-POS が空 `@!c` を読む。
- **修正（precise・非gated）**: `exec_index_assign_expr_named_op` 末尾で、添字代入後 `env[var]` が `Instance`/`Mixin`
  （＝object 添字代入で ASSIGN-POS/ASSIGN-KEY を dispatch したケース）なら `locals_set_by_name` で local slot へ write-through。
  plain Array/Hash 要素代入は fast path で既に slot 更新済＆ここに Instance/Mixin として到達しないので発火しない。
  ON は blanket reconcile の冪等 superset＝byte-identical。**副次効果**: plain `role P does Positional { has @!c handles
  <AT-POS ASSIGN-POS> }` の `$p[0]=v`（ON/OFF 両方で `Nil` を返していた pre-existing バグ）も同時に修正。
- pin=`t/positional-role-attr-writeback-coherence.t`（7・Positional/parametric/Associative 添字代入・BIND-POS・ON/OFF 両 PASS）。
  make test 9755。**注**: delegated mutating *method call*（`$q.push(5)` が `@!c` へ委譲）は ON/OFF **両方**で失敗する別の
  pre-existing バグ（method-call 後の invocant slot 未 refresh・トグル非依存）＝本スライス対象外・スコープ外として pin から除外。

### 7.1i ✅ スライス1.13（DONE・第45セッション）= substr-rw / subbuf-rw lvalue slot writeback（OFF 最大塊・12 fail）
`substr-rw($s, ...) = v`／`subbuf-rw($buf, ...) = v` が OFF で `$s`/`$buf` を変更しない（`gorch ding` のまま・raku=
`gloop ding`）。経路＝lvalue assign は `__mutsu_assign_named_sub_lvalue` Call → `assign_named_sub_lvalue_with_values`
（builtins_lvalue.rs:258/281）が env scan で target var を特定し `assign_method_lvalue_with_values` に委譲→string/buf を
変異して `env[$s]` に書くが **caller local slot 未 refresh**。`exec_call_func_op` の `lvalue_writeback_target` 機構は
`__mutsu_assign_method_lvalue`/`__mutsu_index_assign_method_lvalue`（args[4]=target）専用で named-sub lvalue は対象外。
- **修正（precise・非gated）**: substr-rw/subbuf-rw branch で解決済 target var 名を `pending_rw_writeback_sources` に push。
  `exec_call_func_op` は **常に** `apply_pending_rw_writeback(code)`（vm_call_func_ops.rs:597）を呼ぶので、env の更新値が
  precise に slot へ drain される（blanket pull 不要）。ON は冪等 superset＝byte-identical。
- pin=`t/substr-rw-lvalue-writeback-coherence.t`（6・substr-rw/from-only/累積/bound proxy/subbuf-rw・ON/OFF 両 PASS）。
  make test 9754。`S32-str/substr-rw.t` 46/46・`S03-operators/buf.t` も ON/OFF PASS（OFF survey の **2 file** 消化）。

### 7.1j ✅ スライス1.14（DONE・第45セッション）= zip short-circuit topicalizing thunk writeback（`zip.t` 68/71）
`23 Zandthen ($side-effect = $_,)` の topicalize thunk write が OFF で lost（OFF=0・raku/ON=23）。`Zandthen`/`Zorelse`
は `builtin_zip_shortcircuit_topic`（builtins_feed.rs:299）/`builtin_zip_shortcircuit` 経由で thunk を評価するが、
`builtin_cross_shortcircuit`（slice 1.9・X-cross）と違い **captured-outer write を記録していなかった**＝即時起動 closure で
escape 解析が box しない→blanket reconcile のみが運んでいた。修正＝両関数で thunk 実行後に
`record_eager_block_free_var_writeback(&code, &params)` を呼び pending に push→`__mutsu_zip_shortcircuit*` call site の
`apply_pending_rw_writeback` が drain（X-cross と同型）。ON は冪等 superset＝byte-identical。
pin=`t/zip-shortcircuit-topic-writeback-coherence.t`（5・Zandthen/Zorelse topicalize＋increment・ON/OFF 両 PASS）。
make test 9769。`S03-metaops/zip.t` ON/OFF PASS（OFF survey の **1 file** 消化）。

### 7.1k ✅ スライス1.15（DONE・第45セッション）= `.map` block LAST phaser writeback（`map.t` 62）
`(^20).map({ LAST $ranLAST=True; last if $_==10; $_ }).iterator` の LAST phaser write が OFF で lost（OFF=Nil・ON=True）。
真因＝eager map loop（`resolution.rs:2123` 周辺）は LAST phaser body を map body と**別に** compile/run し
（:2253-2258）、その直後の `record_eager_block_free_var_writeback(&code, ...)`（:2276）は **map body の `code` のみ**記録＝
phaser の `$ranLAST=True` を拾わない。修正＝phaser 実行直後に `vm.record_eager_block_free_var_writeback(&phaser_code, &[])`
追加→pending push→call site の `apply_pending_rw_writeback` drain。ON は冪等 superset＝byte-identical。
pin=`t/map-last-phaser-writeback-coherence.t`（4・last/natural-end/eager・ON/OFF 両 PASS）。`S32-list/map.t` ON/OFF PASS。

### 7.1l ✅ スライス1.16（DONE・第45セッション）= `undefine()` lvalue slot writeback（`undef.t` 85）
`undefine($x) = v`（rw）が OFF で `$x` を変更しない（OFF=foo・ON/raku=bar）。slice 1.13 の substr-rw と**同一経路**＝
`assign_named_sub_lvalue_with_values` の `undefine` branch（builtins_lvalue.rs:250）が `env[$x]` のみ書き slot 未 refresh。
修正＝env 書込前に target var 名を `pending_rw_writeback_sources` に push→call site の `apply_pending_rw_writeback` drain。
pin=`t/undefine-lvalue-writeback-coherence.t`（3）。`S32-scalar/undef.t` ON/OFF PASS。

### 7.2 後続スライス（その先）
- **delegated mutating method call の invocant writeback**（pre-existing・トグル非依存）: `$q.push(5)` が `handles` 経由で
  `@!c` に委譲されるとき、attr は変異するが method-call 後に caller の `$q` slot/instance が refresh されず累積が壊れる
  （ON/OFF 両方で失敗）。env_dirty 削除には不要（ON でも壊れている＝OFF survey の決定的依存ではない）が、一般的バグとして別途修正可。
- **container `@`/`%` の named-sub 以外の cell 化**（必要なら）: closure 捕捉 container は現状 Arc 共有で動く。`box_captured_lexicals`
  の `@`/`%` skip を緩和する場合は §8 の decont 監査が前提（broad boxing は ~12 file 回帰を実証済＝precise 限定必須）。
- **並行 cluster の残り**（supply/whenever/react ~11）: §7.1g で決定的部（promise-combinator/scheduler-cue-times の
  atomic-CAS sync）は解決。残りは **flaky な reactive 系**（supply/whenever の timing 依存・`^not ok` に出ず
  abort/timeout で manifest）＝決定的 OFF-pin が書けないため toggle survey で追いきれない。env_dirty 削除の最終段
  （`blanket_reconcile_if_dirty` 空洞化）で実挙動を確認するのが現実的。基本 cross-thread captured-write は shared_vars で
  既に ON/OFF 両成立済（cross-thread cell の本格実装は不要と判明）。

### 7.2a ★OFF roast survey（第45セッション・2026-06-21・authoritative）= env_dirty 削除の真の残サーフェス
これまでの「決定的 OFF 依存 = 0 到達」は **t/ のみの `^not ok` survey** に基づく過小評価だった。`MUTSU_NO_BLANKET_RECONCILE=1
make roast`（release・全 whitelist 1285）を実走したところ、**13 ファイルが OFF で決定的に fail**（全て pre-existing＝
main baseline でも同じ subtest が OFF fail・本スライスの変更とは無関係＝debug 比較で確認）。これが env_dirty 物理削除
（§7.4）を解禁するために潰すべき残サーフェス（初回 13 → slice 1.13 で substr-rw.t/buf.t、slice 1.14 で zip.t、slice 1.15 で map.t、slice 1.16 で undef.t、slice 1.17 で proto.t/subsignature.t、slice 1.18 で caller.t/callframe.t 消化 → **残 4**）:

| file | failed subtests | 推定カテゴリ | 状態 |
|------|-----------------|--------------|------|
| ~~S32-str/substr-rw.t~~ | 1, 8-9, 16, 24-25, 33-38 | substr-rw lvalue slot writeback | ✅ slice 1.13 |
| ~~S03-operators/buf.t~~ | 38 | subbuf-rw lvalue slot writeback | ✅ slice 1.13 |
| S02-types/lazy-lists.t | 24-26 | lazy list captured-outer | |
| ~~S03-metaops/zip.t~~ | 68, 71 | Z-cross topicalizing thunk writeback | ✅ slice 1.14 |
| ~~S02-names/caller.t~~ | 9 | caller-frame by-name write slot writeback | ✅ slice 1.18 |
| ~~S06-advanced/callframe.t~~ | 12 | caller-frame by-name write slot writeback | ✅ slice 1.18 |
| ~~S06-multi/proto.t~~ | 21 | caching proto `state %` writeback | ✅ slice 1.17 |
| ~~S06-multi/subsignature.t~~ | 54 | caching proto `state %` writeback | ✅ slice 1.17 |
| ~~S06-signature/code.t~~ | 8 | `&`-param default self-scoping | ✅ slice 1.19 |
| ~~S12-construction/destruction.t~~ | 3 | cross-thread worker-DESTROY captured-write（retain-on-miss sync） | ✅ slice 1.20 |
| ~~S32-list/map.t~~ | 62 | `.map` block LAST phaser writeback | ✅ slice 1.15 |
| ~~S32-scalar/undef.t~~ | 85 | undefine() lvalue slot writeback | ✅ slice 1.16 |
| S32-io/IO-Socket-Async.t | 5, 7 | reactive 並行（flaky 疑い） | |

**triage（第45〜46セッション・実測済）**:
- ~~**map.t 62**~~ ✅ **slice 1.15 で消化**（§7.1k）。`.map({ LAST $x=True })` の LAST phaser body は map body と**別に**
  コンパイル・実行される（`resolution.rs:2253`）ので、map body の `record_eager_block_free_var_writeback`（:2276）が
  phaser の write を拾わなかった。phaser 実行直後に `record_eager_block_free_var_writeback(&phaser_code, &[])` 追加で解決。
- **lazy-lists.t 24-26（`.kv`/`.pairs`/`.antipairs` is lazy）= writeback ではなく laziness バグの露出**（実測 ON=1/OFF=0/
  raku=1）。`make-lazy-list = gather { take ...; $was-lazy = 0 }.lazy` を `.kv` が **eager に force**して `$was-lazy=0` を
  走らせてしまう（mutsu の `.kv`/`.pairs`/`.antipairs` が非 lazy）。ON は write-loss で偶然 raku 一致、OFF は slice 1.6 の
  gather writeback が正しく伝播して**潜在 laziness バグを露出**（doc §7.3 教訓 #3 の典型）。∴ 修正は `.kv`/`.pairs`/
  `.antipairs` の **真の lazy 化**（L 系・別軸）＝precise-writeback では解けない。env_dirty 削除には laziness 修正が前提。
- ~~**undef.t 85**~~ ✅ **slice 1.16 で消化**（§7.1l・substr-rw と同経路の `undefine()` lvalue）。
- ~~**proto.t 21 ＋ subsignature.t 54**~~ ✅ **slice 1.17 で消化（1 修正で 2 file・PR #3389）**。caching proto
  （`proto cached($a){ state %cache; %cache{$a} //= {*} }`）の cache が呼び出しを跨いで accumulate せず multi が毎回
  再 dispatch（OFF=`aba`／raku=ON=`ab`）。真因＝**`{*}` redispatch が RHS で評価される hash element-assign の
  dual-store divergence**: `{*}`（`call_proto_dispatch`）の `restore_env_preserving_existing`（dispatch.rs:2074）が
  `self.env` を丸ごとスワップし、env の `%cache` Arc を proto body の local slot の Arc から**切り離す**（strong_count→1）。
  `try_fast_hash_element_assign`（vm_var_assign_ops.rs:2606）は strong_count==1 だと local slot を更新しない（env のみ
  in-place mutate）ので slot が stale な空 hash のまま残り、続く `sync_env_from_locals`（run_inner）が stale slot を env に
  書き戻して `state` 永続化が空 hash を保存→cache 喪失。修正（非 gated・precise）＝fast hash assign 末尾で strong_count==1
  でも local slot が**存在すれば**（＝定義上必ず diverged）代入後 env 値を slot へ mirror（env-only `%*ENV` 等は slot 無し＝
  no-op、default は blanket reconcile で冗長＝byte-identical）。`state @`-array element-assign（`@cache[$i] //= {*}`）+
  in-place push は元々 coherent で非該当。教訓: run_inner の state persist は `sync_state_locals`（env 優先・476）→
  `sync_env_from_locals`（480）順なので、reorder（env flush 先）は逆効果（stale local が fresh env を clobber）＝源流の
  divergence 修正が正解。pin=`t/proto-state-cache-writeback-coherence.t`（6・ON/OFF 両 PASS）。make test 9788。
- ~~**caller.t 9 ＋ callframe.t 12**~~ ✅ **slice 1.18 で消化（1 修正で 2 file・第46セッション）**。caller-frame の
  lexical を名前書きする `$CALLER::x = v`（rw dynamic-var）／`callframe(d).my.<$x> = v` が OFF=stale（ON=書込成功）。
  真因＝`set_caller_var`（mod.rs:5327）は `caller_env_stack[idx]` と現在 env に書き、`pop_caller_env_with_writeback`
  （mod.rs:5277）が return 時に restored caller env へ dynamic var を伝播するが、**caller の local slot は未更新**＝OFF で
  stale slot を読む。修正＝`set_caller_var` で書いた bare name を**新リスト `pending_caller_var_writeback`** に push し、
  call-site の `apply_pending_rw_writeback` が `env[name]`→slot に drain。★`pending_rw_writeback_sources`（drop-on-miss）
  と分離した理由＝caller-frame の slot は数フレーム上にあり、writer が return 前に**より深い**呼び出しをすると（`f(){ callframe(1).my.<$x>=v; g() }`）pending が g() の call-site で1フレーム早く消費される→**retain-on-miss**（slot が
  当該 code に無ければ破棄せず保持し、所有フレームまで運ぶ）。`is dynamic` でない caller lexical は従来通り die。
  pin=`t/caller-frame-write-slot-coherence.t`（5・ON/OFF 両 PASS）。make test 9799。
- ~~**code.t 8（`&`-param）**~~ ✅ **slice 1.19 で消化（第47セッション・PR 別途）**。`sub foo(&foo = &foo){...}` の default
  scoping バグ＝**writeback でなく露出バグ**（ON は carrier `lives-ok { foo }` で captured write が reconcile に落とされ偶然
  PASS、OFF が正しく伝播して露出）。真因＝パラメータの default 式を評価する際に **そのパラメータ自身が scope に入っていない**
  ため、`&foo`（default RHS）が外側の登録済み sub `foo` に解決されていた（raku は param `&foo` 自身＝undefined に解決）。
  修正＝`bind_function_args_values` の両 default-eval サイト（binding.rs ~770 named / ~1500 positional）を新ヘルパー
  `eval_param_default` に集約し、default 式評価の**直前にパラメータ名自身を undefined 型オブジェクト（or Nil）で env に
  pre-bind**（self-reference が outer ではなく未定義パラメータに解決＝Raku の「param はその default 内で scope に入る」規則）。
  earlier param（`$b = $a`）は前 iter で bind 済なので影響なし、別名 outer（`$z = $y`）も影響なし。pin=
  `t/param-default-self-scoping.t`（7・ON/OFF 両 PASS）。make test 9819。
- ~~**destruction.t 3**~~ ✅ **slice 1.20 で消化（第48セッション・retain-on-miss cross-thread sync）**。`submethod DESTROY {
  $a++ }` の worker-thread captured write が top-level slot に届かない。真因＝§7.2c の「cell-detachment」は**誤診断**（#3398
  で訂正済・真因は cross-thread writeback の **drop-on-miss** リスト誤用）。実測トレースで確定: worker が `$a` を 1 に書き
  `shared_vars_dirty` にマーク → `await` の `sync_shared_vars_to_env`（mod.rs:6420）が `["a","@order"]` を pending に push し
  `env["a"]=1` → しかしその後 `await` 内の `run_pending_instance_destroys()`（builtins_system.rs:1796）が **DESTROY を
  `locals=[]` で dispatch** し、その tail の `apply_pending_rw_writeback`（drop-on-miss）が pending を**消費して捨てる** →
  top-level frame（`locals=["a","@order","b0"]`・"a" slot 所有）が drain する頃には `sources=[]`。**修正**＝`sync_shared_vars_to_env`
  の push 先を **drop-on-miss の `pending_rw_writeback_sources` → retain-on-miss の `pending_caller_var_writeback`** に変更
  （cross-thread synced var の owning slot は数フレーム上＝caller-var と同形の retain-on-miss が正しい）。slice 1.18 の
  caller-frame writeback と同じリストを再利用＝中間 DESTROY frame は slot 無しで retain、top-level が drain して slot refresh。
  pin=`t/destroy-cross-thread-writeback-coherence.t`（5・ON/OFF 両 PASS）。
  - ＋別軸: lazy-lists.t 24-26（laziness バグ・L 系）／IO-Socket-Async.t 5,7（flaky）。

**残＝別軸 2（lazy-lists laziness・IO-Socket-Async flaky）のみ。純 writeback コヒーレンスのサーフェスは枯渇
（slice 1.18 で純 writeback、1.19 で露出バグ、1.20 で cross-thread DESTROY writeback）。
§7.4（env_dirty 削除）は lazy-lists の真 lazy 化（L 系・別軸）後に射程。**

### 7.2c ✅ destruction.t 3 — cross-thread（worker DESTROY）captured-write（第47調査・第48解決＝slice 1.20）

**★解決済（slice 1.20・第48セッション）。** 真因は cross-thread writeback の **drop-on-miss リスト誤用**（§7.2a の slice 1.20
行を参照）。修正＝`sync_shared_vars_to_env` の push 先を `pending_rw_writeback_sources`（drop-on-miss）→
`pending_caller_var_writeback`（retain-on-miss）に変更。なお第47前半の「cell-detachment」診断は#3398 で誤りと訂正済
（計測で `$a` は cell-box されておらず、`box_decl_local_cell`/`box_captured_lexicals` の probe が一切発火しない）＝
真因は cross-thread。以下は当時の調査記録（歴史参考）。

**最小再現（決定的・`MUTSU_NO_BLANKET_RECONCILE=1` で確実 fail）**:
```raku
my $a = 0;
my @order;
class Foo { submethod DESTROY { $a++ } }
class Bar { submethod DESTROY { push @order, "x" } }
my $b0 = Bar.new; $b0 = Nil;
await start {
    loop {
        $*VM.request-garbage-collection;
        my $foo = Foo.new;
        my $bar = Bar.new unless +@order;   # 条件付き生成
        last if $a && @order;
    }
};
say "a=$a";     # OFF: a=0（ON/raku: a=1）
```

**最重要事実: `start` は別スレッド**。`builtin_start`→`spawn_callable_promise`（builtins_system.rs:106）が
**`clone_for_thread()` + `spawn_user_thread`** でワーカーを起動＝**DESTROY はワーカースレッド（クローン interpreter）で発火**。
env はワーカー→親に伝播するが（slice 1.11 の shared_vars 機構）、親の **local slot** は別途 refresh が要る。

**切り分け（OFF）**:
- 単純 cross-thread write `await start { $x = 5 }` → ✅ `x=5`（slice 1.11 で既に成立）。
- 単一 DESTROY-in-thread `await start { my $f=Foo.new; $f=Nil; $*VM.request-garbage-collection }` → ✅ `x=5`。
- **fail するのは loop + 複数 captured var（`$a` scalar + `@order` array）+ 条件付き生成**（d10/min4/min10）。
  ループ無し・単一 var は OK。

**真因（計測で確定・親の await call-site で probe）**: `exec_call_func_op`→`dispatch_func_call_inner`→**L597
`apply_pending_rw_writeback(code)`** の時点で:
- `code.locals = ["a","@order","b0"]`（top-level frame）／`find_local_slot("a") = Some(0)`（**slot は存在**）／
  `env.get("a") = Int(1)`（**env は正しく伝播済**）。
- だが `pending_rw_writeback_sources` / `pending_caller_var_writeback` が **空**＝drain が走らず slot[0] が 0 のまま。
- ∴ **欠落は「ワーカーが書いた captured scalar `a` を親の pending writeback に記録する」一点だけ**。単一 write 版は
  slice 1.11 の `sync_shared_vars_to_env`（mod.rs:6412・dirty key を `pending_rw_writeback_sources` に push）が "a" を
  運ぶので動く。loop+複数 var 版では **"a" が dirty として運ばれない**（ループ内の `last if $a` 読み出しが dirty を消費する／
  複数 var の dirty 追跡が一部しか乗らない、のいずれか＝要特定）。

**試した修正と不成立（第47・全 revert 済）**: ①DESTROY merge（class.rs:1421）で変更 captured scalar を
`pending_rw_writeback_sources`/`pending_caller_var_writeback` に記録＋loop 境界 drain → ❌ 記録は**ワーカー interpreter の
pending** に入り join で消失（drain は `locals=[]`/`["foo","bar"]` のワーカー frame でしか起きず "a" slot 不在で retain→消失）。
②DESTROY merge で cell write-through → ❌ そもそも env が cell でなく plain Int（`saved=Some(Int(0))`）＝cell 不在で不成立。

**∴ 次セッションの本丸＝cross-thread writeback recording の拡張（slice 1.11 family・localized で解ける見込み）**:
`sync_shared_vars_to_env`（await が builtins_system.rs:1791 で呼ぶ）が、ワーカーで mutate された captured-outer scalar を
**漏れなく** `pending_rw_writeback_sources` に push すれば、親の await call-site の既存 drain（L597・top-level slot に到達可）が
slot を refresh する。**まず特定すべき: loop+複数 var の場合に shared_vars_dirty から "a" が落ちる経路**（ループ内 `$a` 読み出しの
dirty クリア? worker の最終 sync で一部 var のみ?）。これは **cell substrate 不要**＝cross-thread shared-var の dirty/pending
記録の取りこぼし修正。env も slot も await call-site で到達可能なことは計測済みなので、pending 記録さえ補えば通る。
destruction.t 3 がその pin。（別軸: lazy-lists.t laziness / IO-Socket-Async flaky は無関係。）
（補助案B＝await/loop 境界 top-level drain は、cell 伝播が入れば不要。）

### 7.2b ✅ proto `{*}` redispatch ＋ `state` var coherence（proto.t 21 / subsignature.t 54）= slice 1.17 で解決
**実際の真因は当初推測（`restore_env_preserving_existing` が state を巻き戻す）とは別だった。** plain `state %h` を持つ
通常 sub は OFF でも正常 persist（`counter(){state %seen; %seen<x>++}` → 1 2 3）＝state 機構自体は OK。`state $n`
スカラも proto body で正常 persist。**問題は `%cache{$a} //= {*}` の hash element-assign 固有**: `{*}`
（`call_proto_dispatch`）の `restore_env_preserving_existing`（dispatch.rs:2074）が `self.env` を丸ごとスワップし、env の
`%cache` Arc を proto body の local slot の Arc から切り離す（strong_count→1）。`try_fast_hash_element_assign`
（vm_var_assign_ops.rs:2606）が strong_count==1 で local slot を更新しない→slot stale→`sync_env_from_locals` が
env を stale slot で clobber→state 永続化が空 hash 保存。修正＝fast hash assign 末尾で strong_count==1 でも local slot
存在時に env 値を mirror。詳細＝§7.2a の proto.t 行。pin=`t/proto-state-cache-writeback-coherence.t`。

### 7.3 ★各スライスの必須手順（slice 1 の教訓）

1. **boxing は `cell_boxing_active()` gated を維持**（default は reconcile・byte-identical）。新サーフェスを cell 化する
   = その cluster の var を toggle 下で box する実装を追加。
2. **必ず local で `make roast`（または該当 synopsis）を回す**。`make test` だけでは roast 回帰を見逃す
   （slice 1 は let.t/code.t を make test では検出できなかった）。
3. **cell 化が露出する潜在バグ**（reconcile が carrier で write を落として偶然 PASS していたケース）を覚悟する。
   toggle 下で fail が残るなら、それは「次に直すべき本当のバグ」＝記録して order 付け。
4. toggle OFF/ON 両方で pin が PASS することを確認。

### 7.4 最終（env_dirty 削除）

残サーフェス（toggle OFF survey）が 0 になったら → `blanket_reconcile_if_dirty` を空洞化 →
`env_dirty` / `ensure_locals_synced` / `saved_env_dirty` 削除（PLAN.md §2-E）。`pairs`/`slip` carrier-drop も同時に安全化。
このとき boxing の `cell_boxing_active()` gate を外して恒久 ON 化。

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

**★純 writeback コヒーレンスは slice 1.20（#3400）で完了。lazy-lists.t 24-26 の真 lazy 化も完了（2026-06-22）＝
OFF roast survey の決定的サーフェスは IO-Socket-Async.t の flaky のみに枯渇。** これで env_dirty 削除（§7.4）の前提が
すべて揃った。

**lazy-lists.t 24-26 の解決（2026-06-22・完了）**:
- 真因: `make-lazy-list = gather { take ...; $was-lazy = 0 }.lazy` を `.kv`/`.pairs`/`.antipairs` が **eager に force** して
  `$was-lazy = 0` を走らせていた。ON は write-loss で偶然 raku 一致、OFF が正しく伝播して露出（writeback でなく laziness バグ）。
- 修正: `MapGrepSpec` に `index_transform: Option<IndexTransform>`（Pairs/AntiPairs/Kv）を追加し、`.kv`/`.pairs`/`.antipairs`
  を **lazy index-pipe**（`LazyList::new_index_pipe`）化。genuinely-lazy ソース（`.lazy` gather・無限 spec）に対しては eager
  force でなく lazy pipe を返す。`source_idx` を positional key に使い、pull は既存の `pull_source_element`（gather coroutine を
  incremental 消費）を再利用。`force_lazy_pipe` が `index_transform` Some 時に func/grep を bypass して index 変換を emit。
  3 dispatch 経路（CallMethodMut fast-path・CallMethod 非mut・runtime slow-path methods.rs）すべてに早期 dispatch を追加。
  index-pipe は preserve marker を持つので `my @res = one.kv` が lazy 保持。値は eager 版とバイト一致。pin=
  `t/lazy-pairs-kv-antipairs.t`（18・ON/OFF 両 PASS）。

**次 = §7.4（env_dirty 物理削除）**: `blanket_reconcile_if_dirty` 空洞化 → `env_dirty`/`ensure_locals_synced`/
`saved_env_dirty` 削除 → `cell_boxing_active()` gate を外して boxing 恒久 ON 化。IO-Socket-Async の flaky はこの空洞化で実挙動確認。

---

## 10. env_dirty 物理削除 substrate グラインド（2026-06-22 着手）

§7.4 は「blanket reconcile を空洞化すれば env_dirty を消せる」と書いていたが、**第49セッションの実証で
それは不十分**と判明した。env_dirty は2つの役割を持つ:

1. **blanket reconcile**（`blanket_reconcile_if_dirty`・env_dirty-gated の O(全 locals) 巻き戻し）のゲート。
   `MUTSU_NO_BLANKET_RECONCILE`（boxing）下で既に無効＝**除去可能**。
2. **精密 reconcile**（`reconcile_locals_from_env_at_site`・carrier fallback `lives-ok{}`・Proxy STORE 等の
   サイトで全 locals を env から pull）の perf ゲート。**boxing 下でも load-bearing**。

**実証**: `MUTSU_NO_BLANKET_RECONCILE=1` のまま `reconcile_locals_from_env_at_site` を no-op 化すると
`make test` が FAIL（"wrapper scalar mutation of captured var propagates" 他）。∴ env_dirty 完全削除は
**精密 reconcile も不要にする＝精密 reconcile に依存する surface を一つずつ precise writeback / cell 共有へ畳む**
multi-session グラインドが前提（§4-A の outer cell 共有・PLAN §2-E）。

### 10.1 計測ハーネス `MUTSU_NO_PRECISE_RECONCILE`

`reconcile_locals_from_env_at_site` を no-op 化する独立トグル（`precise_reconcile_disabled()`・
`vm/vm_env_helpers.rs`）。`MUTSU_NO_BLANKET_RECONCILE=1 MUTSU_NO_PRECISE_RECONCILE=1`（double-OFF）＝
**boxing のみ＝env_dirty 削除後の到達状態**を測定する。double-OFF で残る fail が「precise writeback / cell 共有に
未変換の by-name writer」。これが 0（flaky 除く）になったら精密 reconcile + env_dirty を物理削除できる。
（旧 `MUTSU_NO_BLANKET_RECONCILE` が slice 1〜1.20 の OFF survey を駆動したのと同型のハーネス。）

### 10.2 double-OFF baseline（2026-06-22・16 → 15）

double-OFF で fail する t/（＝精密 reconcile 依存 surface）:
`closure-nested-writeback`（method 捕捉 `$output`）/ `concurrent-cell-writeback-coherence` /
`done-paren-stmt-modifier`（react done()）/ `eval-carrier-precise-writeback` /
`junction-invocant-autothread-writeback-coherence` / `let-temp` + `let-temp-restore-writeback-coherence` /
`note-gist-and-dynamic-handle` / `react-do-whenever-tap-coherence` / `react-whenever-last-next` /
`resumable-control-signal-indirect-call` / `single-store-slice-c-prime` / `supply-on-demand-closing` /
`supply-sync-infinite-emit` / `wrap-closure-capture`。カテゴリ＝carrier / 並行（supply/react）/ closure / その他。

### 10.3 slice S1（2026-06-22）— bound-Proxy substr-rw/subbuf-rw/undefine を precise 化（16→15）

`my $r := substr-rw($s, ...); $r = v` の Proxy STORE が `$s` を env に by-name 書きするが、STORE は
slot owner の1フレーム下で走るため slice 1.13 の `pending_rw_writeback_sources`（drop-on-miss）では消える。
**修正**: STORE の record サイト（`builtins_lvalue.rs` の substr-rw/subbuf-rw/undefine 分岐）で retain-on-miss の
`pending_caller_var_writeback`（新ヘルパ `record_caller_var_writeback`）にも記録し、Proxy STORE assign サイト
（`vm_var_assign_ops.rs` の local 3 サイト＋`vm_misc_ops.rs` の global by-name サイト）で
`reconcile_locals_from_env_at_site` でなく `apply_pending_rw_writeback` で精密 drain。owner slot が数フレーム上でも
retain-on-miss が運ぶ（slice 1.18 と同型）。blanket reconcile は任意 user `Proxy` STORE 用の fallback として local
サイトに残置（double-OFF harness 下では no-op）＝default build はバイト不変。pin=既存
`t/substr-rw-lvalue-writeback-coherence.t`（test 4・double-OFF で PASS 化）。make test 9890（default/double-OFF 両 PASS）。

### 10.4 slice S2（2026-06-22）— `let`/`temp` restore を precise 化（15→13）

`temp $x = 20` のブロック退出復元が `restore_let_value`（`accessors.rs`）の非cell path で `env.insert` のみ＝
slot stale（double-OFF で `$x` が 20 のまま）。restore は復元名を正確に知っている。**修正**: 非cell path で復元名を
`pending_rw_writeback_sources`（drop-on-miss・same-frame bare block 用）＋ `pending_caller_var_writeback`
（retain-on-miss・nested callee の `temp` 用）に記録し、block-exit op 3 サイト（`vm_misc_ops.rs` success/err・
`vm_control_ops.rs` err）で `reconcile_locals_from_env_at_site` の前に `apply_pending_rw_writeback` で精密 drain。
blanket reconcile は fallback として残置（harness 下 no-op）＝default build 挙動不変。cell-boxed binding は従来どおり
shared Arc 経由で slot が見えるので非該当。pin=`t/let-temp.t` + `t/let-temp-restore-writeback-coherence.t`
（double-OFF で PASS 化）。make test 9904。

### 10.5 slice S3（2026-06-22）— closure/method nested-capture writeback を precise 化（13→10）

`cap({ note "" but role { method gist { $seen = 1 } } })`（note-gist）/ `capture-out` の `$output`
（closure-nested-writeback）/ `&f.wrap(-> { $seen = True; callsame })`（wrap-closure-capture）= nested な
gist/method/wrapper closure が捕捉した outer lexical を変異するが、その lexical は当該 closure の**直接 free var で
ない**（nested 内で捕捉）ため §closure-dispatch の free_var 記録（811行）が拾えない。**修正**: ①closure-dispatch の
env-scan writeback ループ（`vm_closure_dispatch.rs`）で、書き戻す caller-visible var の値が変化した場合
`pending_caller_var_writeback`（retain-on-miss）に記録（`cell_boxing_active()` ガード＝default build の hot closure 経路
には不可。blanket reconcile が担う）。②wrap dispatch（`vm_call_func_ops.rs:518`・`vm_call_exec_ops.rs:56`）で
`apply_pending_rw_writeback` を追加（wrapper の記録を drain）。blanket reconcile は fallback として残置。
pin=`t/note-gist-and-dynamic-handle.t` + `t/closure-nested-writeback.t` + `t/wrap-closure-capture.t`（double-OFF で
PASS 化）。make test 9904。

### 10.6 slice S4（2026-06-22）— regex embedded `{ }` / `:let` の cross-frame caller writeback を precise 化（10→8）

`sub do-match($txt) { $txt ~~ / (\d+) { $tracked = +$0 } / }`（single-store-slice-c-prime test 7）/
EVAL 内 `my regex la { :let $a = 5; … }`（eval-carrier-precise-writeback test 12）= regex の埋め込み `{ }` /
`:let` ブロックが、当該マッチが走るフレーム（`do-match` の本体・EVAL'd code）の slot では**ない** caller lexical
（sub の呼び出し元／EVAL の呼び出し元の `$tracked`/`$a`）を `env` へ by-name 書きする。`writeback_match_locals`
（マッチサイト `vm_comparison_ops.rs`）は**現フレームの slot しか書かない**ため、owning slot は1つ以上上のフレームに
あり stale のまま残る（double-OFF で `$tracked=-1`/`$a=1`）。EVAL の cell-boxing（slice 1.7）は `$x=5` 型の
`SetGlobal` 書き込みは cell 経由で拾うが、`:let` は `restore_env_entries` の直接 env insert で cell リンクを断つ
ため拾えない。**修正**: マッチサイトで `writeback_match_locals` の後、埋め込み write 名（`pending_local_updates`）の
うち match-special（`$/`/`$0`…）でも現フレーム slot でもないものを retain-on-miss の
`pending_caller_var_writeback`（`record_caller_var_writeback`）に記録。owning フレームへ戻る call site の
`apply_pending_caller_var_writeback` が drain する（slice 1.18/S1-S3 と同型）。**1 修正で 2 surface 消化**（sub
carrier と EVAL carrier の双方）。`cell_boxing_active()` ガード＝default build は従来どおり blanket/精密 reconcile が
担う＝バイト不変。pin=`t/single-store-slice-c-prime.t`（test 7）+ `t/eval-carrier-precise-writeback.t`（test 12）
（double-OFF で PASS 化）。

### 10.7 slice S5（2026-06-22）— junction invocant autothread の per-eigenstate writeback を precise 化（8→7）

`my Mu $x = JB1.new | JB1.new | JB2.new; $x.a`（`method a { $cnt1++ }` / JB2 は `$cnt2++`）= invocant junction を
ユーザーメソッドで autothread し、各 eigenstate が captured-outer / `our` 変数を変異する。各 eigenstate の dispatch は
その by-name write を `pending_rw_writeback_sources` に記録するが、**次の eigenstate の dispatch がそのバッファを
上書き**するため、post-loop drain に残るのは**最後の eigenstate のソースだけ**。EARLIER eigenstate のみが書いた変数
（最後が `$cnt2` を書く間に `$cnt1` を書いた）は失われ owning slot が stale（double-OFF で `$cnt1=1`・期待 2）。同じ変数を
全 eigenstate が書く場合は最後のソースで足りるので動いていた（露出は「異なる変数を異なる eigenstate が書く」場合のみ）。
**修正**: junction loop の各 eigenstate 呼び出し後に `pending_rw_writeback_sources` ＋ `pending_caller_var_writeback` を
drain して retain-on-miss の `pending_caller_var_writeback` に accumulate（`record_caller_var_writeback`・dedup）。
loop 後の 1 回の `apply_pending_caller_var_writeback` が env（既に全 eigenstate の累積値を保持）から owning caller slot を
精密に書く。CallMethod（非mut）/ CallMethodMut（mut）両 junction path に対称適用（`$cnt++` は mut path 経由）。
`cell_boxing_active()` ガード＝default build は従来どおり blanket `reconcile_locals_from_env_at_site` の pull＝バイト不変。
pin=`t/junction-invocant-autothread-writeback-coherence.t`（6・double-OFF で PASS 化）。回帰元
`roast/S03-junctions/autothreading.t` 107/107 PASS。

### 10.8 slice S6（2026-06-22）— resumable CONTROL handler writeback を precise 化（7→6）

`my $out=''; CONTROL { default { $out ~= "[{.message}]"; .resume } }; my $w = &warn; $w.("indirect")` =
indirect call（`$w.(...)`／`&warn.(...)`＝CallOnValue）から raise された resumable な `warn` を、enclosing CONTROL の
`resume_safe` ハンドラがインライン実行する（`try_resume_safe_control_inline`・builtins_control_flow.rs）。ハンドラ本体は
installing frame の lexical `$out` を変異するが、ハンドラは locals を env から再構成 → 実行 → **変化した slot を env に
flush + env_dirty** するだけで、frame の local SLOT は warn-call サイトの blanket/精密 `reconcile_locals_from_env_at_site`
でしか更新されない＝double-OFF で no-op。**direct `warn`（ExecCall）は通り**、indirect（CallOnValue）だけ落ちた
（計測: indirect 後 `env["out"]=[indirect]` は生存・slot[0] が stale・read が slot を見て空）。**修正**: ハンドラの flush
ループで変化した名前を `pending_rw_writeback_sources`（drop-on-miss・same-frame）＋ `pending_caller_var_writeback`
（retain-on-miss・deeper raise site が installing frame まで運ぶ）に記録。両 call site（ExecCall/CallOnValue）が既に走らせる
**非ゲートの `apply_pending_rw_writeback`** が env → slot を drain する。`cell_boxing_active()` ガード＝default build は従来
どおり blanket reconcile が担う＝バイト不変。pin=`t/resumable-control-signal-indirect-call.t`（5・double-OFF で PASS 化）。

### 10.9 slice S7（2026-06-22）— react/whenever captured-outer writeback を precise 化（6→0）

`react whenever Supply.from-list(…) { …; $i++ }` の `whenever` callback が captured-outer lexical `$i` を変異する。
callback は `vm_call_map_block` 経由で by-name に env を書くが per-write 記録がなく、`exec_react_scope_op`
（vm_register_ops.rs）の post-loop `reconcile_locals_from_env_at_site`（double-OFF で no-op）でしか slot に届かない。
**修正**: react event loop の**前後で caller-frame slot-backing env 値をスナップショット**し、変化した slot だけを env
から精密に書き戻す（per-write 記録が無い by-name writer の唯一の精密化手段＝loop が書いた名前を差分で同定）。**この
1 修正で 5 surface 一掃**（`done-paren-stmt-modifier`/`concurrent-cell-writeback-coherence`/`react-whenever-last-next`/
`supply-on-demand-closing`/`supply-sync-infinite-emit`）。加えて `exec_whenever_scope_op`（:1841）の
`my $tap = do whenever $sup {…}` bind は **target_var 名が既知**なのでその slot のみ精密書き戻し
（`react-do-whenever-tap-coherence`）。同期 `from-list` emit＝単一スレッドなので tractable。`cell_boxing_active()`
ガード＝default build は blanket reconcile が担う＝バイト不変。pin=`t/done-paren-stmt-modifier.t`（4）+
`t/react-do-whenever-tap-coherence.t`（2）他（全 6・double-OFF で PASS 化）。broad supply/react/concurrency/promise/start
t/ も両モード PASS。

### 10.10 t/ surface 0 到達・**roast double-OFF sweep が 25 file の隠れサーフェスを露呈**（authoritative）

S1〜S7 で **t/ pin（16）＋ broad supply/react/concurrency/promise/start クラスタが両モード（default / double-OFF）で全
PASS**＝t/ で観測できる精密 reconcile 依存サーフェスは枯渇。**だが全 roast whitelist（1285）の double-OFF sweep
（`MUTSU_NO_BLANKET_RECONCILE=1 MUTSU_NO_PRECISE_RECONCILE=1` release）を初めて実走したところ 25 file が決定的 fail
＝t/ pin は不完全だった**（第45 で「OFF roast survey こそ authoritative」と判明したのと同型の教訓）。env_dirty 物理削除は
この roast サーフェスを全消化してから。診断は `tmp/roast-double-off.log` / `tmp/roast-double-off-fails.txt`。

**roast double-OFF 25 file（S8 着手前・カテゴリ別）**:
- **S03 演算子（andthen/orelse/notandthen）**＝user `method defined { $calls++ }` の captured-outer write。
  → **✅ S10.11 で解決**（CallDefined snapshot writeback・1 修正 3 file）。
- **S14 roles（anonymous/mixin-6e/parameterized-mixin/rw/submethods-6e・5）**＝role mixin/`rw` accessor の writeback
  （anonymous/parameterized-mixin は abort=Bad plan）。
- **S02 types（baghash/mixhash/set・3）**＝Set/Bag/Mix 演算の captured write。
- **S02 names（our/symbolic-deref・2）**＝`our`/symbolic deref の by-name writeback。
- **その他**: S02-types/lazy-lists・S04 gather（abort）・S04 terminator・S04 pointy-rw・S06 lvalue-subroutines・
  S06 named-parameters・S12 coercion-methods・S12 primitives・S12 defer-next・S17 cas-loop・S17 throttle・S32 kv。

**∴ env_dirty 物理削除（§7.4 / PLAN §2-E）は roast double-OFF 0 が前提**: ①roast 25→0 を slice で消化 →
②`blanket_reconcile_if_dirty` / `reconcile_locals_from_env_at_site` を空洞化 → ③`env_dirty` / `ensure_locals_synced` /
`saved_env_dirty` を削除 → ④`cell_boxing_active()` gate を撤去して boxing を恒久 ON（＝単一ストアの派生ビュー化）。

### 10.11 slice S8（2026-06-22）— user `.defined`（andthen/orelse/notandthen）writeback を precise 化（roast 25→22）

`my $calls=0; my class Foo { method defined { $calls++; True } }; Foo andthen meow $_` = `andthen`/`orelse`/`notandthen`
は LHS の definedness を `OpCode::CallDefined` で判定し、user `method defined` があれば dispatch する（vm.rs:2831）。その
user method が captured-outer `$calls` を by-name env 書きするが、`CallDefined` の post-call `reconcile_locals_from_env_at_site`
（double-OFF で no-op）でしか slot に届かず stale（double-OFF で `$calls=0`・期待 1）。`run_instance_method` 経由なので
per-write 記録なし→**user-method 呼び出し前後で caller-frame slot-backing env 値をスナップショット**し変化した slot だけ
精密書き戻し（react S7 と同手法）。`cell_boxing_active()` ガード＝default は blanket reconcile が担う＝バイト不変。
**1 修正で 3 roast file（S03 andthen/orelse/notandthen）消化**。pin=roast/S03-operators/{andthen,orelse,notandthen}.t
（double-OFF で PASS 化）。

### 10.12 slice S9（2026-06-22）— symbolic-deref store の carrier writeback を precise 化（roast 22→21）

`my $a_var=42; my $b_var="a_var"; lives-ok { $::($b_var) = 23 }; is $a_var, 23` = `$::($name) = v`（symbolic deref store・
`exec_symbolic_deref_store_op`）と `::('$x') = v`（indirect type lookup store・`exec_indirect_type_lookup_store_op`）は
ターゲット lexical を by-name で env 書きし `update_local_if_exists` で**現フレームの** slot だけ更新する。`lives-ok { }`
carrier 内ではターゲット slot は carrier-caller のフレームにあり、carrier writeback（`writeback_carrier_writes`）経由で
しか届かないが、両 store op は**carrier_writes にログしていなかった**ため OFF で reconcile されず stale（bare block では
動くが lives-ok 内だけ失敗）。**修正**: 両 store op の env insert 後に `note_caller_env_write(&store_name)` を呼び
carrier_writes へログ＋env_dirty（regex `:let`・`s///` writeback と同パターン）。スカラなので `:=` cell hazard なし。
default build はバイト不変（scalar reconcile は blanket の subset）。pin=roast/S02-names/symbolic-deref.t（double-OFF で
PASS 化）。**残 roast double-OFF 21**（our.t は EVAL+class+`$GLOBAL::`++ の別機構で未消化）。

### 10.13 slice S10（2026-06-22）— user Proxy STORE の captured-outer writeback を precise 化（roast 21→20）

`my $realvar="foo"; sub proxyvar($p) is rw { Proxy.new(STORE => method ($v) { $realvar = $v }, …) }; proxyvar("PRE")="BAR";
is $realvar, 'BAR'` = lvalue sub が返す user `Proxy` の STORE method が captured-outer scalar `$realvar` を by-name 書き
する（`assign_proxy_lvalue`・builtins_lvalue.rs）。ターゲット slot は assign-caller のフレームにあり、assign サイトの
blanket reconcile（double-OFF で no-op）でしか届かず stale（double-OFF で `foo`・期待 `BAR`）。**修正**: `assign_proxy_lvalue`
の STORE 呼び出し**前後で env の writeback-safe スカラのみをスナップショット**し、変化した名前を `record_caller_var_writeback`
（retain-on-miss）に記録 → assign call site の `apply_pending_caller_var_writeback` が drain。スカラ限定
（`is_writeback_safe_scalar` フィルタ）なので container/`:=` cell hazard なし。`cell_boxing_active()` ガード＝default は
blanket reconcile が担う＝バイト不変。pin=roast/S06-routine-modifiers/lvalue-subroutines.t（double-OFF で PASS 化）。
**残 roast double-OFF 20**。

### 10.14 slice S11（2026-06-22）— lives-ok container carrier の Set/Bag/Mix writeback を precise 化（roast 20→17）

`my $b=(…).BagHash; lives-ok { $b<a> = 42 }; is $b<a>, 42` = block Test fn（`lives-ok`/`dies-ok`＝`exec_call_pairs_op`
carrier）が captured-outer の Set/Bag/Mix を env 経由で変異するが、`writeback_carrier_writes` は container slot を保護的に
スキップ（barrier 任せ）するため double-OFF で stale。**修正**: carrier 実行**前後で env をスナップショット**し、変化した
slot を write-through。**対象を COW aggregate（scalar + Set/Bag/Mix）に限定**＝`Value::Set/Bag/Mix(Arc<…Data>)` は
in-place 変異で `make_mut` が COW し新 Arc へ detach する（pre snapshot は旧 Arc を保持）ので差分検出が確実。**Array/Hash
除外**（env が COW-detach コピーで interior `:=` element cell を破壊する hazard）。**★Instance も除外**＝attributes が
`Arc<RwLock>` の **shared cell** で in-place 変異するため snapshot が cell を共有し pre==post で差分検出**不可**（Instance
rw-accessor は別スライス＝S14-roles/rw.t に残置）。`cell_boxing_active()` ガード＝default はバイト不変。
回帰確認: element-bind-cell/S03-binding nested・arrays/set・mix 全 PASS（`:=` hazard なし）。pin=roast/S02-types/{baghash,
mixhash,set}.t（double-OFF で PASS 化）。**残 roast double-OFF 17**（rw.t Instance rw-accessor 含む）。

### 10.15 slice S12（2026-06-22）— lives-ok carrier writeback の eligibility を slot-overwritable に拡張（roast 17→11）

S11（§10.14）は carrier writeback を **COW aggregate（scalar + Set/Bag/Mix）に限定**したが、これは「新しい env 値の型」で
判定していたため取りこぼしが多かった。S12 は eligibility を **「上書きされる slot の*現在値*の型」**（`slot_carrier_overwritable`）
へ転換: 除外は `HashSlotRef`/`ContainerRef`（`:=` binding cell）と plain `Array`/`Hash`（env COW-detach コピーが interior
`:=` element cell を破壊する hazard）の**2 種のみ**で、それ以外（scalar/Set/Bag/Mix/Mixin/Instance/…）はすべて write-through
対象。これで `lives-ok { $a does Role[42] }`（Int(0) slot → Mixin・型変化）/ `lives-ok { $obj.r1++ }`（Instance rw-accessor・
diverged Instance）/ Pair `.kv`/`.values` rw aliasing / `$GLOBAL::`++ も拾える。Instance の in-place attribute 変異は
shared `Arc<RwLock>` cell で pre==post になり diff 不発火＝無害（genuine な値/型変化のみ write-through）。**全 roast double-OFF
sweep で 17→11（新規回帰ゼロ・残 11 全て pre-existing）＝S11 比でさらに 6 file 消化**（S02-names/our・S04 pointy-rw・S04
gather・S12 coercion-methods・S14 rw・S32 kv）。`cell_boxing_active()` ガード＝default はバイト不変。回帰確認: S03-binding
nested/arrays・element-bind-cell 全 PASS。**残 roast double-OFF 11**: S14 `does`-mixin block-scoped 再代入 4（anonymous/
mixin-6e/parameterized-mixin/submethods-6e＝block 内 `$a does R` 再代入が outer env に届かない・次スライス）／lazy-lists／
terminator／named-parameters／primitives／defer-next／cas-loop／throttle。

### 10.16 slice S13（2026-06-22）— does/but mixin の captured-outer writeback を precise 化（roast 11→7）

S14 roles の `does`/`but` mixin 4 file（anonymous/mixin-6e/parameterized-mixin/submethods-6e）。3 つの異なる取りこぼし:
1. **`Mixin` PartialEq の inner 委譲**: `lives-ok { $a does Role[42] }` で Int(0) slot が Mixin になるが、
   `Value::Mixin(Int(0),_) == Int(0)`（PartialEq が inner に委譲・value/mod.rs:2881）のため carrier の差分（`pre != cur`）が
   false → 上書き漏れ。**修正**: `carrier_writeback_changed_aggregates` の差分判定に `std::mem::discriminant` 比較を追加
   （variant 変化 Int→Mixin を value 等価でも検出）。
2. **`does`/`but` op 自体の reconcile 依存**: `$y does R`（`exec_does_op`/`exec_does_var_op`）/ `$obj but R`
   （`exec_but_mixin_op`）が走らせる role の `submethod TWEAK`/`BUILD` が captured-outer counter
   （`my $n=0; submethod TWEAK { $n++ }`）を変異するが、各 op の post-call `reconcile_locals_from_env_at_site` は
   double-OFF で no-op。**修正**: 3 op に carrier snapshot 差分（`snapshot_carrier_overwritable_env` +
   `carrier_writeback_changed_aggregates`）を `cell_boxing_active()` ガードで追加。
3. **Hash slot の型変化**: `my $a = {:x}; lives-ok { $a does role {...} }` で **Hash** slot が Mixin 化。Hash は interior
   `:=` element cell hazard で通常除外だが、**型変化（Hash→Mixin の whole replacement）は安全**。**修正**:
   `carrier_writeback_changed_aggregates` で Array/Hash slot も `discriminant(slot) != discriminant(env)`（型変化）時のみ
   上書き（同 variant の detached コピーは引き続き skip）。
`cell_boxing_active()` ガード＝default はバイト不変。**全 roast double-OFF sweep で 11→7（新規回帰ゼロ）**。回帰確認:
S03-binding nested/arrays PASS。**残 roast double-OFF 7**: lazy-lists／terminator／named-parameters／primitives／
defer-next／cas-loop／throttle（各別機構）。

### 10.17 slice S14（2026-06-22）— param `where` clause の captured-outer writeback を precise 化（roast 7→6）

`my $t=''; sub order_test(:$a where { $t ~= 'a' }, :$b where { $t ~= 'b' }) {…}; order_test(b=>2, a=>3); ok $t ~~ /a.*b/`
= named param の `where { $t ~= 'a' }` clause が binding 中に captured-outer `$t` を変異する。**計測で slot-only と確定**:
`order_test(...)` 後、closure 経由読みは `$t='ab'`（env に届いている）が direct 読みは空（caller slot stale）＝where-clause の
write は outer env に達するが caller slot は call-site の blanket pull（double-OFF で no-op）でしか refresh されない。
**修正**: where-constraint named-eval（`types/binding.rs` ~797）で **eval 前後に env の writeback-safe スカラをスナップ
ショット**し、変化した名前（`_`/param 自身を除く）を retain-on-miss の `pending_caller_var_writeback`
（`record_caller_var_writeback`）に記録 → compiled-function call site の `drain_and_reconcile_after_cached_call`
→`apply_pending_caller_var_writeback` が drain。スカラ限定（`is_writeback_safe_scalar`）で `:=` hazard なし。
`cell_boxing_active()` ガード＝default はバイト不変（where-constraint は稀パスで hot でない）。pin=roast/S06-signature/
named-parameters.t（double-OFF で PASS 化）。**残 roast double-OFF 6**: cas-loop／defer-next／primitives／lazy-lists
（laziness 別軸）／throttle（timing）／terminator（parser）。

### 10.18 slice S15（2026-06-22）— CAS block の captured-outer writeback を precise 化（roast 6→5）

`my $was='WRONG'; cas($head, { $was = $_; $next }); ok $was === Node` = `cas $var, { … }` の block が captured-outer
caller scalar `$was` を変異する。**slot-only と確定**（closure 経由読みは正・direct は stale・bare block 内のみ顕在化）:
block の write は env に届くが caller slot は cas の呼び出しサイトの blanket pull（double-OFF で no-op）でしか refresh
されない。**修正**: `builtin_cas_var`（`builtins_atomic.rs`）の block 実行（`call_sub_value`）前後で env writeback-safe
スカラをスナップショットし、変化した名前（`_`/`$_`/cas 対象名を除く）を retain-on-miss の `pending_caller_var_writeback`
に記録 → cas call site の `apply_pending_rw_writeback`（slow path L600）が drain。スカラ限定で `:=` hazard なし。
`cell_boxing_active()` ガード＝default はバイト不変。pin=roast/S17-lowlevel/cas-loop.t（double-OFF で PASS 化）。
**残 roast double-OFF 5**: defer-next（multi-dispatch）／primitives（meta compose）／lazy-lists（laziness 別軸・writeback
でない）／throttle（timing・flaky 系）／terminator（auto-curly array composer・parser）。

### 10.20 slice S16（#3437・2026-06-22）— proto-multi 候補の captured-outer writeback を precise 化（roast 5→4）

`proto method l(|){*}` ＋ `multi method l(%t,*@l){ $r ~= '%'; &?ROUTINE.dispatcher()(self,…) }` の候補 body が
captured-outer caller scalar `$r` を変異する。**真因＝proto 候補は常に slow path 経由**: `proto method` の `{*}`
（`call_proto_dispatch`・method_ctx あり）は `call_method_with_values` で候補を再ディスパッチし、候補は
compiled path（`vm_call_method_compiled`＝env_dirty/saved_env_dirty/pending を記録）でなく
`run_instance_method_resolved`（class.rs:934）を通る。この slow path は捕捉スカラ write を `merged_env`
（L1421 の「saved_env に在るキーのみ伝播」ループ）で env へ伝播するが **`env_dirty` を立てず precise writeback も
記録しない**。∴ caller の **local slot** は call-site の blanket pull でしか refresh されず、しかも slow path が
`env_dirty` を立てないため blanket pull すら発火せず **default build でも caller slot が stale**（double-OFF 限定で
ない潜在バグ）。計測＝MUTSU_DBG プローブで `merged_env["r"]=="X"`（env 伝播済）だが top-level slot 空を確認。
**修正**: `try_proto_method_body`（dispatch.rs）で `run_proto_method` の **前後に env writeback-safe スカラを
スナップショット**し、変化した名前（`_`/`$_` 除く）を retain-on-miss の `pending_caller_var_writeback`
（`record_caller_var_writeback`）に記録 → proto call site（`vm_call_method_ops`/`vm_call_method_mut_ops` の
`try_proto_method_body` 短絡 return 直前）の `apply_pending_rw_writeback` が drain。**★ungated**（`cell_boxing_active()`
ガード無し）＝precise writeback は blanket reconcile の部分集合で正しい結果を変えず、かつ default build の潜在バグ
（`say "$r"` 読みで stale）も直すため。`is $r` 読みは reconcile site を踏むので roast defer-next.t は default で偶然
通っていたが、`say` 直読みは default でも壊れていた。pin=`t/proto-multi-captured-writeback-coherence.t`（5・ON/OFF
両 PASS）。スカラ限定（`is_writeback_safe_scalar`）で `:=` hazard なし。make test 10015。
**残 roast double-OFF 4**: primitives（meta compose・writeback 候補）／lazy-lists（laziness 別軸）／throttle
（timing・flaky 系）／terminator（auto-curly array composer・parser）。

### 10.21 slice S17（#3438・2026-06-22）— custom HOW type-check メソッドの captured-outer writeback を precise 化（roast 4→3）

`class UnionTypeHOW { method type_check(Mu $, Mu \c){ ++$union-type-checks; … } method find_method(Mu $,$n){ $find++; … } }`
＋ `Metamodel::Primitives.create_type($how, …)` の custom 型に対する `Int ~~ $union` / `$union ~~ Int` smartmatch。
HOW の `type_check`/`accepts_type`/`find_method` メソッドが captured-outer caller **counter** を `++` する。**S16 と同
family**: HOW メソッドは type-check 時に slow path（`run_instance_method_resolved`）で dispatch され、捕捉スカラ write を
env に伝播するが `env_dirty` を立てず precise writeback も記録しない → caller slot が blanket pull でしか refresh されない。
**counter が read-modify-write（`++$c`）なので一層厄介**: slot が stale だと次の smartmatch 文が stale slot を env へ
re-flush し increment が文跨ぎで消える（double-OFF で find=4→1・checks=2→0 に潰れる）。**修正**: 3 dispatch サイト
（`metamodel.rs` の `accepts_type`/`find_method`、`smart_match.rs` の `type_check`）を新ヘルパー
`call_how_method_recording_writeback`（env writeback-safe スカラを HOW 呼び出し前後でスナップショット→変化名を
`pending_caller_var_writeback` に記録）経由に置換 → smartmatch op（`exec_smart_match_expr_op` 末尾）の
`apply_pending_caller_var_writeback` が drain（各文末で slot refresh＝次文の re-flush が fresh 値を使う）。**★ungated**
（S16 同様・custom-HOW dispatch 限定スコープ・precise writeback は blanket の部分集合・bare sink-context smartmatch
`$obj~~T;$obj~~U;` の default 潜在バグも直す）。pin=`t/custom-how-type-check-writeback-coherence.t`（6・ON/OFF 両 PASS）。
**残 roast double-OFF 3**: lazy-lists（laziness 別軸）／throttle（timing・flaky 系）／terminator（auto-curly composer・parser）。

### 10.22 slice S18（#3439・2026-06-22）— EVAL carrier の scalar 再代入 writeback を container slot にも適用（roast 3→2）

`my $z = []; EVAL q'$z = do { 1 } + 2;'; is $z, 1` = EVAL carrier が captured-outer caller scalar `$z` を **container を
保持していた slot に** scalar 再代入する。**真因＝`writeback_carrier_writes`（vm_env_helpers.rs）の reconcile 適格判定が
「古い slot 値が scalar か」だった**: `my $z = []`（slot=Array）→ EVAL の `$z = 1`（env=Int）writeback 時、slot の現在値が
Array なので scalar 分岐をスキップ → container 保護分岐（COW `:=` cell hazard 回避で上書き拒否）→ blanket reconcile
（double-OFF で no-op）頼みになり slot が stale Array のまま。**この case は誤って「parser（auto-curly）」と分類されていたが
実体は EVAL carrier scalar writeback**（normal は z=1・auto-curly parse は正常・double-OFF だけ z=[]）。**修正**: 適格判定を
**新しい env 値が scalar か**に変更（`is_writeback_safe_scalar(&env_val)` ＋ slot が `:=` bind cell＝`ContainerRef`/`HashSlotRef`
でないこと）。scalar 新値は container COW-copy hazard を持たないので、slot が以前 container を持っていても安全に上書き可。
`my @e = …; EVAL q'@e = 7,8'`（container→container）は新値が container なので従来の保護分岐のまま＝非該当。`:=` bind cell
（`t/element-bind-cell.t`・`S03-binding/nested.t`・`arrays.t`）両モード PASS で回帰なし。pin=`t/eval-carrier-scalar-writeback-coherence.t`
（6・ON/OFF 両 PASS）。**この修正は一般的**（任意の `EVAL q'$x = scalar'` で container を持っていた slot を救う）。
**残 roast double-OFF 2**: lazy-lists（laziness 別軸）／throttle（timing・flaky 系）。

### 10.23 S19 — lazy-lists の真 lazy 化（gather grep/map・PR 進行中・2026-06-22）

§10.19 で「真の laziness blocker」とした `S02-types/lazy-lists.t` 14/16（`grep is lazy` / `map is lazy`）を **真 lazy 化で解決**
（double-OFF でも 27/27 PASS）。**真因＝blanket reconcile は load-bearing ではなかった**: gather は normal/double-OFF の
**両方で eager force されていた**（§10.19 の「take counter=10」計測は正しいが結論が逆）。normal で `$was-lazy=1` だったのは
gather tail `$was-lazy=0` の captured write が **write-loss していた**だけ（ON は write-loss で偶然 raku 一致＝doc §7.3 教訓#3）。
∴ blanket reconcile が laziness を維持していたのではなく、reconcile が write を伝播した瞬間に露出した。修正は writeback でなく
**`gather { … }.grep/.map` を eager force せず lazy pipe で pull する**こと。

真の eager-force 元は **4 サイト**（すべて「`.lazy` gather/infinite spec を map/grep で lazy pipe 化する」例外が
`lazy_pipe.is_some() || is_infinite_spec()` のみで **gather coroutine を除外していた**）:
1. `is_lazy_pipe_source`（methods_collection.rs:69）— gather を lazy pipe source と認めず → `ll.needs_vm_lazy_dispatch()`
   （`is_from_gather() || is_infinite_spec()`）を OR 追加。
2. `try_native_method`（vm_native_dispatch.rs:41）— gather+map/grep を native impl に流して materialize → `is_lazy_pipe_source`
   ゲートに統一して defer。
3. `vm_call_method_ops.rs:733` / 4. `vm_call_method_mut_ops.rs:474`（method dispatch 前の force ブロック）/ 加えて
   `runtime/methods.rs:3157`（slow path force）— map/grep 例外に `|| ll.is_from_gather()` を追加し gather を force しない。
インフラ（`pull_source_element` の `Value::LazyList → force_lazy_list_vm_n(ll, idx+1)`・vm_helpers.rs:1131）は既存＝coroutine を
1 take ずつ resume できる。plain gather（非 `.lazy`）も Rakudo では grep+slice で lazy なので `is_from_gather()` で判定（lazy
マーカーは sub return で失われる＝`is_genuinely_lazy()` は不可）。pin=`t/lazy-gather-grep-map-laziness.t`（8・両モード PASS）。

**★残 double-OFF サーフェス（authoritative survey・全1285 release・S19 後）= 2（writeback 候補ゼロ・新規回帰ゼロ）**:
- **`S04-statements/lazy.t`**（**新規同定・pre-existing**）= `$var := lazy { $w++; 42 }; is $var,42` が double-OFF で **hang**
  （exit 124・proper harness でも）。**baseline（S19 前）でも同 hang**＝私の変更と無関係。**LazyThunk の force が double-OFF で
  無限ループ**する別軸（gather でなく `lazy { block }`＝`Value::LazyThunk`）。§10.19 の「残 2」が見落としていた（handoff survey が
  proper harness でなく lazy-lists/throttle のみ計測した疑い）。**次の最有力 double-OFF blocker**。
- **`S17-supply/throttle.t`** 3,4 = timing flaky（§10.19 のまま）。
（survey で出た spurt.t / gb18030・gb2312・shiftjis-encode-decode.t は **harness artifact**＝raw `prove -e mutsu` が fudge
ラッパー無しで誤検出・`run-roast-test.sh` 経由では全 PASS。spurt.t は stale temp-file flaky。double-OFF blocker でない。）

**★full-consumption-through-pipe の gather tail writeback は double-OFF で残**（`my @a=gather{…;$t=1}.grep(*>1); ok $t`）:
直接 gather force は伝播するが、lazy pipe 経由（grep）だと内側 `force_lazy_list_vm_n` の `reconcile_caller_after_lazy_force` が
pipe 中の誤フレームに drop-on-miss で drain して落とす。**lazy-lists.t（slice・full-consume せず）では踏まれない**ので S19 では
未修正（pin からも除外）。env_dirty 削除前の writeback slice 候補（retain-on-miss 化 or 外側 force での drain）。

### 10.19 ハンドオフ — roast double-OFF 残 2（S18 後・2026-06-22）

S4〜S18（16 slice）で **roast double-OFF surface 25 → 2**。全 whitelist（1285）の double-OFF sweep
（`MUTSU_NO_BLANKET_RECONCILE=1 MUTSU_NO_PRECISE_RECONCILE=1` release）で確定した残 2（**両方 writeback でない別軸**・新規回帰ゼロ）:

| file | サブテスト | 種別 | 次の一手 |
|---|---|---|---|
| `S02-types/lazy-lists.t` | 14,16 | **真の laziness blocker** | `grep is lazy`/`map is lazy`。**精密切り分け済（下記）**＝blanket reconcile が laziness 維持に load-bearing。precise writeback では解けない。env_dirty 削除前に **grep/map が `.lazy` gather を eager force しない真 lazy 化**が必要。 |
| `S17-supply/throttle.t` | 3,4 | **別軸（timing）** | `.5〜.8 秒差`のタイミングアサート。flaky 系。env_dirty と無関係の可能性大（決定的 pin 不可なら削除ブロッカーから外す）。 |

**★lazy-lists の精密切り分け（重要・次セッション必読）**: `gather { take $_ for 0..^$n; $was-lazy = 0 }.lazy` を
`grep(*.is-prime)[^3]` で消費し `ok $was-lazy`（lazy なら gather 未完で `$was-lazy=0` 行が走らず 1 のまま）。**blanket だけ
OFF でも precise だけ OFF でも `$was-lazy=0`（両方 ON の default のみ 1）**。take counter を仕込んで計測すると、double-OFF /
blanket-OFF では gather が **eager force される**（takes=10）が normal は lazy（take 計上前に grep が 3 個取って止まる）。
∴ **precise writeback（snapshot 差分）では解けない＝blanket reconcile が「`.lazy` gather を eager force しない」laziness 維持に
load-bearing**。env_dirty 物理削除はこの真 lazy 化が前提。再現＝`my $w=1; sub mk($n){gather{take $_ for 0..^$n; $w=0}.lazy};
$w=1; my \one=mk(10); one.grep(*.is-prime)[^3]; say $w`（normal=1 / double-OFF=0）。

**∴ writeback 候補は枯渇**（S16 proto-multi・S17 custom-HOW・S18 EVAL container-slot scalar writeback で消化）。**残 2 は
両方 writeback でない別軸**（laziness/timing）。**★terminator は parser 誤分類で実体は EVAL writeback だった（S18 で消化）＝
ハンドオフの「別軸」分類は要再検証の教訓**。**次セッション着手順**: ①**lazy-lists の真 lazy 化**（grep/map on lazy gather・
確定した最有力ブロッカー）→ ②throttle が flaky か最終確認 → ③§7.4 / PLAN §2-E（`env_dirty` 物理削除：
`blanket_reconcile_if_dirty`/`reconcile_locals_from_env_at_site` 空洞化 → `env_dirty`/`ensure_locals_synced`/`saved_env_dirty`
削除 → `cell_boxing_active()` gate 撤去）に着手できる。診断ファイル＝`tmp/roast-double-off-final-fails.txt`。
**★S16/S17 の共通教訓**: slow path（`run_instance_method_resolved`）経由の dispatch（proto-multi 候補・custom HOW メソッド）は
捕捉 write を env に伝播するが `env_dirty` を立てない＝blanket pull すら発火せず default build でも slot stale。修正は dispatch
前後の env scalar スナップショット差分→`pending_caller_var_writeback` 記録 → 呼び出し op で drain。**ungated でよい**（custom
dispatch 限定スコープ・blanket の部分集合・default 潜在バグも直す）。**slot-only 判定法**＝同変数を `{ $x }()`（closure＝env 読み）と
direct（slot 読み）で比較し差があれば slot-only＝snapshot 差分で解ける。
