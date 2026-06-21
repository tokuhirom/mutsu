# Captured-outer lexical cell 共有 — 実装プラン（Sub-slice 1b+ / env_dirty 削除への substrate）

> **Status:** SLICE 1〜1.19 DONE（〜2026-06-21・第41〜47セッション）。§6 第1スライス（named-sub 捕捉
> scalar decl-site cell 化）＋§7.1〜7.1l（metaop-thunk `Mu`／carrier single-frame／EVAL carrier multi-frame／
> captured-outer container `@`/`%` cell 化／`X`-cross metaop thunk／nested-method capture／cross-thread shared-var／
> object 添字代入 invocant slot／substr-rw・subbuf-rw／zip short-circuit／map LAST phaser／undefine() lvalue）
> ＋slice 1.17（proto state-%cache）／1.18（caller-frame by-name write）／**1.19（param default self-scoping＝code.t 8）**
> を実装。各 pin が **blanket reconcile ON/OFF 両方で PASS**。**OFF roast survey（authoritative・§7.2a）= 初回 13 → 残 1**。
> **残 1（純 writeback でない）＝destruction.t 3（DESTROY captured-write の cell-detachment・§7.2c に真因確定・専用スライス要）**。
> ＋別軸 2＝lazy-lists.t（laziness・L 系）／IO-Socket-Async.t（flaky）。
> 次＝§7.2c（destruction.t の cell write-through・案A）or env_dirty 削除の最終段（§7.4・`blanket_reconcile_if_dirty` 空洞化）。
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
| S12-construction/destruction.t | 3 | DESTROY captured-write cell-detachment（§7.2c） | 🔴 専用スライス要 |
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
- **残り 1（純 writeback でない＝別軸・第47で真因を cell-detachment と特定）**:
  - **destruction.t 3**: `submethod DESTROY { $in_destructor++ }` の GC 時 captured write が `await start { loop {…} }` の
    後で top-level slot に届かない。**第47セッションで最小再現＋真因を確定**（下記 §7.2c）。当初の「GC タイミング依存」は誤り＝
    **決定的**（`MUTSU_NO_BLANKET_RECONCILE=1` で確実に fail）。真因は単純な writeback gap でなく **cell detachment**（DESTROY が
    slow-path `run_instance_method_resolved` 経由で実行され、その env merge が top-level slot と共有していた `ContainerRef` cell を
    plain Int に置換して切り離す）＋ **await 境界後に top-level frame が drain しない**の二重問題。precise-writeback の単純横展開では
    解けず、専用スライス要（§7.2c）。
  - ＋別軸: lazy-lists.t 24-26（laziness バグ・L 系）／IO-Socket-Async.t 5,7（flaky）。

**残 1（destruction.t GC＝純 writeback でない別軸）＋別軸 2（lazy-lists laziness・IO-Socket-Async flaky）
全消化後に §7.4（env_dirty 削除）が射程**。純 writeback コヒーレンスのサーフェスは slice 1.18 で枯渇、
露出バグ（code.t scoping）は slice 1.19 で消化。

### 7.2c 🔴 destruction.t 3 — DESTROY captured-write の cell-detachment（第47セッション調査・未解決・専用スライス要）

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
        my $bar = Bar.new unless +@order;   # 条件付き生成が鍵
        last if $a && @order;
    }
};
say "a=$a";     # OFF: a=0（ON/raku: a=1）
```

**切り分け結果（min1〜min10）**:
- `$a` 単独の DESTROY write（1クラス）は OFF でも **正常**（min1/min5/min6）。同一 DESTROY が複数スカラ／スカラ＋配列を
  書いても正常（min5/min6）。異なる DESTROY が別々のスカラを書いても**毎イテレーション両方生成なら**正常（min7/min9）。
- **fail するのは**: 複数の DESTROY-bearing クラスがあり、片方（`@order`）が**条件付き生成**（`unless +@order`）で、
  loop 脱出イテレーションでは `$a` を書く DESTROY だけが発火する構成（min4/min8/min10）。継承は不要。

**真因（計測で確定）**:
- `peek()`（`$a` を captured-outer として env から読む sub）は **env=1** を見る＝**env は正しく蓄積**。
- top-level frame の `$a` を読む直接 interpolation は **slot=0（stale）**。∴ top-level `my $a` は OFF の cell-boxing で
  **local slot（cell）に box** され、env と乖離。
- DESTROY は slow-path `run_instance_method_resolved`（class.rs:934）経由。その末尾の env merge
  （`merged_env.insert_sym(*k, v.clone())`・class.rs:1421-1429）が、saved_env に存在する captured var を**新しい plain Int に
  置換**して `self.env = merged_env`。これで env の `$a` が top-level slot の `ContainerRef` cell（Arc）から**切り離される**
  （cell には旧値 0 が残る）。
- かつ `await start {…}` の後、top-level frame で `pending_*_writeback` を drain する経路が**無い**（drain したのは loop 内の
  start-block frame だけ＝`locals=["foo","chld"]` で `$a` slot 無し→retain しても top-level まで運ばれない）。

**試した修正と不成立の理由（第47・revert 済）**:
1. DESTROY merge で変更 captured scalar を `pending_rw_writeback_sources`（drop-on-miss）に記録＋loop 境界
   （vm_control_ops.rs:256）で `apply_pending_rw_writeback` → ❌ slot が現フレームに無く drop。
2. `pending_caller_var_writeback`（retain-on-miss）に変更 → ❌ retain はするが await 後 top-level frame で drain が走らず、
   かつ slot を見つけても **cell でなく plain 代入**になり cell 共有を壊す。
3. 計測で `find_local_slot` が全中間フレームで `$a` slot を見つけられない（`locals=[]`/`["foo","chld"]`）ことを確認＝
   top-level slot は別フレーム。

**∴ 専用スライスの設計方針（次セッション）**: 二択。
- **(A) cell write-through**: DESTROY merge（class.rs:1421）で saved_env[k] が `ContainerRef` cell のとき、plain 置換でなく
  **cell に write-through**（`set_container_value` 等）して cell を merged_env に保持→top-level slot の cell（同一 Arc）が
  即座に更新値を見る。これが本筋（cell 共有の本来の挙動）。`@`/`%` container は元々 Arc 共有なので非該当、scalar cell のみ。
- **(B) await/loop 境界での top-level drain**: `await` の call-site と loop 境界で `pending_caller_var_writeback` を
  retain-on-miss で運び、所有フレームまで到達させる。ただし cell detachment（A）が先に要る（drain しても plain 代入では
  cell を壊す）。
**(A) が本命。** cell write-through が入れば env↔slot が乖離しなくなり、§7.4（env_dirty 削除）の前提にも直結する。

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

1. このプラン §6.1 案A の配線可否を調査（`compute_free_vars` から named sub の `CompiledCode` 到達可能か）。
2. `t/captured-outer-cell-sharing.t` を先に書く（multi-frame `$acc` accumulation 他・raku 期待値で固定）。
3. 検出＋ボックス化を実装（§6.1-6.2）。
4. §6.4 で検証（blanket 外して pin が通る → 戻して make test）。
5. PR（feature branch・auto-merge）。CI 後、§7 スライス2へ。
