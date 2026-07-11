# GC post-3a ロードマップ — hardening / Track B 残 / 層3b NaN-boxing / 層3c

Status: Draft（2026-07-05）
Related: [ADR-0001](adr/0001-gc-strategy-and-phasing.md), [ADR-0003](adr/0003-default-on-gc-trigger.md),
[gc-level1-detailed-design.md](gc-level1-detailed-design.md), [ADR-0004 (JIT)](adr/0004-jit-strategy.md), [PLAN.md](../PLAN.md) §2/§5

この文書は **層3a（cycle collector on Arc）完了後** の GC まわりの残作業を、
着手条件（トリガ）と受け入れゲート付きのスライスに落とすロードマップ。
ADR-0001 のフェーズ順序（3a → 3b → 4=JIT → 3c 判断）は変えない。

## 0. 現在地（2026-07-05）

- **層3a 完了**: `MUTSU_GC` 既定 on（ADR-0003 §5）。全 safepoint 種別 emit、cooperative
  cross-thread STW、dead sweep、adaptive threshold トリガ、CI gc-stress blocking。
- **Track B**: スライス 1（atomic ストア要素セル #4241）/ 2（state aggregate cell 書き戻し #4245）/
  3（state 集約の全モードセル化 #4251）まで完了。
- **受け入れ実測の残差**: bench-class GC-on +8%（= `Value` clone/drop 交通量への per-drop
  buffered-bit 分岐。ユーザー判断で許容済み、**回収は層3b の仕事**）。
- perf 現況: fib(25) 1.0x / method-call 2.7x / bench-class 2.3x / bench-fib 3.2x（対 raku）。

## 1. 層3a hardening バックログ（随時・小粒）

> 原則: **実測で問題が出たときにやる**。予防的な複雑化はしない（1a を単純に保つことが
> JIT を載せやすくする、という ADR-0001 §3-8 の連鎖を壊さない）。

| ID | 内容 | 着手トリガ | ゲート |
|----|------|-----------|--------|
| H1 | GC オーバーヘッド継続観測: `MUTSU_VM_STATS` の gc カウンタ＋ベンチ表の定期再計測（`benchmarks/run-all.sh`） | 各大型 PR 後 | fib/int で `gc_candidate_pushes == 0` 維持、method-call/bench-class の GC-on 残差が悪化しない |
| H2 | `Instance` 本体 node と `attributes` cell の分割（設計書 §13-1、唯一の未決） | 循環 reclaim の精度不足か attrs 経由のリーク/過剰トレースが実測された場合のみ | 対象 pin（attr 循環）で reclaim 数一致・VERIFY green |
| H3 | STW quiescence の未ラップ blocking site 削減: timeout→requeue fallback の発火率を可視化し、発火源を `spawn_user_thread`/safe-region ラップへ移す | `MUTSU_GC_LOG` で fallback 発火が定常的に観測された場合 | churn 系ベンチで fallback 発火 ≈ 0、pause_ns_max 有界 |
| H4 | root 消費型 full-root VERIFY モード（§11 step 11 の hardening リスト: AsyncSocketConnMap 等の `Value` edge を root visitor に包含） | tracing 型の検証/デバッグ機能が必要になったとき（3b の flip 検証にも有用） | VERIFY(full-root) が gc-stress で green |
| H5 | Level 1b: background / incremental collect（設計書 §12 で 1a から除外したもの） | `pause_ns_max` が実ワークロードで問題化した実測がある場合のみ。**新 ADR 必須** | — |

## 2. Track B 残スライス（小・GC キャンペーンの尻尾)

スライス 1〜3 で「構造 = COW スナップショット / 要素値 = セル in-place」テンプレートは
確立済み。残りは全て**このテンプレートの適用漏れ**で、各スライスは
「raku 突き合わせ pin + make test + 関連 roast」をゲートとする。

- ✅ **T4: multidim cas / 残 atomic 経路のセル化 — 完了（2026-07-11）**。多次元 `cas` を
  1-dim `cas` と同一の要素セルテンプレートに統一（top-level slot のセルロック内で
  nested compare+set・COW はセル内側のみ）。read/assign の multidim 経路
  （`multi_dim_index_read` / `index_array_multidim` / assign 各 arm）に container 限定の
  セル透過を追加。★スカラを保持するセルは deref しない（scalar-as-list ラップが
  セル自体を返すことに raw `\target` aliasing が依存 — `t/multislice-lvalue.t` test 10）。
  pin = `t/cas-multidim-cells.t`。
- ✅ **T5: `ContainerRef` の typed-constraint 対応 — probe 完了・漏れ修正済み（2026-07-11）**。
  probe 結果: 通常代入・cross-thread 代入・scalar cas・hash 3-arg cas（代入 desugar）は
  既に enforced。漏れ = **cas 専用 builtin がセルへ直書きする経路**（array 3-arg /
  multidim / hash code形）＋ array code形 cas 未実装（`Unknown function`）。修正 =
  `check_atomic_elem_type`（宣言制約 or store node 埋め込み `value_type`、native int 型は
  Int/Num 許容）を全 cas 書き込みに配線 — raku は compare 失敗でも swap 値を型チェック
  するのでロック前に実施。array code形 cas（1-dim/multidim）も celled retry-loop で新規実装
  （`builtins_atomic_cas_code.rs`）。pin = `t/cas-typed-constraint.t`（16 tests・raku 一致）。
  ★残 gap（別軸・非セル）: un-shaped `my Int @m` への `@m[1;1]` 中間 autoviv を raku は
  X::TypeCheck::Assignment で拒否、mutsu は素通し（multidim autoviv の中間型チェック欠落）。
  フル `CellValue` 型の導入は 3b と同時に判断（Value 表現を触るのは 1 キャンペーン 1 回の原則）。
- ✅ **T6: 非 state の escaped aggregate 捕捉の残件調査 — probe 完了・漏れ修正済み（2026-07-11）**。
  probe 結果: plain/compound element assign・push/pop・whole reassign・cross-thread は
  全経路 raku 一致。漏れ = **closure の free-variable 解析が element incdec
  （`%h{$k}++`/`@a[$i]--`）と element `:delete` を集約の使用として認識しない**
  （`op_container_mutate_const_idx` に PostIncrementIndex 系 / DeleteIndexNamed が
  未登録）→ それだけが唯一の使用である closure は集約を捕捉せず、escape 後の
  変異が消失。opcode 5 種を登録して修正。pin = `t/escaped-closure-elem-incdec-delete.t`
  （12 tests・raku 一致・threads 込み）。
- **対象外（変更なし）**: 高競合の並行「構造」挿入の lost-update は仕様外のまま
  （real rakudo は同形で MoarVM oops。mutsu は不壊で優位 — スライス 2 の結論を維持）。

## 3. 層3b: NaN-boxing（JIT の地ならし・GC 後の本丸）

### 3.1 目的と回収対象

- `Value` 48B → **8B**。bench-class の ~50%（`Value::clone`/`drop_in_place`/`Vec::clone`、
  PERFORMANCE.md）と、GC default-on の bench-class +8% 残差（per-drop 分岐が乗る母数の交通量）を
  まとめて回収する。
- JIT（ADR-0004）の前提: 8B 固定幅・タグ判定が数命令・レジスタに乗る。**3b を JIT より先に行う**
  （ADR-0001 の順序 3a→3b→4）。

### 3.2 表現方針（ADR-0001 §3-6 の確認 + 具体化）

- 64bit NaN-box: `f64` は生値、非 f64 は quiet-NaN 空間のタグ + 48bit payload。
  - inline scalar: `Int`(小整数)・`Bool`・`Nil`・`Package/Symbol`・小さな enum 系。
  - pointer tag: コンテナ系は **`Gc<T>` ポインタをそのまま payload に**（cycle collector は
    non-moving なのでポインタ安定 — mark ビット再配置問題が起きない、ADR-0001 §3-6-3b）。
  - boxed fallback: `BigInt`/`BigRat`/`Complex`/`Rat`/Range 系/その他大型 variant は
    ヒープ箱（`Gc<BoxedValue>` ないし `Arc`）に逃がす。`Str(Arc<String>)` は pointer tag。
- i64 全域が payload に入らない点に注意: 小整数 (48bit 級) は inline、外は BigInt 箱行きか
  `Gc<i64>` 箱。**どちらにするかは実装時に int-arith ベンチで決める**。

### 3.3 スライス分割（5b の教訓: 一斉 flip は 479 エラー/型 → 段階必須）

1. **3b-0 API 壁**: `Value` の直接 `match`/構築サイトを accessor/constructor
   （`as_int()`/`Value::int()`/`is_*()` 等）経由に機械的に寄せる。enum のまま挙動不変。
   ここが最大の面積（数千サイト）だが完全に機械的・並列スライス可能。
   ゲート: 各スライスで make test byte-identical。
   **着手済み（slice a）**: 壁 API 本体 `src/value/view.rs`（`ValueView<'a>` 全 variant
   ミラー + `view()` + scalar accessor/constructor）、設計と移行レシピ =
   [nanbox-3b0-api-wall.md](nanbox-3b0-api-wall.md)、ratchet =
   `scripts/check-value-wall.sh`（`make test` 組込・baseline 17757 from 単調減少のみ許可）、
   exemplar 移行 = `vm_arith_ops.rs` / `vm_loop_writeback.rs` / `vm_native_map.rs`。
2. **3b-1 表現スイッチ**: `Value` の内部表現を `#[cfg]`/newtype で NaN-box 実装に差し替え
   （accessor の中身だけ変わる）。`value_size_guard` を 48→8 に更新。
   ゲート: make test + full roast + gc-stress green、GC カウンタ不変（型フィルタの
   スカラ/コンテナ判定がタグ判定に変わるだけ）。
   **step A（newtype seal・byte-identical）は完了（2026-07-11・ADR-0005 §2.3 参照）**:
   `struct Value(ValueRepr)` + private repr + constructor shim。残 = step B（表現差し替え・
   ADR-0005 Accepted 待ち）。
3. **3b-2 交通量刈り**: clone/drop が memcpy 8B になった後の残ホットスポット
   （`attributes.to_map()` 毎回クローン、`call_compiled_method` の `format!` 等、
   PLAN §5 の既知項目）を profile 順に。
- ゲート（3b 全体）: int-arith 2x・fib +30%（PERFORMANCE.md Phase 4a の期待値）、
  bench-class の `Value` clone/drop share 50% → 20% 以下、GC-on bench-class 残差 +8% → +3% 以下。

### 3.4 リスクと対策

- **Send/Sync**: 現行 `Arc`/`Gc` ベースの cross-thread モデルを変えない（payload は
  ポインタのまま）。
- **Miri/UB**: タグ操作は 1 モジュールに閉じ込め、Miri でユニット検証（5a/5b と同じ手順）。
- **flip の巨大さ**: 3b-0 の API 壁が終わっていれば 3b-1 は小さい。壁が終わる前に
  3b-1 に手を出さない（5b の flip 失敗の再発防止）。

## 4. 層3c: biased reference counting

- **凍結**（ADR-0001 §3-6-3c のまま）。着手トリガ: **JIT(J3/J4) 完了後の profile で
  `Arc`/`Gc` の atomic inc/dec が上位に残った場合のみ**。JIT が int ループを
  ネイティブ化すれば hot path から refcount が消える見込みが高く、その場合は永久に不要。

## 5. 順序と依存（全体）

```
[完了] 3a GC default-on ──┬── H1..H4 hardening（随時・小粒）
                          ├── T4..T6 Track B 尻尾（小・並行可）
                          └── 3b NaN-boxing（3b-0 → 3b-1 → 3b-2）
                                    └── 層4 JIT（ADR-0004: J1 → J2 → J3 → J4）
                                              └── 3c biased RC（実測トリガでのみ）
```

- Batteries（PLAN §1）はこの全系列と並行可能（Value 表現に依存しない）。
- threaded dispatch（PLAN §5 Lever 3）は **JIT と重複するため凍結**を提案
  （ADR-0004 §4 J0 参照）。
