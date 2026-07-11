# ADR-0005: NaN-boxing 表現スイッチ（3b-1）のエンコーディング選択と newtype seal 統合

- **Status**: Proposed（承認待ち）
- **Date**: 2026-07-07
- **Deciders**: tokuhirom, Claude
- **関連**: [ADR-0001](0001-gc-strategy-and-phasing.md)（層3b の位置づけ・順序 3a→3b→4）,
  [ADR-0004](0004-jit-strategy.md)（JIT は 8B 固定幅 `Value` を前提）,
  [gc-post-3a-roadmap.md](../gc-post-3a-roadmap.md) §3.2/§3.3（表現方針・スライス分割）,
  [nanbox-3b0-api-wall.md](../nanbox-3b0-api-wall.md)（3b-0 API 壁・移行レシピ・§6 未決事項・§7 seal）,
  [PLAN.md](../../PLAN.md) §5 Lever 2

> 3b-0（API 壁）は完了済み — `src/value/` 外の直接 `Value::<Variant>` 参照は **0**（ratchet
> `check-value-wall.sh` が `make test` で保証）。本 ADR は 3b-1（`Value` 48B→8B の表現スイッチ）の
> **唯一のブロッキング設計判断＝エンコーディング選択**を決め、あわせて wall doc §7.1 の
> variant-privacy seal を 3b-1 に統合する方針を確定する。実装は本 ADR が Accepted になってから。

---

## 1. Context（背景）

- `Value` は現在 48B の `enum`（~50 variant、`value_size_guard` テストで `<= 48` を固定）。
  bench-class の ~50% が `Value::clone`/`drop_in_place`/`Vec::clone`（PERFORMANCE.md）、
  GC default-on の bench-class +8% 残差も同じ交通量に乗る。3b は `Value` を **8B 固定幅**の
  NaN-box に変え、これらをまとめて回収する（roadmap §3.1）。JIT（ADR-0004）は 8B 固定幅・
  タグ判定数命令・レジスタ搭載を前提にしており、3b はその地ならし。
- 3b-0 で全呼び出し側は `view()`/`ValueView`/accessor/constructor 経由に統一済み。したがって
  3b-1 は原則 **`src/value/` の内部（表現モジュール）だけ**を書き換える。壁が保たれていれば
  外部呼び出し側の変更は不要（これが 3b-0 の狙い）。
- 3b-0 完了時点で判明した重要事実（本 ADR の前提を 1 つ補正する）:
  wall doc §7.1 が「small・mechanical・~zero churn」と見積もっていた **variant-privacy seal の
  module-boundary 方式（private サブモジュール ＋ `pub use` 再エクスポート）は実際には seal に
  ならない**。Rust の enum variant は enum 自体の可視性を継承し、型の再エクスポートで variant も
  外部から到達可能になる（`pub use repr::Value;` の後 `Value::Int(3)` が外部モジュールで
  コンパイル可能なことを実証済み）。同一クレート内なので `#[non_exhaustive]` も無効。
  コンパイル時に真に seal できる唯一の手段は **newtype ラッパー** `struct Value(ValueRepr)`
  （private field ＋ private `ValueRepr`）であり、これは `src/value/` 内部の直接 variant 参照
  **1293 箇所**を `.0` 経由へ書き換える大規模リファクタになる。

### 3b-0 で確定した variant 3 分類（roadmap §3.2・wall doc §2 の具体化）

現行 enum を payload 形状で分類する（8B box の格納先を決める）:

- **inline scalar**（ヒープなし・box payload に直接デコード格納、view は by-value）:
  `Int`（小整数）・`Bool`・`Nil`・`Whatever`・`HyperWhatever`・`Num`（f64 生値）・
  `Package(Symbol)`。**注意**: `Range*`(2×i64)・`Rat`/`FatRat`(2×i64)・`Complex`(2×f64) は
  payload が 48bit を超えるため inline 不可 → boxed。
- **single-pointer（pointer tag）**（`Gc<T>`/`Arc<T>` の生ポインタを payload に）:
  `Str`・`Array`・`Hash`・`Set`・`Bag`・`Mix`・`Sub`・`WeakSub`・`LazyList`・`ContainerRef`・
  `Seq`/`HyperSeq`/`RaceSeq`/`Slip`・`Junction`・`Regex`・`BigInt`・`Promise`/`Channel`・
  `LazyThunk`・`Instance`（下記の付随フィールド問題あり）。
- **heap-boxed struct**（多フィールド → 1 ヒープ箱に退避）:
  `GenericRange`・`Range*`・`Rat`/`FatRat`/`BigRat`/`Complex`・`Pair`・`ValuePair`・`Enum`・
  `RegexWithAdverbs`・`Version`・`Capture`・`Uni`・`Proxy`・`ParametricRole`・`CustomType`・
  `CustomTypeInstance`・`Scalar`・`Mixin`(2 ポインタ)・`LazyIoLines`・`HashEntryRef`・
  `CompUnitDepSpec`・`Routine`(3 Symbol)。

**付随フィールド問題**（wall doc §6 の未決事項）: `Array(Gc, ArrayKind)`・`Set/Bag/Mix(Gc, bool)`・
`Instance{Symbol, Gc, u64}` はポインタ以外の小フィールドを持つ。ポインタ＋付随ビットを 48bit box に
同居させるか、付随ビットを pointee 側へ移すかの選択がある（§3.3）。

## 2. Decision（提案する決定）

### 2.1 エンコーディング = **pointer-favored（NuN-boxing 型）** を推奨採用

box は 64bit の 1 ワード。**ポインタは低位ビットにクリーンに格納**し、`f64` は
**オフセット・エンコード**（load/store 時に定数バイアスを加減）で quiet-NaN 空間の外へ退避する。
inline 小整数・タグは残りのビット空間に割り当てる（正確なビット配分は実装時に確定・§3.1）。

**pointer-favored を選ぶ理由**:

1. **mutsu のホットパスはポインタ支配的**。method dispatch・コンテナアクセス（`Str`/`Array`/
   `Hash`/`Instance`/`Sub`）が最頻で、pointer-favored はこれらの deref を **マスク操作なし**
   （生ポインタがそのまま）にできる。
2. **移行コストが最小**。生ポインタが payload そのものなので `&self.0` を `&Arc<T>`/`&Gc<T>` へ
   transmute でき、`ValueView` の pointer variant フィールドを **現行の `&'a Arc<T>`/`&'a Gc<T>`
   のまま維持**できる（wall doc §2/§7.2）。→ `view.rs` の変更が最小、外部呼び出し側の変更ゼロ。
3. **ヘッドラインベンチは Int 律速**。int-arith・fib は `Int`（両エンコーディングとも inline）を
   使い、`Num`(f64) は二次的。f64 のオフセット加減は 1 ALU 命令で、非主要パスに乗る。

**却下する対案 = NaN-favored（native double）**: `f64` を生値で持ち、非 f64 を quiet-NaN payload に
タグ格納する（JSC/LuaJIT 方式）。`Num` 演算がゼロコストになる利点はあるが、(a) ポインタ deref に
マスク（AND）が要る、(b) `&self.0` がクリーンな `&Arc<T>` にならないため `ValueView` の pointer
variant フィールドを **by-value guard 型 `ArcRef<'a,T>`/`GcRef<'a,T>`**（`ManuallyDrop` で refcount を
触らず `Deref` する再構成型）に変える追加実装が要る。mutsu では Num より pointer/Int が支配的なので
利得が逆。

**両出口を開けたまま進める**: wall doc §2 の移行規則（call site を deref 互換の使い方に限定 —
`s.clone()`/`s.is_empty()`/`&*s`/`Arc::ptr_eq` は可、`&Arc` 自体の保存やアドレス比較は不可）は
3b-0 で既に守られている。したがって、後述のベンチ（§3.2）で pointer-favored が Num 重ワークロードで
不利と実測された場合に限り、guard 型を導入して NaN-favored へ切り替える退路が残る
（call site 変更は不要、`view.rs` の pointer フィールド型と box 内部のみ差し替え）。

### 2.2 variant-privacy seal は **3b-1 に統合**（単独 PR にしない）

wall doc §7.1 の seal を独立ステップとして先行実施しない。理由:

1. seal の唯一の実効メカニズム（newtype ラッパー）が **3b-1 step 2 の構造的前半そのもの**。
   3b-1 は `Value` を `struct Value(内部表現)` の newtype にするので、newtype 化＝副作用で
   variant が compile-time に seal される。単独 seal を挟むと同じ 1293 サイトを二度触る。
2. wall の後退は ratchet（`check-value-wall.sh`・`make test` 組込）が **決定的に検出**済み。
   単独 seal の追加価値は「CI 失敗をコンパイル失敗へ前倒し」する belt-and-suspenders のみで、
   1293 サイトの二重リファクタに見合わない。
3. ratchet は seal 後も残置する（安価な回帰ネット）。

→ 3b-1 の最初のコミットが **byte-identical な newtype 化**（内部表現はまだ現行 enum のまま
`struct Value(ValueRepr)` に包むだけ）になり、これが seal を兼ねる（§3.3 step A）。

### 2.3 スライス構成（roadmap §3.3 step 2 を細分化）

3b-1 を「安全な機械変換（構造）」と「危険な表現差し替え（実体）」に分割する:

- **step A（newtype seal・byte-identical）**: `pub enum Value` → `pub struct Value(ValueRepr)`
  （`ValueRepr` は `src/value/` private の enum、現行 variant をそのまま保持）。`src/value/` 内部の
  1293 サイトを `.0` 経由へ機械変換。`ValueRepr` は依然 48B なので **挙動・サイズとも不変**。
  variant は compile-time に seal される。ゲート: make test byte-identical、`value_size_guard` は 48 のまま。
  **✅ 実装済み（2026-07-11）**。実装形: パターン/struct-like variant 式サイトは
  `Value(ValueRepr::..)`（コンパイルエラー span 駆動の機械変換 ~1800 サイト）、tuple/unit variant の
  **式**サイトは variant 名 constructor shim（`crate::value` 限定可視の `fn Int(..)`/`const Nil` 等 —
  step B でこの shim 本体が NaN-box の tag-packing constructor になる）。seal 実証 =
  `src/value/` 外の `Value::Int(3)` が E0624 (private) でコンパイル不能。`mem::discriminant` の
  variant 比較 3 サイトは wall API `Value::same_variant()` へ置換（struct への discriminant は
  `enum_intrinsics_non_enums` エラー — seal が非-enum 前提の残存 idiom も炙り出した）。
- **step B（表現差し替え・実体）**: `ValueRepr`（48B enum）を pointer-favored NaN-box（8B）へ置換。
  `view()`/constructor/accessor/`with_*_mut` の中身だけが変わる。`value_size_guard` を 48→8 に更新。
  ゲート: make test ＋ full roast ＋ gc-stress green、GC カウンタ不変（型フィルタのスカラ/コンテナ
  判定がタグ判定に変わるだけ）、§3.2 のマイクロベンチ達成。
- step A と step B の分割により、**中間状態が常に byte-identical**（step A）→ 危険な変更（step B）は
  確立済みの newtype 境界の背後だけに閉じる。bisect/レビューも容易。

## 3. 実装時に確定する下位判断（本 ADR では方針のみ）

### 3.1 小整数幅

inline `Int` の幅（48bit inline / 32bit inline / 範囲外は `Gc<i64>` 箱 or `BigInt` 箱行き）は
**int-arith / fib ベンチで flip 時に決定**（roadmap §3.2）。pointer-favored はタグ空間がやや狭い点に
注意。既定案: 48bit 級 inline、範囲外は既存の `BigInt` 経路へ委譲（`Gc<i64>` 箱は計測で必要なら）。

### 3.2 受け入れマイクロベンチ（step B ゲート）

- **int-arith 2x**・**fib +30%**（PERFORMANCE.md Phase 4a 期待値）。
- bench-class の `Value` clone/drop share **50% → 20% 以下**。
- GC-on bench-class 残差 **+8% → +3% 以下**（roadmap §3.3 の 3b 全体ゲート）。
- **pointer-favored 妥当性チェック**: `Num` 重ワークロード（複素数/浮動小数ループのマイクロベンチを
  1 本追加）で回帰が **f64 オフセット 1 命令ぶんに収まる**ことを確認。ここで想定外の回帰が出た場合のみ
  §2.1 の退路（guard 型で NaN-favored）を発動。

### 3.3 付随フィールドの配置

`Array` の `ArrayKind`・`Set/Bag/Mix` の mutability bool は **box のスペアタグビットに載せる**を
既定とし、ビットが逼迫した場合に pointee へ移す。`Instance` の `{class_name: Symbol, id: u64}` は
ポインタ 1 本に収まらないため、`InstanceAttrs`（既に `Gc`）側へ寄せるか専用ヒープ箱にする
（step B の設計で確定）。

## 4. Consequences

- step A 完了時点で variant は compile-time に seal され、wall doc §7.1 の目的を達成（ratchet は残置）。
- pointer-favored 採用により `ValueView` の pointer variant フィールドが現行の `&Arc<T>`/`&Gc<T>` の
  まま維持され、`view.rs` の変更と外部影響が最小化される。
- `Num` 演算に f64 オフセット加減 1 命令が乗る（非主要パス・§3.2 で妥当性を実測）。
- Send/Sync は不変（payload はポインタのまま・roadmap §3.4）。タグ操作は 1 モジュールに閉じ込め
  Miri でユニット検証（roadmap §3.4）。
- 3b-1 完了で JIT（ADR-0004 J1）の前提「8B 固定幅 `Value`」が満たされる。続いて 3b-2（交通量刈り）。

## 5. Alternatives considered

| 案 | 利点 | 欠点 | 判定 |
|---|---|---|---|
| **pointer-favored NaN-box（本 ADR）** | ポインタ deref マスク不要・view 移行最小（`&Arc<T>` 維持）・外部影響ゼロ | f64 にオフセット 1 命令 | **推奨採用** |
| NaN-favored（native double） | `Num` 演算ゼロコスト | ポインタ deref にマスク・view が guard 型（ArcRef/GcRef）へ追加実装・mutsu は Num 非支配 | 却下（退路として保持） |
| 単独 variant-privacy seal を先行 | 表現変更と分離してレビュー | 実効メカニズム=newtype=3b-1 step A なので 1293 サイトを二度触る・ratchet で回帰は既に防止済 | 却下（3b-1 に統合） |
| module-boundary seal（`pub use` 再エクスポート） | churn ゼロの想定だった | **seal にならない**（variant が再エクスポートで漏れる・実証済み） | 却下（実現不能） |
| 一斉 flip（step A/B 非分割） | コミット数少 | 5b で 479 エラー/型・レビュー/bisect 不能（roadmap §3.3/§3.4） | 却下（段階必須） |

---

*本 ADR は Proposed。承認後、3b-1 step A（newtype seal・byte-identical）から着手し、
step B（pointer-favored 表現差し替え）を §3.2 のベンチゲートで受け入れる。*
