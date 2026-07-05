# ADR-0003: デフォルト GC=on のトリガ方針（level-1a の production トリガ）

- **Status**: Accepted（2026-07-05 ユーザー承認）
- **Date**: 2026-07-05
- **Relates to**: [ADR-0001](0001-gc-strategy-and-phasing.md)（§4.2 起動方式 / §4.3 A' 範囲）,
  [ADR-0002](0002-phase-a-gate-reassessment.md), `docs/gc-level1-detailed-design.md` §9

## 1. Context

level-1a cycle collector（Bacon-Rajan on Arc）は機能的に完成した: 全 safepoint 種別の emit
（#4195）、worker churn 下の cooperative cross-thread STW（#4205）、共有 captured-env の
phantom-edge unsoundness 修正（#4213）、deterministic/random stress 面の完備（§9.2）、CI
gc-stress の全ステップ blocking 化（#4219）。しかし GC は**依然 default off** であり、
`MUTSU_GC=on` でも automatic トリガ未設定なら collect は program-end の 1 回のみ —
長時間稼働のサーバプロセスはサイクルゴミを溜め続ける。ADR-0001 は §4.2（同期/非同期）と
§4.3（A' をどこまで前提とするか）を未決のまま残していた。

2026-07-05 に確定した 2 つの事実が設計を規定する:

1. **push 回数周期のトリガは production には漸近的に不適**。`MUTSU_GC_EVERY_CANDIDATE=N` は
   N push ごとに full trial-deletion scan（コスト = O(live suspect graph)）を発火する。
   candidate を churn しながら大きな生存構造を伸ばすワークロード
   （S17-lowlevel/cas-loop.t: 4×1000 `cas` で linked list 構築）では、N=64 で
   8500+ collects × 各 ~4300 ノードのトレース = **180 秒超**（N=1024 で 3.3 秒、GC-off ~5 秒）。
   固定周期は同じ生存 suspect を何度も再走査する。トリガは「前回の scan がどれだけ回収できたか」
   に適応しなければならない。
2. **A'（root 集約）は level-1a の前提ではない**（§11 step 11 調査）: collector は refcount
   駆動で root 列挙を一切消費しないため、frame/upvalue の root 集約は 1a のトリガにも
   正しさにも寄与しない。ADR-0001 §4.3 は「A' は繰延」として狭く解決する。

## 2. Decision（Proposed）

1. **同期 collect**（ADR-0001 §4.2 = 同期。設計書 §12「1a では background collector を捨てる」を
   再確認）。トリガは既存の `PENDING` フラグを arm し、次に safepoint を踏んだ mutator 上で
   inline に collect する（他 mutator が生きていれば既存の cooperative STW）。非同期
   （concurrent collection）は 1a では却下: trial deletion は並行 refcount 変異に耐えられず
   （epoch 機構が必要）、全種 safepoint 網の完備によりトリガ→collect の遅延は既に十分小さい。

2. **トリガ = candidate バッファの「サイズ」閾値 + adaptive backoff**。
   `buffer_candidate` はバッファ長が実効閾値に達したら `PENDING` を arm する。collect の
   たびに `threshold = clamp(BASE, 2 × survivors, MAX)`（survivors = revive された生存
   suspect 数）へ更新: 「ほぼ全部生きていた」scan は閾値を引き上げ（cas-loop 型の quadratic
   から自動退避）、「ほぼ全部回収できた」scan は BASE へ戻す。チューナブルは
   `MUTSU_GC_THRESHOLD`（BASE。既定値は計測で決定 — 出発点 16384）のみ。既存の stress 用
   env 変数群は CI/デバッグ用としてそのまま残す（production トリガは独立した追加機構）。

3. **`MUTSU_GC` の既定値 off → on の切替**は、次を順に満たしてから行う:
   a. gc-stress roast の blocking 化と green 維持（#4219 で完了・継続観察）。
   b. 閾値トリガの実装 + gc_stress での担保（GC-off と出力 byte 一致・churn 下の
      `pause_ns_max` 有界・cas-loop 型ワークロードが GC-off 比 ~1.2 倍以内）。
   c. 計測オーバーヘッド予算: fib/int ループ ≈ 0（スカラ型フィルタ — ADR-0001 の
      「fib で push == 0」ゲート）、method-call / bench-class は GC-off 比 < 5%。
   d. opt-out `MUTSU_GC=off` の維持。
   切替自体は本 ADR の Accepted 後、b/c の実測を添えた PR で行う。

4. **A' は繰延**（Context 2 による）: 将来の root 消費型 VERIFY 拡張、および
   NaN-boxing / JIT 世代の最適化の enabler として、それぞれの時間軸で扱う。

## 3. Consequences

- サーバ型プログラム（join しない worker がサイクルを churn する — ADR-0001 の動機ケース）が、
  バッファ増加に比例した自動サイクル回収を quadratic 病理なしで得る。
- `MUTSU_VM_STATS` の gc カウンタ群が default-on 切替の受け入れ計測器になる。
- stress ノブ（`EVERY_SAFEPOINT`/`EVERY_CANDIDATE`/`AT`/random）は CI・デバッグ用として不変。

## 4. Alternatives considered

| 案 | 判定 |
|---|---|
| push 回数周期（`EVERY_CANDIDATE` の流用） | 却下 — O(live graph) scan の周期発火は quadratic（cas-loop.t 実測 180 秒超） |
| background thread での concurrent collect | 却下（1a）— trial deletion が並行変異に非対応。設計書 §12 の再確認 |
| 時間ベース（N 秒ごと） | 却下 — アイドルプロセスで無駄、バースト時に遅すぎる。サイズ閾値が負荷に自然追従 |
| A' 完了を前提にする | 却下 — collector は root 列挙を消費しない（§11 step 11 調査） |

---

*2026-07-05 ユーザー承認により Accepted。実装は §2 の Decision に従い、§2-3 のゲートを満たして default-on を切り替える。*

## 5. Update（2026-07-05・切替時の受け入れ実測とゲート改訂）

§2-3c の実測（release・9 反復 best・load < 1.5）:

| bench | GC-on overhead | 判定 |
|---|---|---|
| fib25 / bench-fib / int-arith | +0.5% / +3.0% / +2.0% | ✓（型フィルタで scalar/関数 hot path はほぼ無料 — ADR-0001 の本来の狙いを達成） |
| method-call | +0.7〜6.4%（測定ノイズ帯） | ✓/△ |
| **bench-class** | **+7.5〜8.3%** | ✗（< 5% に対し） |
| churn 系 | ≤ +10% | ✓（≤ 1.2x） |

bench-class の残差は「28k インスタンスに対して 16.5M 回の `Value` clone/drop」という既存の
クローン交通量に per-drop の buffered-bit 分岐 1 個が乗る構造的コストで、GC 側の安価な改善
（drop fast path で +61%→+8%・program-end collect skip・`gc_enabled` キャッシュ）は投入済み。
交通量自体の削減は ADR-0001 が層 3b（NaN-boxing）に割り当てる領域である。

**ユーザー判断（2026-07-05）: bench-class ~8% を許容してこのまま default-on に切り替える。**
リークしない既定の価値が class 密度の高いコードの 8% に優先し、根本削減は 3b で回収する。
`cfg!(test)`（crate 自身の unit-test ビルド）のみ既定 off — 並列 `cargo test` のテスト分離のため
（GC-on unit test は CI gc-stress ジョブが担保）。

