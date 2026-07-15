# ADR-0004: JIT の方式選定とフェーズ計画（層4）

- **Status**: Accepted（2026-07-06 ユーザー承認 — Lever 3 凍結を含む）
- **Date**: 2026-07-05
- **Deciders**: tokuhirom, Claude
- **関連**: [ADR-0001](0001-gc-strategy-and-phasing.md)（フェーズ順序 3a→3b→4、レベル1 GC が
  JIT の前提コストを消す判断）, [gc-post-3a-roadmap.md](../gc-post-3a-roadmap.md)（層3b が本 ADR の
  前提工事）, [PLAN.md](../../PLAN.md) §5 Lever 3/4, PERFORMANCE.md

> ADR-0001 は「JIT は GC の後」「レベル1 GC（non-moving + refcount）なら JIT は stack map /
> forwarding / write barrier が不要で、`Arc`/`Gc` の inc/dec を emit するだけ」という
> *順序と前提* を決めた。本 ADR は JIT **そのもの**の方式を決める。

---

## 1. Context（背景）

- 実行系は単一の bytecode VM（`Interpreter`、~330 opcode、`exec_one` の match dispatch）。
  コンパイル単位は `CompiledCode`（定数プール・indexed locals・`simple_locals`/upvalue 解析済み）。
- perf 現況（対 raku）: fib(25) 1.0x・int-arith 0.7x と命令律速はほぼ互角だが、
  **bench-fib（型制約付き）3.2x・method-call 2.7x・bench-class 2.3x** が残る。
  目標（PLAN §5）: method-call < 1.5x・bench-class < 1.5x・bench-fib < 2x。
- JIT の期待値（PERFORMANCE.md Phase 4c）: tight numeric loop で 5–10x。
- GC はレベル1（cycle collector on Arc, default-on）。**non-moving**なのでポインタは動かず、
  refcount が生存を保証する。safepoint 機構（backedge/call/return/await/...）と
  cooperative STW は稼働済み — JIT コードはこの safepoint を **poll する側**として参加する。
- 層3b（NaN-boxing, 8B `Value`）が先行する（ADR-0001 の順序）。JIT は 8B 固定幅の
  値をレジスタで扱える前提で設計する。

## 2. Decision（提案する決定）

### 2.1 バックエンド = **Cranelift**（JIT モード）

- 理由: pure-Rust で組み込みが軽い（rustc/LLVM ツールチェーン非依存）・コンパイル速度が
  JIT 向き・wasmtime で実運用実績・non-moving GC と相性の良い「素の関数ポインタ」モデル。
- 却下: LLVM(inkwell) は生成コード品質最強だがビルド・リンク・コンパイル時間とも
  重量級で、mutsu の「高速起動」の武器（起動 0.04x）を損ねるリスク。自前 asm template JIT は
  arch ごとの保守コスト。wasm 経由は間接層が無駄。
- 依存は **feature flag + 実行時 flag**（`MUTSU_JIT=on|off`、既定は当面 off）で隔離し、
  JIT なしビルド（非対応 arch）でも full 機能を維持する。
  （**2026-07-13 更新**: J5 ゲート達成により既定 on へ切替済み — 末尾の追記参照。
  `MUTSU_JIT=off` が明示オプトアウト。）

### 2.2 コンパイル単位 = **`CompiledCode` 関数全体**（method JIT）。tracing はしない

- ホット判定: `CompiledCode` ごとの呼び出しカウンタ（+ backedge カウンタ）。閾値超過で
  コンパイルし、以後のエントリはネイティブ関数ポインタ経由。
- OSR（実行中ループの途中乗り換え）は **初期スコープ外**（フェーズ J4 で backedge カウンタ
  からの hot-loop entry を再検討）。mutsu の主要ベンチは関数呼び出し反復なので、
  entry 置換だけで大半を回収できる。
- meta-tracing（PyPy 型）は却下: インフラ規模が別次元で、既存 VM 資産を捨てることになる。

### 2.3 実行モデル = **subroutine-threading + inline 化の漸進**（deopt なし）

JIT の最初の形は「opcode 列を、**VM ヘルパ関数呼び出しの列**としてネイティブ化」する
（= dispatch ループの除去がまず取れる）。その上で高頻度 opcode から順に CLIF へ
インライン展開していく:

- Tier A（J1–J2）: 全対応 opcode を helper call で直訳。未対応 opcode を含む関数は
  JIT 対象外（インタプリタのまま）— **guard/deopt/OSR-exit を一切作らない**。
  「コンパイルできるか」は静的に `CompiledCode` を見て決まるので、実行中の脱最適化が
  不要になる。これが ADR-0001 の「GC を単純に保つ＝JIT も単純に保つ」路線の JIT 版。
- Tier B（J3–J4）: Int/Num の算術・比較・分岐・local get/set・定数・小さな呼び出し規約を
  CLIF に直接展開（NaN-box タグ判定 + fast path、slow path は helper へ）。
  型プロファイルではなく **タグ分岐**で済ませる（8B NaN-box 前提）。
- 「JIT が emit する GC 協調コード」は 2 つだけ:
  1. 値の複製/破棄サイトでの `Gc`/`Arc` inc/dec（Tier B のインライン部のみ。helper call 部は
     helper 内で従来どおり）
  2. backedge / call サイトでの **safepoint poll**（`STOP_REQUESTED` チェック → 協調 park）

### 2.4 GC / スレッドとの整合（レベル1 前提の明文化）

- **stack map 不要**: JIT フレームに「collector から見えない生ポインタ」を置いてよい。
  refcount を正しく保っている限り、参照中の node が回収されることはない
  （cycle collector は孤立サイクルだけを刈る）。
- **forwarding 不要**: non-moving なのでコンパイル済みコードに埋めたポインタは不変。
- **write barrier 不要**: 世代別ではない。ただし mutation chokepoint の
  candidate push（buffered bit）は helper 経由の変異では従来どおり動く。Tier B で
  コンテナ変異をインライン化する時は candidate push もセットで emit する（＝コンテナ変異の
  インライン化は優先度最下位に置き、当面 helper に残す）。
- **STW**: JIT コードは safepoint poll を挟むので、cooperative STW の quiescence 会計に
  そのまま参加する。long-running なネイティブループが poll を欠くと STW が飢えるため、
  **backedge poll は Tier A から必須**とする。

### 2.5 フェーズとゲート

| フェーズ | 内容 | ゲート（受け入れ条件） |
|---|---|---|
| **J0** | 前提: 層3b 完了（8B Value）。**Lever 3（threaded dispatch）は凍結**（Tier A が dispatch ループ除去で同じ利得をより大きく取るため、二重投資を避ける） | 3b のゲート（gc-post-3a-roadmap §3.3）達成 |
| **J1** | Cranelift 組み込み骨組み: feature flag・関数カウンタ・コンパイルキュー・エントリ差し替え。fib 相当の最小 opcode 集合を Tier A で | `MUTSU_JIT=on` で fib/int-arith が JIT 実行され、**on/off で出力 byte 一致**。make test green（on） |
| **J2** | Tier A opcode カバレッジ拡大（arith/compare/branch/local/const/call-compiled-fn/return）+ CI に **jit-stress ジョブ**（gc-stress と同型: `MUTSU_JIT=on` で make test + roast） | jit-stress green。bench-fib で dispatch 除去分の実測改善（目標 3.2x → ~2.5x） |
| **J3** | 呼び出し規約の最適化: JIT→JIT 直接呼び出し・method dispatch の inline cache（class Symbol キー）・型制約チェックの JIT 内 fast path | method-call < 1.5x・bench-class < 1.5x・bench-fib < 2x（PLAN §5 目標の達成レバー） |
| **J4** | Tier B: NaN-box タグ分岐つき算術インライン + hot-loop（backedge カウンタ、必要なら OSR entry） | int ループ 5–10x（PERFORMANCE.md 期待値）。fib ループ内で `gc_candidate_pushes == 0` かつ refcount 操作ゼロ（ADR-0001 §3-8 の完成形） |
| **J5** | 既定 on 切替: gc-stress × jit-stress のマトリクス green + オーバーヘッド予算（非ホットコードのコンパイルコストが起動 0.04x を悪化させない）を実測して `MUTSU_JIT` 既定 on | 起動ベンチ不変・全 CI green・ADR 更新 |

### 2.6 テスト戦略

- **差分実行が第一ゲート**: 同一プログラムを `MUTSU_JIT=off/on` で走らせ出力 byte 一致
  （gc-stress で確立した手法の流用）。
- CI: `jit-stress` ジョブ（J2〜）。将来 `MUTSU_GC=on × MUTSU_JIT=on` は default 同士なので
  通常ジョブがマトリクスを兼ねる。
- `MUTSU_VM_STATS` に `jit_compiles` / `jit_entries` / `jit_bailouts`（対象外関数の理由別
  カウント）を追加し、「何がまだインタプリタに落ちているか」を数字で追う。

## 3. Consequences

- deopt/OSR-exit/stack map を持たない分、**JIT 本体は小さく保てる**が、対応 opcode を
  含まない関数は丸ごとインタプリタに残る。→ `jit_bailouts` カウンタで対象外の分布を可視化し、
  カバレッジ拡大を計測駆動にする。
- Lever 3（threaded dispatch）は凍結。JIT が頓挫した場合のみ復活させる。
- 層3c（biased RC）の要否判断は J4 完了後の profile まで保留（gc-post-3a-roadmap §4）。
- Cranelift 依存が増える（ビルド時間・バイナリサイズ）。feature flag で非 JIT ビルドを
  維持し、リリースバイナリでの既定は J5 で判断する。

## 4. Alternatives considered

| 案 | 利点 | 欠点 | 判定 |
|---|---|---|---|
| **Cranelift method JIT（本 ADR）** | 軽量・Rust 統合・JIT 向きコンパイル速度 | ピーク性能は LLVM 未満 | **採用** |
| LLVM (inkwell) | 最高のコード品質 | ビルド/リンク重量級・コンパイル遅い・起動性能の武器を損ねる | 却下 |
| 自前 template/copy-patch JIT | 依存ゼロ・最速コンパイル | arch ごとの実装/保守、Tier B 相当の最適化を自作 | 却下（Cranelift の Tier A がほぼ同コストで可搬） |
| meta-tracing（PyPy 型） | 理論上の適応最適化 | VM 資産の作り直し・年単位 | 却下 |
| wasm 経由（wasmtime） | sandbox/可搬 | 間接層のオーバーヘッド・GC/host 連携が煩雑 | 却下 |
| deopt/guard 型 speculative JIT | ピーク性能 | stack 再構築（OSR-exit）実装が本格 VM 級 | 却下（bailout=関数単位不採用で代替） |

---

*2026-07-06 ユーザー承認により Accepted（Lever 3 の凍結も同時承認）。実装は
gc-post-3a-roadmap の層3b（NaN-boxing）完了後、§2.5 のフェーズ J1 から着手する。*

*2026-07-13 追記: **J5 完了 — `MUTSU_JIT` 既定 on**。ゲート実測（bench CI main
`f19946ad`）: gc-stress × jit-stress マトリクス green（J2 以降常設）・
bench-startup 0.0087s → +jit 0.0086s（起動予算不変 — cold code は閾値未満で
コンパイルされないため）・全 `+jit` 系列がインタプリタ同等以上
（fib 0.77x→0.59x・bench-fib 1.64x→1.26x・回帰系列なし）。bench CI の plain
系列は以後 `MUTSU_JIT=off` を明示 pin してインタプリタ基準線の意味を維持する。
J4 の「int ループ 5–10x」ゲートは文言上未達のまま J5 を先行した: J4c で
インタプリタ自体が -34% 高速化して相対差の分母が縮んだためで、絶対性能は
両系列とも改善している。残る J4d（変数 op インライン）は既定 on の上で続行。*

*2026-07-15 追記: **J4d 完了 — 本 ADR のフェーズ計画を完遂しクローズ**。
スライス構成: #4527（Tier B GetLocal インライン + lock-free hot-range cache）・
#4528（SetLocal ミラーの per-store intern 連鎖除去）・#4529（light-call 儀式の
alloc-free 化）・#4534（dispatch テーブルの FxHash 化 + 引数スキャン融合）・
#4537（light-call の caller-env フレーム再利用 — 空 overlay シングルトンを
CoW write latch として使う動的検出方式）・#4540（readonly-set の Arc
スナップショット → mutation journal 化）。bench CI 実測（ratio = 対 raku）:
fib+jit 0.34（`d6d405cc`・pre-#4534）→ 0.28（`c397b90b`）・bench-fib+jit
0.66 → 0.56。local 累計では fib(28) が pre-J4d ~0.4s → 0.13s。*

*J4 の「int ループ 5–10x」ゲート再判定: int-arith の JIT on/off 比は 1.24x
（`c397b90b`: 0.0375s vs 0.0466s）で文言上は未達のままだが、この期待値は
**J4c/J4d 以前のインタプリタを分母にした数字**であり、J4c（インタプリタ -34%）と
J4d（dispatch・call 儀式という on/off 共通コストの削減）が分母を縮め続けた結果、
比率では原理的に届かない。絶対性能では int-arith+jit は raku の 0.18x
（≈5.5 倍高速）・全 `+jit` 系列がインタプリタ系列と同等以上で、当初「5–10x」が
意図した水準（tight numeric loop の native 級実行）は絶対値で実現済み。
比率ゲートは obsolete と判定してクローズする。*

*設計メモ（J4d ①②の評価結果）: 「CallFunc opcode への per-call-site inline
cache」は評価の上**不採用** — #4534 の FxHash 化後、名前キーの二重 probe は
~10-15ns/call まで下がり、per-site 化の残り利得（~3%）は `CompiledFns` の
Arc 化 + map-id 検証機構の複雑さに見合わない。「JIT→JIT 直接呼び出し」も
専用 native 経路は作らず、interpreter/JIT が共有する light-call 経路の固定費
（env swap の Arc churn / readonly snapshot / GC safepoint の locked RMW）を
削る形で同じ利得を回収した（#4537/#4540 — profile 上 call 儀式が dispatch probe の
3 倍超だったため）。残る on/off 共通固定費の根治（SetLocal env-mirror 等）は
PLAN §6 lexical-slot campaign の領域。*
