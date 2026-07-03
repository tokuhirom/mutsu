# ADR-0002: Phase A ゲート再評価 — GC 着手条件の充足確認

- **Status**: Accepted
- **Date**: 2026-07-03
- **Deciders**: tokuhirom, Claude
- **関連**: [ADR-0001](0001-gc-strategy-and-phasing.md)（本 ADR は ADR-0001 §3-2 の「GC は Phase A 完了後に着手」の
  判定基準を補足・確定する。ADR-0001 自体を撤回・supersede するものではない）

## 1. Context

ADR-0001 は「GC は Phase A（互換性＋速度で raku に追いつく）完了後に着手」と決めたが、
「完了」の判定を PLAN.md のメトリクス表（whitelist 数・perf 倍率）に委ねていた。
2026-07-03 時点で GC 開発着手の是非を検討したところ、この表が **stale** であることが判明した。

## 2. 判明した事実

- PLAN.md 記載の whitelist 数「1285 / 目標 1300+」は古い値。
- 実測: whitelist **1345**（`roast-whitelist.txt`）／ roast 全 `.t` ファイル **1463**。
  → **目標 1300+ は既に達成済み**。残り ~118 件は §F 記載の通り、rakudo 自身が未実装の構文・
  MoarVM REPR 依存・fudge 対象など、個別対応の見込みが薄いロングテール。
- perf 目標（method-call <1.5x 等）は未達だが、残る主要レバー（Lever 2 NaN-boxing・Lever 4 JIT）は
  **ADR-0001 自身が GC の後（層3b・層4）に順序づけている**。GC 未着手を理由に perf 完了を待つのは
  「GC の後にやる作業の完了を GC 着手条件にする」循環になり、自己矛盾する。
  残る pre-GC perf 項目は Lever 3（threaded dispatch）のみで、これは GC と独立に並行可能。

## 3. Decision

- **Phase A の完了条件を「substrate 完了 ∧ roast 目標達成」と確定する。** perf の全レバー完了は
  Phase A 完了条件に含めない（Lever 2/4 は設計上 GC 後のため）。
- 上記基準により **Phase A は完了とみなし、GC（Phase B）の開発に着手する。**
- PLAN.md のフェーズ表・メトリクス表を本 ADR の数値に合わせて更新する。
- 実装は `docs/gc-level1-detailed-design.md` §11 の手順に従い、**root visitor 導入**
  （`Interpreter::visit_roots()` への root 集約。GC 本体・`Gc<T>` はまだ実装しない）から着手する。

## 4. Consequences

- PLAN.md の「A. 追いつく（いまここ）」を「B. Value 表現リワーク＋GC（いまここ）」に更新する。
- roast/perf の残件（§F ロングテール・Lever 3 以降）は引き続き並行して進める。GC 着手と排他ではない。
- 今後 PLAN.md のメトリクス表を更新する際は、都度 `wc -l roast-whitelist.txt` 等で実測し、
  stale な数値を記載し続けない（本件のように判断を誤らせるため）。
