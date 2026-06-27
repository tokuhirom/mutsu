# Architecture Decision Records (ADR)

このディレクトリは mutsu のアーキテクチャ上の意思決定を記録する。

## 目的

設計の分岐点（大きな方式選定・順序決定・撤回しうる判断）について、
**「なぜそう決めたか」と「何を却下したか」**を後から辿れるようにする。
コードや PLAN.md からは読み取れない *判断の文脈* を残すのが ADR の役割。

## 運用

- 1 決定 = 1 ファイル。`NNNN-kebab-title.md`（連番）。
- **Status**: `Proposed`（議論中・承認待ち）/ `Accepted`（確定）/ `Superseded by ADR-XXXX`（更新済）。
- 判断が変わったら**既存 ADR を書き換えず**、新しい ADR で supersede し、旧 ADR の Status を更新する。
- 既存 docs に合わせ日本語で記述する。

## 一覧

| # | タイトル | Status |
|---|---|---|
| [0001](0001-gc-strategy-and-phasing.md) | GC 導入の方式選定とフェーズ計画 | Proposed |
