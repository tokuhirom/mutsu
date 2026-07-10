# roast ブロッカー再評価メモ

この文書は、roast の失敗を「テストファイル単位」ではなく
**根本原因単位**で追うための索引。
個々の失敗を片端から潰すためではなく、
**今どこを直せば何がまとめて動くか**を判断するために使う。

**最終更新 2026-07-10**（旧 §3「第一級コンテナ / container identity」と旧 §2.3（hash
multislice）をセクションごと削除 — 全サブキャンペーン完了: element mutation in-place 化
#4362/#4366・post-call writeback 全廃 #4370/#4372/#4377・attr accessor slot 化 #4360・
multislice #4354/#4355・scalar itemization #4382・escaped our-sub lexical write #4385。
詳細は `news/2026-07.md`。それ以前: 旧 §4「並行・非同期（S17）」削除＝S17 全 99 ファイル＋
socket 系 whitelist 済み・cross-thread lexical writeback 解消（#4336/#4340/#4345/#4348））

## この文書の読み方

- ファイル名の羅列ではなく、**根本原因単位**でブロッカーを追う。
- 各ブロッカーは次の粒度で書く:
  - **根本原因**: 何の抽象が欠けているか
  - **変更レイヤ**: parser / compiler / VM / runtime / value 表現 / scheduler のどこを触るか
  - **Next slice**: 次に何を切り出して着手するか
  - **完了条件**: どこまで行けば、その類の roast がまとめて動くか
- **whitelist 済みになった項目はこの文書から削除し、詳細は `news/` に移す。**
  このファイルは常に「現在まだ開いている残件」だけを持つ。
- **あるセクションの残件がゼロになったら、セクションごと削除する。** 完了報告は `news/` に残す。
- 個別の per-file 詳細ログは `TODO_roast/S*.md` を参照。このファイルはそちらの要約と
  優先順位づけに徹する。
- **このファイルは頻繁に複数セッションから並行更新される。** 着手前に必ず
  `MUTSU_FUDGE=1 prove -e target/debug/mutsu roast/<path>.t` で実際の pass/fail 数を
  取り直すこと。ここに書かれた数字は執筆時点のスナップショットであり、数時間で
  古くなることがある。

## 現在の前提

- whitelist は **1373**（2026-07-10 時点、`wc -l roast-whitelist.txt`）。
- 安い 1 ファイル勝ちはほぼ枯渇している。roast 由来の大型ブロッカーは出尽くしており、
  残件は本ファイルの §1/§2（深い基盤待ち・低優先）と §4（whitelist 非目標）のみ。
  次の構造工事の選定は [PLAN.md](../PLAN.md)（lexical-scope slot campaign / GC post-3a /
  Batteries）を正とする。
- かつてここにあった「真の lazy 配列 / 無限列」「dispatch / 演算子 sugar の desugar surface」
  「並行・非同期（S17）」「第一級コンテナ / container identity（§3）」はいずれも完了済み
  （詳細は `news/2026-06.md` / `news/2026-07.md`）。

---

## 1. Hard — 単体では動かせない、大きな下位基盤待ち

### 1.1 Unicode / RakuAST（低 ROI・据え置き）

- `S32-str/format.t`（reachable 26/49 は全 pass、test 27-29 で中断）
  - **根本原因**: `Formatter::Syntax.parse`→Match、`Formatter.CODE`→Callable、
    `Formatter.AST`→`RakuAST::Node` を要求し、ここで runtime error 中断するため以降が到達不能。
    本質は **RakuAST サブシステム不在**（参照 raku 本体すら `Format` 6.e 未対応）。
  - **評価**: RakuAST 本実装なしには whitelist 不可。stub 化は禁止のため据え置き。

---

## 2. 局所修正で進む残り

### 2.2 `S02-types/generics.t`

- **現状**: 0/1（変化なし）。
- **論点**: 6.e の coercion type 項 + `Array[T]` サブクラス化が必要。ローカル raku
  (v2022.12=6.d) でもこのテスト自体がコンパイルに失敗するため、参照実装での検証すらできない。
- **評価**: 深い機能待ちで、局所修正では進まない。優先度は低い。

---

## 4. whitelist を目標にしない項目

### 4.1 rakudo 側も失敗する、または roast 側の問題が強いもの

ここは mutsu 側で一般改善が入るのはよいが、
**そのファイルを whitelist すること自体を目標にしない**。

- `S05-nonstrings/basic.t`
- `S05-metasyntax/angle-brackets.t`
- `S05-mass/rx.t`
- `S06-advanced/caller.t` — stale test・over-stated plan（22 planned vs 実質 19 assertion）。
  rakudo 自身の期待値と乖離しており unpassable と確定済み（#3975）。
- `S06-advanced/return_function.t`
- `S10-packages/require-and-use--dead-file.t`
- `S12-traits/parameterized.t`
- `S12-meta/exporthow.t`
- `S12-class/open_closed.t`
- `S32-str/sprintf.t`

### 4.2 低 ROI で後回しにすべきもの

- `S32-str/format.t` — `RakuAST` が無い限り最後まで通しにくい（§1.1 参照）。

---

## 5. 今のおすすめ着手順

**roast 由来の大型ブロッカーは出尽くした。** 第一級コンテナ / container identity
campaign（旧 §3）は 2026-07-10 に全サブキャンペーン完了（element mutation in-place 化
#4362/#4366・post-call writeback 全廃 #4370/#4372/#4377・attr accessor slot 化 #4360・
hash multislice #4354/#4355・scalar itemization #4382・escaped our-sub lexical write
#4385 — 詳細は `news/2026-07.md`）。cross-thread lexical writeback campaign（旧 §4.1）
も完了済み。

残っているのは §1（RakuAST 待ち・据え置き）・§2.2（6.e generics・深い機能待ち）・
§4（whitelist 非目標）のみで、いずれも「次の 1 本」には適さない。
**次の構造工事の選定は [PLAN.md](../PLAN.md) を正とする**（lexical-scope slot campaign
完遂〔PLAN §6〕/ GC post-3a ロードマップ〔PLAN §2〕/ Batteries〔PLAN §1〕）。

whitelist を目標にしない §4 の項目は、mutsu 側の一般改善のついでに触れるのはよいが、
そのファイル単体を通すことを目的にしない。
