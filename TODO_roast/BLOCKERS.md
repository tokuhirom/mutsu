# roast ブロッカー再評価メモ

この文書は、roast の失敗を「テストファイル単位」ではなく
**根本原因単位**で追うための索引。
個々の失敗を片端から潰すためではなく、
**今どこを直せば何がまとめて動くか**を判断するために使う。

**最終更新 2026-07-11**（§6「未 triage の孤立残件」を新設 — `roast-history.sh` の
sweep 非 pass 集合を全数 triage し、大型共通根本原因を持たない個別残件 15 本を索引化。
併せて `roast-history.sh` を `cd roast` してから実行するよう修正（CI の
`run-roast-test.sh` と CWD を揃えた）ため、CWD 依存の encode 3 本
（`S32-str/gb18030|gb2312|shiftjis-encode-decode.t`）は sweep でも通るようになり残件から除外。）

**2026-07-10**（旧 §3「第一級コンテナ / container identity」と旧 §2.3（hash
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
  残件は本ファイルの §1/§2（深い基盤待ち・低優先）・§4（whitelist 非目標）・
  §6（未 triage の孤立残件・共通根本原因なし）のみ。
  次の構造工事の選定は [PLAN.md](../PLAN.md)（lexical-scope slot campaign / GC post-3a /
  Batteries）を正とする。
- **BLOCKERS.md は失敗集合の網羅リストではなく、根本原因単位の索引**である。
  `roast-history.sh` の sweep 非 pass 集合との差は次で説明できる: (1) whitelist 済みは
  ここから削除済み（news/ 送り）、(2) load 由来の flaky / timeout は sweep の見かけ上の
  失敗、(3) 大型の共通根本原因を持たない小粒な個別残件は §6 に集約（per-file 詳細は
  `TODO_roast/S*.md`）。
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

## 6. 未 triage の孤立残件（個別ブロッカー・共通根本原因なし）

2026-07-11 の sweep（`c3cf84dd`, `cd roast` CWD で全数取り直し）で非 pass だが、
§1/§2/§4 のいずれにも属さず、**互いに共通する大型根本原因を持たない**個別残件。
それぞれ単独の調査が要る tail であり、campaign を組めないため BLOCKERS 大型索引には
出していなかったもの。cheap win を拾う際の入口として一覧化する（per-file 詳細は
`TODO_roast/S*.md`）。数字は執筆時点のスナップショット。

| ファイル | 現状 | ブロッカー（一言） |
|---|---|---|
| `S02-names/pseudo-6d.t` | 91/159 で中断 | `::("CALLER")::<$*bar>` CALLER 疑似パッケージ deref 未対応 |
| `S02-names/pseudo-6e.t` | 79/202 で中断 | 同上（6.e 版） |
| `S02-names-vars/names.t` | 141/156・notok 3 | test 144 付近「Null PMC access when printing a var」edge |
| `S02-types/array-shapes.t` | 34/43 で中断 | 未初期化 shaped array の `.map` 等、shaped-array の深い機能 |
| `S05-capture/hash.t` | parse で 0/? | 「The use of hashes in regexes is reserved」= 正規表現内 `%h` 未対応 |
| `S05-metasyntax/longest-alternative.t` | 56/62・notok 5 | LTM（longest-token-match）の tie-break edge |
| `S06-advanced/return-prioritization.t` | 9/11・notok 2 | `return` の優先順位（phaser 絡み）edge |
| `S10-packages/basic.t` | 50/83・notok 9 | package 宣言（`our`/ネスト）の edge |
| `S12-attributes/class.t` | 19/28 で中断 | test 19 以降で attribute 機能ギャップにより abort |
| `S12-attributes/trusts.t` | 9/15・notok 6 | `trusts` によるクラス間 private アクセス未対応 |
| `S12-traits/basic.t` | parse で 0/? | 「Can only use : as invocant marker ... after the first parameter」= カスタム trait シグネチャ未対応 |
| `S19-command-line-options/01-dash-uppercase-i.t` | 0/8 | `-I` + `@*INC` イントロスペクション（is-run サブプロセス）。`@*INC` 未populate |
| `S32-basics/xxPOS.t` | 11/64 で中断 | test 11 以降で深い機能により abort |
| `S32-hash/perl.t` | 43/55・notok 12 | `.perl`/`.raku` の Hash round-trip 整形 edge |
| `S32-temporal/time.t` | 8/10・notok 2 | `time`/`gmtime` の整形 edge |

これらは「次の 1 本」候補にはなり得るが、いずれも単発で共通利得が小さい。優先は
引き続き [PLAN.md](../PLAN.md) の構造工事を正とする。

## 5. 今のおすすめ着手順

**roast 由来の大型ブロッカーは出尽くした。** 第一級コンテナ / container identity
campaign（旧 §3）は 2026-07-10 に全サブキャンペーン完了（element mutation in-place 化
#4362/#4366・post-call writeback 全廃 #4370/#4372/#4377・attr accessor slot 化 #4360・
hash multislice #4354/#4355・scalar itemization #4382・escaped our-sub lexical write
#4385 — 詳細は `news/2026-07.md`）。cross-thread lexical writeback campaign（旧 §4.1）
も完了済み。

残っているのは §1（RakuAST 待ち・据え置き）・§2.2（6.e generics・深い機能待ち）・
§4（whitelist 非目標）・§6（未 triage の孤立残件）のみで、いずれも「次の 1 本」には
適さない（§6 は cheap win の入口として一覧化はしてある）。
**次の構造工事の選定は [PLAN.md](../PLAN.md) を正とする**（lexical-scope slot campaign
完遂〔PLAN §6〕/ GC post-3a ロードマップ〔PLAN §2〕/ Batteries〔PLAN §1〕）。

whitelist を目標にしない §4 の項目は、mutsu 側の一般改善のついでに触れるのはよいが、
そのファイル単体を通すことを目的にしない。
