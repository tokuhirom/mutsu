# roast ブロッカー一覧

roast の失敗を「テストファイル単位」ではなく**根本原因単位**で追うための索引。
今どこを直せば何がまとめて動くかを判断するために使う。

**最終更新 2026-07-14**（★未 whitelist 79 本を全数実測し直したところ、**旧版の残件表は
S\* 系 24 本しか載せておらず、`integration/` 41 本・`6.c/` 7 本・APPENDICES 4 本が丸ごと
抜けていた**。それらはほぼ全ファイルが **raku 満点＝★達成可能** で、「★の残件は無い」という
旧版の結論は誤りだった。§「integration — 実プログラム互換」を新設）。

**2026-07-12**: 全セクションを単一の残件表に統合。途中経過・完了報告の類は `news/` に移し、
本ファイルは「現在開いている残件」だけを持つ。同日、ローカル raku を
**Rakudo v2022.12 → v2026.06** に更新し、raku 列を全行再測定。

## この文書の読み方

- **whitelist 済みになった項目はこの表から削除**し、詳細は `news/` に移す。
  完了した campaign の履歴も `news/2026-06.md` / `news/2026-07.md` を参照。
- per-file の詳細ログは `TODO_roast/S*.md`。全 roast × raku の結果一覧は
  [`raku-baseline.tsv`](raku-baseline.tsv)（生成: `scripts/roast-raku-baseline.sh`、
  解説: [`raku-baseline.md`](raku-baseline.md)）。
- **数字は執筆時点のスナップショット。** このファイルは複数セッションから並行更新される
  ため、着手前に必ず `MUTSU_FUDGE=1 prove -e target/debug/mutsu roast/<path>.t` で
  実際の pass/fail を取り直すこと。
- **raku 列の読み方**: ローカル参照実装は **Rakudo v2026.06（デフォルト言語 6.d）**。
  raku は **未 fudge の生ファイル**を直接実行して測る（fudge は roast/ 書き換えを要する
  ため適用不可）。`SORRY` はローカル raku がコンパイル不能＝廃止構文・rakudo 未実装構文・
  fudge 前提の行などで、「テストが不正」を意味しない。raku が満点なのに mutsu が
  落ちるものだけが達成可能な実バグ（★）。

## 現在の前提

- whitelist は **1384 / 1463**（2026-07-14、`wc -l roast-whitelist.txt`）＝未 whitelist **79 本**。
- **S\* 系（各シノプシスの機能テスト）は出尽くした。** かつての大型 campaign（真の lazy 配列 /
  dispatch・演算子 sugar の desugar / S17 並行・非同期 / 第一級コンテナ container identity /
  cross-thread lexical writeback）はすべて完了済みで、S\* に残るのは下の
  [個別ファイル表](#s-系の個別ファイル残件)（**全て 非目標／oracle 不能／基盤待ち**）だけ。
- **★フロンティアは `integration/` に移った**（下節）。未 whitelist 79 本の内訳は
  S\* 系 24 本ではなく **`integration/` 41 本＋`6.c/` 7 本＋APPENDICES 4 本＋roast 自身の
  ツール test 2 本＋MISC 1 本**が主体で、`integration/` はほぼ全ファイルが raku 満点。

## integration — 実プログラム互換（★達成可能の本体・2026-07-14 全数実測）

`integration/` は Advent Calendar・99 problems といった**実在の Raku プログラム**を走らせる
テスト群で、「実用コードがそのまま動く」というプロジェクトゴールに最も近い互換性指標。
`raku-baseline.tsv` 上、未 whitelist の `integration/` は **ほぼ全ファイルが `raku_status=PASS`
（raku 満点）＝定義上すべて ★達成可能**。1 ファイル 1 バグではなく**根本原因が数個に集約**する。

計測: `MUTSU_FUDGE=1 timeout 25 prove -e target/debug/mutsu <file>`（2026-07-14・debug build）。

| 根本原因クラスタ | 本数 | 該当ファイル（抜粋） | 症状 |
|---|---|---|---|
| **① スタックオーバーフローで abort**（★根本原因は 1 つではない — 下節参照） | 4 | `99problems-41-to-50.t`・`99problems-51-to-60.t`・`man-or-boy.t`・`deep-recursion-initing-native-array.t` | `fatal runtime error: stack overflow, aborting`（プロセス abort）。**症状は同じだが原因は 4 つ別々**で、うち「本当に深い再帰」は 1 本だけだった |
| **② パース不能（`===SORRY!===`）** | 10 | `advent2009-day16.t`(:58 `{`)・`advent2009-day23.t`(:122 `gather {`)・`advent2010-day11.t`(:24 `q \| … \|` = `\|` デリミタの q)・`advent2012-day04.t`(:22 `do {…} … *` 連番列)・`advent2012-day19.t`(:11 `4.7k` = ユーザ定義 postfix)・`advent2013-day04.t`(:31 heredoc インデント)・`advent2014-day16.t`(:99 `{`)・`advent2012-day15.t`（`Unexpected block in infix position`）・`6.c/MISC/bug-coverage.t`(:287 `subtest … => {`)・`6.c/APPENDICES/A04-experimental/01-misc.t` | 個別の構文が未対応。1 本ずつ潰す（安い ★ が混じっている可能性が高い） |
| **③ ハング / タイムアウト** | 5 | `advent2012-day21.t`・`advent2013-day14.t`・`gather-with-loops.t`・`APPENDICES/A01-limits/{misc,overflow}.t` | 25s で打ち切り。gather×loop の遅延評価と limit 系 |
| **④ エラーメッセージ品質** | 2 | `error-reporting.t`（mutsu 4/33・raku 33/33）・`weird-errors.t`（26/36） | 「Parse error contains line number」等、**例外の文面・行番号・バックトレース**を検査するテスト。PLAN §6「エラーメッセージ品質向上」と同一の的 |
| **⑤ 個別機能ギャップ** | 残り | `advent2009-day24.t`（派生 grammar の拡張）・`advent2010-day14.t`（`nextsame` の継承/mixin）・`advent2010-day22.t`（`Rat` の `$!numerator`/`$!denominator` 属性）・`advent2011-day07.t`（`Metamodel::GrammarHOW` 継承）・`advent2011-day10.t`（`--doc` / `DOC INIT {}`）・`advent2009-day20.t`（signature の introspection/stringification）・`advent2009-day18.t`（パラメタ化 role の mixin）・`advent2013-day10.t`（`:round` 等の演算子 adverb）・`precompiled.t`（precomp） | 1 ファイル数本ずつ。★の近道はここ |

**近道（1 subtest 差のファイル）**: `6.c/S04-declarations/my-6c.t` は **111/112**（唯一の失敗＝
test 57 `OUTER::<$x>` 疑似パッケージ）。`advent2011-day04.t` は 1/2、`advent2009-day24.t` は 3/4。

### ① の内訳（2026-07-14 に 4 本とも root-cause 済み）

「深い再帰でスタックが溢れる」という当初の診断は**誤り**だった。同じ abort に見えて原因は別物:

| ファイル | 実際の原因 | 状態 |
|---|---|---|
| `99problems-41-to-50.t` | **無限再帰**。クロージャのフレーム env が *caller* の env の scoped child で、捕捉 env を「既にあれば入れない」でマージしていたため、caller の同名レキシカルが callee 自身の捕捉を shadow する（＝レキシカルスコープが動的スコープに化ける）。P46 のアクションは節ごとに `my @args` を持つクロージャを作るので、内側の節が外側の `@args`（自分自身を含む）を読み自己再帰した | **#4510 で修正**。P41・P46 通過。以降は P47 の パラメータ化 `multi rule expr($p)` 待ち（⑤へ） |
| `99problems-51-to-60.t` | ベアブロック `{ }` 内で宣言した sub の中で `not $tree.defined` が `Any` に対し**偽**になり、`add-to-tree` が無限再帰（P57）。ファイル先頭 test 8 も別バグ（平衡二分木の枝落ち） | 未着手。`tmp/bst2.raku` 相当が最小再現 |
| `man-or-boy.t` | raku と 15 行完全一致したあと発散し、余分な `B` が発火する。`&x1..&x5` に兄弟フレームのクロージャが混入する**別の捕捉リーク**（`$k is copy` の共有が絡む） | 未着手。#4510 では直らない |
| `deep-recursion-initing-native-array.t` | **これだけが本物の深い再帰**。約 20,000 段のネストが必要。`main.rs` は既に 256MB スタック（`thread::Builder::stack_size`）だが debug では 10k–20k 段で溢れる | 未着手。要 スタック拡張（`stacker`。ただし無限再帰が abort→OOM に化けるので Raku フレーム数の上限ガードとセット）または 呼び出しフレームのヒープ化 |

教訓: **abort の形（`stack overflow`）は原因を意味しない。** 深さを数えて raku と突き合わせ、
無限再帰か有限の深い再帰かを先に分けること（`raku` 側は 8 呼び出しで終わるのに mutsu が
1900 行トレースを吐く、で一発で分かる）。

## S* 系の個別ファイル残件

分類の定義:

- **★達成可能** — ローカル raku が満点なのに mutsu が落ちる＝実バグ。whitelist 到達可能。
  ただし残っているものはいずれも単発 fix ではなく複数機能が必要（安い 1 ファイル勝ちは枯渇済み）。
- **基盤待ち** — RakuAST・6.e generics など大きな下位基盤の実装が必要。低優先・据え置き。
- **oracle 不能** — ローカル raku が SORRY／中断で参照検証できない（6.e 専用構文・6.d 制限）。
  mutsu 側の実装課題は残るが、正解をローカルで確認できないため優先度低。
- **非目標** — rakudo 自身も失敗する・roast 側の問題・廃止仕様。mutsu 側の一般改善の
  ついでに触れるのはよいが、**そのファイルの whitelist を目標にしない**。
- **通過不能** — テスト自体が意図的 flunk 等を含み、通せないと確定。

| 分類 | ファイル | mutsu | raku (v2026.06/6.d) | ブロッカー（一言） |
|---|---|---|---|---|
| 基盤待ち | `S32-str/format.t` | 26/49 で中断 | **49/49 満点**（v2022.12 は SORRY） | `Formatter::Syntax.parse`→Match、`Formatter.AST`→`RakuAST::Node` を要求＝**RakuAST サブシステム不在**。raku 更新で oracle は得られたが、stub 化は禁止のため据え置きのまま |
| 基盤待ち | `S02-types/generics.t` | 0/1 | **1/1 満点**（v2022.12 は SORRY） | 6.e coercion type 項 + `Array[T]` サブクラス化が必要。raku 更新で参照検証は可能になったが、要求される基盤の大きさは不変 |
| oracle 不能 | `S02-names/pseudo-6d.t` | 116/159 で中断 | SORRY（`::=` NYI） | `::("CALLER")::<$*bar>` CALLER 疑似パッケージ deref 未対応。v2026.06 でも rakudo が `::=` 束縛未実装で SORRY |
| oracle 不能 | `S02-names/pseudo-6e.t` | 79/202 で中断 | SORRY（`$?` 定数 twigil NYI） | 同上（6.e 版）。v2026.06 でも SORRY |
| oracle 不能 | `S02-names-vars/names.t` | 144/156・notok 3 | SORRY（未 fudge 行） | test 142「Null PMC access when printing a var typed as ::foo」edge。raku は fudge 前提の行（裸の `$`）で SORRY |
| oracle 不能 | `S02-types/array-shapes.t` | 35/43 で中断 | 38/43（raku も中断・v2026.06 でも同じ） | shaped array `.pairs` の `.value` が writable container を返さない |
| oracle 不能 | `S05-metasyntax/longest-alternative.t` | 57/62・notok 5 | SORRY（`::` LTM stopper NYI） | LTM（longest-token-match）の tie-break edge。v2026.06 でも rakudo が `::` 未実装で SORRY |
| oracle 不能 | `S10-packages/basic.t` | 59/83・notok 9 | 6/83（mutsu 先行・v2026.06 でも同じ） | package 宣言 semicolon form のエラー検出 edge |
| oracle 不能 | `S12-attributes/trusts.t` | 9/15・notok 6 | SORRY（前方参照 `trusts B`） | `trusts` によるクラス間 private アクセス未対応。v2026.06 でも SORRY |
| oracle 不能 | `S19-command-line-options/01-dash-uppercase-i.t` | 0/8 | 0/8（`$*OS` 未対応・v2026.06 でも同じ） | `-I` + `@*INC` + `$*OS` イントロスペクション（is_run サブプロセス） |
| oracle 不能 | `S32-basics/xxPOS.t` | 11/64 で中断 | 53/64（raku も中断・v2026.06 でも同じ） | test 12 以降で深い機能により abort |
| 非目標 | `S05-capture/hash.t` | 拒否（raku と一致） | SORRY（廃止） | regex 内ハッシュキャプチャ `%<name>=(...)` は現行仕様で削除・予約。mutsu も raku 同様コンパイルエラー＝**実装不要** |
| 非目標 | `S05-mass/rx.t` | test 20 で中断 | SORRY（`::` NYI） | rakudo 自身 `::` backtracking control NYI でコンパイル不能。mutsu 残: `<commit>`、`::`/`:::` cut、廃止メタ文字の compile-time 例外群 |
| 非目標 | `S05-metasyntax/angle-brackets.t` | 51/95 で中断 | SORRY（v2026.06 は `<*xyz>` を compile 時拒否） | 到達可能な 51 subtest は全 pass。test 52 の code-string subrule（`{...}` 入り文字列を regex 化）は旧 raku も死ぬ＝MONKEY-SEE-NO-EVAL 相当が必要。新 raku は line 267 の未 fudge `<*xyz>` で走行すらしない |
| 非目標 | `S05-nonstrings/basic.t` | 0/5（ran 0） | SORRY（廃止） | 非文字列（stream/array）への regex マッチ＝早期 Perl 6 仕様。Rakudo 未実装のまま言語から削除 |
| 非目標 | `S06-advanced/caller.t` | 19 ran / plan 22 | SORRY | stale test・over-stated plan（22 planned vs 実質 19 assertion）。unpassable 確定（#3975） |
| 非目標 | `S06-advanced/return_function.t` | 1/4 で中断 | SORRY | 旧 Pair-flattening 挙動を期待。rakudo もコンパイル拒否。spectest.data 外 |
| 非目標 | `S10-packages/require-and-use--dead-file.t` | 4/18 | raku も FAIL | `require`/`use`/`no` の戻り値と `%*INC` 更新が未実装。ただし raku 自身も落ちるファイル |
| 非目標 | `S12-class/open_closed.t` | 8/9・notok 1 | SORRY（`oo` モジュール無し） | test 7 は roast 側の typo（`method h` の中身が `'called Qux.i'`）。upstream 修正待ち |
| 非目標 | `S12-meta/exporthow.t` | 1/12 で中断 | 2/12 で ABORT | rakudo 自身が line 22 で死ぬ（`ClassHOW` に `tryit` 無し） |
| 非目標 | `S12-traits/basic.t` | parse で 0 | SORRY（廃止） | 廃止 `trait_auxiliary` 構文。raku も拒否 |
| 非目標 | `S12-traits/parameterized.t` | 6/8 で中断 | SORRY（廃止） | 同上（`trait_auxiliary:<is>` カテゴリは言語から削除済み） |
| 非目標 | `S32-str/sprintf.t` | 170/174（fail 101-104） | SORRY（`zprintf` 未定義・v2026.06 でも同じ） | 残 101-104 は zero-pad `%g` sci の raku quirk（`34.1e+30`）。参照 raku に `zprintf` が無く正解アルゴリズムを検証不能 |
| 通過不能 | `S32-temporal/time.t` | 8/10・notok 2 | SORRY | テスト側に `flunk("FIXME ...")` の意図的失敗が 2 本 + raku も `gmtime`/`localtime`/`times` 未定義 |

## 今のおすすめ着手順

1. **① の残り 3 本**（`99problems-51-to-60.t` / `man-or-boy.t` / `deep-recursion-initing-native-array.t`）。
   4 本を一撃で、という当初の想定は外れた（上の内訳表を参照）——
   `99problems-41-to-50.t` はクロージャのスコープ修正（#4510）で片付いたが、残りは別物。
   なかでも `deep-recursion-initing-native-array.t` だけが真の「深い再帰」で、ここは機構の話
   （スタック拡張 + Raku フレーム数の上限ガード / 呼び出しフレームのヒープ化）になる。
2. **② パース不能 10 本を 1 本ずつ**。構文ごとに独立で、安い ★ が混じっている見込み
   （`q | … |` デリミタ・ユーザ定義 postfix `4.7k`・heredoc インデント等）。
3. **近道**: `6.c/S04-declarations/my-6c.t` は `OUTER::<$x>` の 1 subtest だけ
   （111/112）。`advent2011-day04.t` は 1/2。
4. **④ エラーメッセージ品質**（`error-reporting.t` 4/33）は PLAN §6 の同名タスクと同じ的なので、
   そちらを進めるときに roast の合否で駆動できる。
- S\* 系の表（非目標・oracle 不能・基盤待ち）は、mutsu 側の一般改善のついでに前進させるのはよいが、
  そのファイル単体の whitelist を目的にしない。`S32-str/format.t`・`S02-types/generics.t` は
  raku 更新で oracle が得られたが、必要基盤（RakuAST / 6.e generics）の大きさは変わらない。
