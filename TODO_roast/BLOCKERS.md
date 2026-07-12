# roast ブロッカー一覧

roast の失敗を「テストファイル単位」ではなく**根本原因単位**で追うための索引。
今どこを直せば何がまとめて動くかを判断するために使う。

**最終更新 2026-07-12**（全セクションを単一の残件表に統合。途中経過・完了報告の類は
`news/` に移し、本ファイルは「現在開いている残件」だけを持つ。数値は同日に再取得）。

## この文書の読み方

- **whitelist 済みになった項目はこの表から削除**し、詳細は `news/` に移す。
  完了した campaign の履歴も `news/2026-06.md` / `news/2026-07.md` を参照。
- per-file の詳細ログは `TODO_roast/S*.md`。全 roast × raku の結果一覧は
  [`raku-baseline.tsv`](raku-baseline.tsv)（生成: `scripts/roast-raku-baseline.sh`、
  解説: [`raku-baseline.md`](raku-baseline.md)）。
- **数字は執筆時点のスナップショット。** このファイルは複数セッションから並行更新される
  ため、着手前に必ず `MUTSU_FUDGE=1 prove -e target/debug/mutsu roast/<path>.t` で
  実際の pass/fail を取り直すこと。
- **raku 列の読み方**: ローカル参照実装は **Rakudo v2022.12（Raku 6.d）**。
  `SORRY` はローカル raku がコンパイル不能＝多くは 6.e 専用構文か廃止構文で、
  「テストが不正」を意味しない。raku が満点なのに mutsu が落ちるものだけが
  達成可能な実バグ（★）。

## 現在の前提

- whitelist は **1382**（2026-07-12、`wc -l roast-whitelist.txt`）。
- **roast 由来の大型共通ブロッカーは出尽くした。** かつての大型 campaign
  （真の lazy 配列 / dispatch・演算子 sugar の desugar / S17 並行・非同期 /
  第一級コンテナ container identity / cross-thread lexical writeback）はすべて完了済み。
- 残件は下表の個別ファイルのみで、campaign を組める共通根本原因は無い。
  **次の構造工事の選定は [PLAN.md](../PLAN.md) を正とする**（lexical-scope slot /
  GC post-3a / Batteries）。

## 残件一覧

分類の定義:

- **★達成可能** — ローカル raku が満点なのに mutsu が落ちる＝実バグ。whitelist 到達可能。
  ただし残っているものはいずれも単発 fix ではなく複数機能が必要（安い 1 ファイル勝ちは枯渇済み）。
- **基盤待ち** — RakuAST・6.e generics など大きな下位基盤の実装が必要。低優先・据え置き。
- **oracle 不能** — ローカル raku が SORRY／中断で参照検証できない（6.e 専用構文・6.d 制限）。
  mutsu 側の実装課題は残るが、正解をローカルで確認できないため優先度低。
- **非目標** — rakudo 自身も失敗する・roast 側の問題・廃止仕様。mutsu 側の一般改善の
  ついでに触れるのはよいが、**そのファイルの whitelist を目標にしない**。
- **通過不能** — テスト自体が意図的 flunk 等を含み、通せないと確定。

| 分類 | ファイル | mutsu | raku (v2022.12/6.d) | ブロッカー（一言） |
|---|---|---|---|---|
| ★達成可能 | `S32-hash/perl.t` | 47/55・notok 8 | 55/55 満点 | 残 8 は匿名 typed hash `$(my Any %)` の EVAL round-trip 型喪失＝パラメータ化ロール type-capture binding 待ち（匿名 typed 容器タグ付けが binding を壊す既存制限） |
| ★達成可能 | `6.c/S06-other/main-refactored.t` | 0/501 | 501/501 満点 | 新 MAIN インターフェース全体（USAGE 生成・引数 coercion 等）が必要 |
| 基盤待ち | `S32-str/format.t` | 26/49 で中断 | SORRY（6.e `Format`） | `Formatter::Syntax.parse`→Match、`Formatter.AST`→`RakuAST::Node` を要求＝**RakuAST サブシステム不在**。stub 化は禁止のため据え置き |
| 基盤待ち | `S02-types/generics.t` | 0/1 | SORRY（6.e） | 6.e coercion type 項 + `Array[T]` サブクラス化が必要。ローカル raku 自身もコンパイル不能で参照検証すらできない |
| oracle 不能 | `S02-names/pseudo-6d.t` | 116/159 で中断 | SORRY（6.e 要） | `::("CALLER")::<$*bar>` CALLER 疑似パッケージ deref 未対応 |
| oracle 不能 | `S02-names/pseudo-6e.t` | 79/202 で中断 | SORRY（6.e 要） | 同上（6.e 版） |
| oracle 不能 | `S02-names-vars/names.t` | 144/156・notok 3 | SORRY | test 142「Null PMC access when printing a var typed as ::foo」edge |
| oracle 不能 | `S02-types/array-shapes.t` | 35/43 で中断 | 38/43（raku も中断） | shaped array `.pairs` の `.value` が writable container を返さない |
| oracle 不能 | `S05-metasyntax/longest-alternative.t` | 57/62・notok 5 | SORRY | LTM（longest-token-match）の tie-break edge |
| oracle 不能 | `S06-advanced/return-prioritization.t` | 9/11・notok 2 | 4/11（mutsu 先行） | `return` in LEAVE phaser（別 lexical scope）edge |
| oracle 不能 | `S10-packages/basic.t` | 59/83・notok 9 | 6/83（mutsu 先行） | package 宣言 semicolon form のエラー検出 edge |
| oracle 不能 | `S12-attributes/trusts.t` | 9/15・notok 6 | SORRY | `trusts` によるクラス間 private アクセス未対応 |
| oracle 不能 | `S19-command-line-options/01-dash-uppercase-i.t` | 0/8 | 0/8（`$*OS` 未対応） | `-I` + `@*INC` + `$*OS` イントロスペクション（is_run サブプロセス） |
| oracle 不能 | `S32-basics/xxPOS.t` | 11/64 で中断 | 53/64（raku も中断） | test 12 以降で深い機能により abort |
| 非目標 | `S05-capture/hash.t` | 拒否（raku と一致） | SORRY（廃止） | regex 内ハッシュキャプチャ `%<name>=(...)` は現行仕様で削除・予約。mutsu も raku 同様コンパイルエラー＝**実装不要** |
| 非目標 | `S05-mass/rx.t` | test 20 で中断 | SORRY（`::` NYI） | rakudo 自身 `::` backtracking control NYI でコンパイル不能。mutsu 残: `<commit>`、`::`/`:::` cut、廃止メタ文字の compile-time 例外群 |
| 非目標 | `S05-metasyntax/angle-brackets.t` | 51/95 で中断 | 同地点で死ぬ | 到達可能な 51 subtest は全 pass。test 52 の code-string subrule（`{...}` 入り文字列を regex 化）は raku も死ぬ＝MONKEY-SEE-NO-EVAL 相当が必要 |
| 非目標 | `S05-nonstrings/basic.t` | 0/5（ran 0） | SORRY（廃止） | 非文字列（stream/array）への regex マッチ＝早期 Perl 6 仕様。Rakudo 未実装のまま言語から削除 |
| 非目標 | `S06-advanced/caller.t` | 19 ran / plan 22 | SORRY | stale test・over-stated plan（22 planned vs 実質 19 assertion）。unpassable 確定（#3975） |
| 非目標 | `S06-advanced/return_function.t` | 1/4 で中断 | SORRY | 旧 Pair-flattening 挙動を期待。rakudo もコンパイル拒否。spectest.data 外 |
| 非目標 | `S10-packages/require-and-use--dead-file.t` | 4/18 | raku も FAIL | `require`/`use`/`no` の戻り値と `%*INC` 更新が未実装。ただし raku 自身も落ちるファイル |
| 非目標 | `S12-class/open_closed.t` | 8/9・notok 1 | SORRY（`oo` モジュール無し） | test 7 は roast 側の typo（`method h` の中身が `'called Qux.i'`）。upstream 修正待ち |
| 非目標 | `S12-meta/exporthow.t` | 1/12 で中断 | 2/12 で ABORT | rakudo 自身が line 22 で死ぬ（`ClassHOW` に `tryit` 無し） |
| 非目標 | `S12-traits/basic.t` | parse で 0 | SORRY（廃止） | 廃止 `trait_auxiliary` 構文。raku も拒否 |
| 非目標 | `S12-traits/parameterized.t` | 6/8 で中断 | SORRY（廃止） | 同上（`trait_auxiliary:<is>` カテゴリは言語から削除済み） |
| 非目標 | `S32-str/sprintf.t` | 170/174（fail 101-104） | SORRY（`zprintf` 6.e） | 残 101-104 は zero-pad `%g` sci の raku quirk（`34.1e+30`）。参照 raku に `zprintf` が無く正解アルゴリズムを検証不能 |
| 通過不能 | `S32-temporal/time.t` | 8/10・notok 2 | SORRY | テスト側に `flunk("FIXME ...")` の意図的失敗が 2 本 + raku も `gmtime`/`localtime`/`times` 未定義 |

## 今のおすすめ着手順

- 「次の 1 本」に適する残件は無い。**構造工事の選定は [PLAN.md](../PLAN.md) を正とする。**
- roast 側で cheap win を探すときは上表の ★達成可能 から入る。ただし現存の ★ は
  いずれも複数機能を要する深いもので、単発 quick win ではない。
- 非目標・oracle 不能の項目は、mutsu 側の一般改善のついでに前進させるのはよいが、
  そのファイル単体の whitelist を目的にしない。
