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
| **① スタックオーバーフローで abort**（★同じ症状で原因は 4 つ別々 — 下節参照） | **完了** | `99problems-41-to-50.t`(#4510)・`99problems-51-to-60.t`(#4516)・`man-or-boy.t`（`&`-シジル free var スキャン + vouch — 下節）は無限再帰で修正済み、`deep-recursion-initing-native-array.t` は debug 計測の誤診（release では元から通る）＝whitelist 追加 | `fatal runtime error: stack overflow, aborting`（プロセス abort）。**4 本のうち 3 本は無限再帰、1 本は計測ミス。「本当に深い再帰でスタックが足りない」ものは 1 本も無かった** |
| **② パース不能（`===SORRY!===`）** | **0（完了）** | 10 本すべて解消（#4511 / #4514 / #4515 / #4517 / #4518 / #4522） | **このクラスタは枯渇。** 7 本が whitelist 到達、3 本はパースを通過して機能ギャップへ移行 — 下の「② の内訳」参照 |
| **③ ハング / タイムアウト** | 5 | `advent2012-day21.t`・`advent2013-day14.t`・`gather-with-loops.t`・`APPENDICES/A01-limits/{misc,overflow}.t` | 25s で打ち切り。gather×loop の遅延評価と limit 系 |
| **④ エラーメッセージ品質** | 2 | `error-reporting.t`（mutsu 4/33・raku 33/33）・`weird-errors.t`（26/36） | 「Parse error contains line number」等、**例外の文面・行番号・バックトレース**を検査するテスト。PLAN §6「エラーメッセージ品質向上」と同一の的 |
| **⑤ 個別機能ギャップ** | 残り | `advent2009-day24.t`（派生 grammar の拡張 — **test 2 の `.parse` がハングに変化**、要再調査）・`advent2010-day14.t`（**`nextsame` ではなく** メソッド内 `say` → user `$*OUT.print` の captured-outer 書き込みが消える dual-store バグ — 最小再現は下記）・`advent2011-day07.t`（`Metamodel::GrammarHOW` 継承）・`advent2011-day10.t`（`--doc` / `DOC INIT {}`）・`advent2011-day04.t`（`.wrap` 後の再帰呼び出しが wrapper を通らず cache が top-level のみ）・`advent2013-day10.t`（演算子 adverb: `1/3 :round` のユーザ infix 適用・prefix `!` 適用・`1+2-3 :adv` の最後の infix・`1**2**3 :adv` の最左 `**`・`m:nth[5]` の X::Comp::Group — fail 26,28,31-32,38）・`precompiled.t`（precomp） | 1 ファイル数本ずつ。**whitelist 到達 (2026-07-15)**: `advent2010-day22.t`（builtin 型の `^attributes`/`^methods(:local)` テーブル）・`advent2009-day20.t`（signature.raku のリテラル default 表示・sigil→Positional/Associative/Callable 型・stub map callback の遅延）・`advent2009-day18.t`（`my Cup of EggNog $mug` の `.WHAT.raku` が型引数を保持） |

**近道（1 subtest 差のファイル）**: `6.c/S04-declarations/my-6c.t` は **111/112**（唯一の失敗＝
test 57 `OUTER::<$x>` 疑似パッケージ）。`advent2011-day04.t` は 1/2、`advent2009-day24.t` は 3/4。

### advent2010-day14.t の実バグ（2026-07-15 root-cause）

fail 1・5 は `nextsame` ではない（`nextsame` の継承チェーンも mixin も単体では動く）。
実体は **メソッド内から `say` したときだけ、user 定義 `$*OUT.print` の captured-outer /
`our` 変数への書き込みが完全に消える** dual-store バグ:

```raku
our $out = "";
my $*OUT = class { method print(*@args) { $out ~= @args.join } }
class A { method m { say "y" } }
A.new.m;                      # print は呼ばれる（$*ERR 経由で確認済み）
# raku: $out eq "y\n" / mutsu: $out eq "" — env からも消えている
```

- sub 内からの `say` は正しく蓄積される（`write_to_named_handle` の Slice F reconcile が効く）。
- メソッド内から **明示的に** `$*OUT.print("y")` と書けば正しい（CallMethod op 経由）。
- 消えるのは `say`（`Say` op → `write_to_named_handle` → `call_method_with_values` の
  内部 redispatch）がメソッドフレームの中で走った場合のみ。`our` 変数ですら消えるので、
  メソッド return 時の env merge が内部 redispatch の書き込みを捨てていると思われる。

### ① の内訳（2026-07-14 に 4 本とも root-cause 済み）

「深い再帰でスタックが溢れる」という当初の診断は**誤り**だった。同じ abort に見えて原因は別物:

| ファイル | 実際の原因 | 状態 |
|---|---|---|
| `99problems-41-to-50.t` | **無限再帰**。クロージャのフレーム env が *caller* の env の scoped child で、捕捉 env を「既にあれば入れない」でマージしていたため、caller の同名レキシカルが callee 自身の捕捉を shadow する（＝レキシカルスコープが動的スコープに化ける）。P46 のアクションは節ごとに `my @args` を持つクロージャを作るので、内側の節が外側の `@args`（自分自身を含む）を読み自己再帰した | **#4510 で修正**。P41・P46 通過。以降は P47 の パラメータ化 `multi rule expr($p)` 待ち（⑤へ） |
| `99problems-51-to-60.t` | **無限再帰**（P57）。`CallMethodMut` が名前付きレシーバへのメソッド呼び出しのたびに `locals[slot] = env[name]` を無条件実行していたが、callee の env は「flat 化した caller + 自分の書き込み」で、**パラメータはスロットにしか無い**。よってレシーバが変わっていないとき caller の同名変数が callee のパラメータを上書きし、自己再帰 `add-to-tree($tree[1], …)` の `$tree` が呼び出し側のノードに巻き戻って葉に到達しなかった | **#4516 で修正**（rebind したときだけ書き戻す）。37 テスト完走。残 fail = test 8（平衡二分木の枝落ち）・test 21 |
| `man-or-boy.t` | **無限再帰**。#4510 と同じ「呼び出し側フレームの同名レキシカルがクロージャ自身の捕捉を shadow する」バグの残り。ただし今回 shadow されるのは **`&x1..&x5`（A の `&`-シジル・パラメータ）**で、`authoritative_free_vars` に入っていないため上書き install されない。詳細は下記 | **修正済み・whitelist 追加**（10/10）。(a) `GetCodeVar`/`CallOnCodeVar` を free var スキャンに追加（定数はシジル無し `"x1"`・レキシカルは `&x1` なので `&` を付けて記録）、(b) rw-arg sink 除外（`own_call_arg_sources`）から `&` 名を免除（raku は `&` パラメータへの `is rw` を拒否するので rebind 経路が無い） |
| `deep-recursion-initing-native-array.t` | **バグではなかった。** 約 20,000 段のネストが要るのは本当だが、**release では元から通る**（7 秒・3/3 安定）。この表の初回計測が **debug ビルド**だったせいで overflow に見えていただけ（debug は 1 フレームあたりのネイティブスタックが数倍。release の限界は 20k–40k 段で、必要な 20k はぎりぎり収まる） | **whitelist 追加**。スタック拡張も上限ガードも不要だった |

教訓: **abort の形（`stack overflow`）は原因を意味しない。** 深さを数えて raku と突き合わせ、
無限再帰か有限の深い再帰かを先に分けること（`raku` 側は 8 呼び出しで終わるのに mutsu が
1900 行トレースを吐く、で一発で分かる）。4 本中 3 本は無限再帰で、残る 1 本は**計測ミス**だった。

もう一つの教訓: **①の計測は debug ビルドで行われていた。** debug は 1 Raku フレームあたりの
ネイティブスタック消費が release の数倍あり、`deep-recursion-initing-native-array.t`（約 20,000 段）
は debug でだけ溢れていた。CI は release なので、**スタック深さが疑わしい失敗は必ず release で
確認する**（CLAUDE.md の「Delegate the full roast run to CI」節と同じ話）。

dual-store（locals ↔ env）由来のバグを疑うときの決定的 probe: **同一式の中で同じ変数を 2 回読む**。

```raku
sub f($tree, $d) {
    say $tree.WHAT.^name ~ ' | ' ~ $tree.WHAT.^name;
    return if $d >= 1;
    f($tree[1], $d + 1);
}
f([3, Any, Any], 0);
# raku:  Array | Array  /  Any | Any
# 修正前 mutsu:            Any | Array   ← 1 回目は正・2 回目が呼び出し側の値に巻き戻る
```

### man-or-boy の診断（2026-07-14・#4516 後の実測）

`.WHICH`（`Sub|ID`）で各 `&x` の同一性を追うと発散点が特定できる。**probe で `&x` を
*呼んで* はいけない**（`x4('TAG')` は B クロージャを起動して実行そのものを変えてしまう）。
`sub bee` のように名前を付けても mutsu は `&x.name` を空で返すので（別の小ギャップ）、
識別には `.WHICH` を使う。

```
A#1 k=4 d=0 B=Sub|33  x=[27 28 29 30 31]    <- 元のリテラル 5 本
A#2 k=3 d=1 B=Sub|41  x=[33 27 28 29 30]    <- A#1 の B が A#1 の中から発火 = 正しい
...
A#5 k=0 d=4 B=Sub|65  x=[57 49 41 33 27]    <- k<=0 なので x4(=Sub|33 = A#1 の B) を呼ぶ
A#6 k=2 d=1 B=Sub|73  x=[33 57 49 41 33]    <- 期待は [33 27 28 29 30]
```

A#6 は「A#1 の B が **A#5 の中から** 発火した」もの。B の本体は
`A(--$k, $B, &x1, &x2, &x3, &x4, ...)` なので `&x1..&x4` は **A#1 の捕捉 env**（= 27,28,29,30）
から読まれねばならないが、mutsu は **呼び出し元 A#5 のフレーム**の `&x1..&x4`（= 57,49,41,33）
を読んでいる。A#1 の中から発火する分（A#2）はチェーンが偶然一致するので正しく見え、
**別フレームから発火した瞬間に壊れる**。

これは #4510 で直した shadowing と同一のバグ。#4510 は「creator が捕捉後に変更しない
plain lexical」だけを上書き install する（`CompiledCode::authoritative_free_vars`）が、
`&x1..&x5` はそこに入っていないため don't-overwrite に落ち、呼び出し元の同名パラメータに
負ける。

**解決（2026-07-15）**: 原因は 2 段重ねで、両方を直す必要があった（片方だけの実験が
「効かなかった」のはこのため）:

1. **free var スキャン漏れ**: `&x1` の読みは `GetCodeVar`/`CallOnCodeVar` で行われ、定数は
   シジル無しの `"x1"`。`op_name_const_idx`（GetGlobal 系）に含まれないため `free_var_syms` に
   一切入らず、capture フィルタ（`&x1` は plain user lexical なのでシステム名ルールでも
   残らない）が落として **捕捉すらされていなかった**＝純粋な動的スコープで解決されていた。
   → スキャンに `&` を付けた `&x1` キーで記録する。
2. **rw-arg sink 除外**: A は `x4($k, &x1, …)` と `&x1..&x5` を引数として転送するので
   `own_call_arg_sources` が vouch を却下していた。だが **raku は `&`-シジル・パラメータへの
   `is rw` を拒否する**（"Can only use 'is rw' on a scalar ('$' sigil) parameter"）ので、
   `&` 引数が callee から rebind される経路はスペック上存在しない → `&` 名を免除。
   （`is raw` での rebind は理論上残るが既知ギャップとして許容。直接の `&f = …` rebind は
   name-write なので従来どおり捕捉される。）

**残ギャップも解消（同 PR の追いコミット）**:

- **ベアコール `x()`**: `CallFunc`/`ExecCall` 系は callee 名としてしか読みを記録しないため
  free var スキャン対象外だった。全 callee 名の登録は `&say` 等で `free_var_syms` が bloat
  する（クロージャ呼び出しごとの `free_at_entry` lookup 代）ので、**定義点で見えている
  enclosing `&`-シジル・レキシカルの集合**（`CompiledCode::outer_code_var_names`、
  `compile_closure_body`/`compile_sub_body` が親の `local_map` から transitively に伝搬）に
  一致する callee 名だけを `&name` free var として記録する。ゼロ bloat・レキシカルに正確。
- **多段ネストの authority 消失**（scalar でも同じだった pre-existing バグ）: vouch は
  「creator 直下の子」にしか焼かれず、`sub mk($x) { my $mid = sub { sub { $x } }; $mid.() }`
  の内側クロージャは中間フレームが vouch できない（`$x` は中間の local でない）ため、
  呼び出し側の同名レキシカルに再び shadow された。vouch は「サブツリー全体が書かない」
  ことを既に保証している（nested の free-var write は creator まで fold される）ので、
  **中間クロージャが同名を再宣言しない限り authority を子孫へ伝搬**して解決
  （`propagate_authoritative_down`）。

### ② の内訳（クラスタ完了・#4511 / #4514 / #4515 / #4517 / #4518 / #4522）

**whitelist 到達（7 本）**: `advent2010-day11.t`・`advent2013-day04.t`・`advent2014-day16.t`（#4511:
`qto`/`qqto` fused adverb、quote 語とデリミタ間の空白・改行、heredoc 本体の開始行、`q:to:c` の
選択的 adverb、lazy gather Seq の多引数 `for`）／`advent2009-day16.t`・`advent2009-day23.t`（#4514:
`when` 文修飾子、Match を RHS とする smartmatch、`(with …)`/`(given …)` の式化、`my@i` の
空白なし宣言）／`advent2012-day04.t`（#4515 の feed 継続行・無限 closure sequence の lazy `for`、#4517 のコンマより緩い演算子の優先順位、#4518 の分解シグネチャ＋grep バインダ）／`advent2012-day19.t`（#4522）／`6.c/APPENDICES/A04-experimental/01-misc.t`（#4523: `^find_method`/`^lookup` を具体値に対して引けるように、coercion 型の暗黙ソースを `Any` に、`Date`/`DateTime` の `.IO` を型オブジェクトで例外に）。

最後の 1 本 `advent2012-day19.t` は #4522 で全 10 subtest 通過。**優先順位トレイト付きユーザ定義
演算子**（`sub postfix:<k> is tighter(&infix:<*>)`）が項に一切適用されない問題（postfix ループが
「prefix 層で拾われる」として全部スキップしていたが、その拾い上げはユーザ定義 prefix の分岐内に
しか無かった）・英字 postfix の連鎖（`4kVW`）・`is looser(&postfix:<k>)` が k の登録レベルではなく
`PREC_PREFIX` を基準にしていたバグ・`Mu.ACCEPTS`・`Range.new` を実装。

**パースは通ったが subtest が残る（2 本）** — ②ではなく機能ギャップに移行:

| ファイル | 残 | ブロッカー |
|---|---|---|
| `integration/advent2012-day15.t` | 2/11 | phaser の文形式が独自スコープを作る（`NEXT (state $best) max= $_;` の `$best` が `LAST` から見えない）／`INIT` がメインラインより後に走る |
| `6.c/MISC/bug-coverage.t` | 3/17 | **① `.count-only`/`.bool-only`（Iterator API 一式）が最大の壁**。② `is-deeply gather { … }` を続けて 2 回書くと 2 本目が空になる（gather の遅延評価と Test 関数の引数評価の相互作用）。③ 等差列 `0.1, 2 ... 3` の第 2 項は raku では「前項＋公差」で再計算され Rat 2.0 になるが mutsu は種 Int 2 をそのまま出す。#4523 で 12/17 → 14/17 |

**②で唯一パースできないまま残った 1 本**: `integration/advent2012-day19.t`。
`sub postfix:<k> ($a) is tighter(&infix:<*>)` のように**優先順位トレイト付きのユーザ定義
postfix** が、項に対して一切適用されない（`4.7k` が SORRY）。`postfix/loop_.rs` の postfix
ループは登録済み優先順位が `PREC_PREFIX` 未満の postfix を「prefix 層で拾われる」として
スキップするが、その拾い上げループは**ユーザ定義 prefix 演算子の分岐の中にしか無い**ので、
`4.7` のような素の項では誰も消費しない。さらにこのファイルは `4kΩ`（別レベルの postfix の
連鎖）と `4.7kΩ ± 5%`（それらより緩い中置）を要求するので、**ユーザ定義演算子を精度順位表に
沿って適用する precedence climbing** が要る＝単発 fix ではない。付随して
`registry.rs` の `resolve_op_precedence` が `is looser(&postfix:<k>)` を
「登録済みレベル-5」ではなく「`PREC_PREFIX`-5」と解決するバグも直す必要がある。

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

1. ~~**① の残り 1 本 = `man-or-boy.t`**~~ **完了**（2026-07-15・whitelist 追加。
   「man-or-boy の診断」節の解決記録を参照）。①クラスタは全消化。
   ②パース不能クラスタも #4522 で全消化（上の表参照）。
3. **近道**: `6.c/S04-declarations/my-6c.t` は `OUTER::<$x>` の 1 subtest だけ
   （111/112）。`advent2011-day04.t` は 1/2。
4. **④ エラーメッセージ品質**（`error-reporting.t` 4/33）は PLAN §6 の同名タスクと同じ的なので、
   そちらを進めるときに roast の合否で駆動できる。
- S\* 系の表（非目標・oracle 不能・基盤待ち）は、mutsu 側の一般改善のついでに前進させるのはよいが、
  そのファイル単体の whitelist を目的にしない。`S32-str/format.t`・`S02-types/generics.t` は
  raku 更新で oracle が得られたが、必要基盤（RakuAST / 6.e generics）の大きさは変わらない。
