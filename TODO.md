# mutsu - Roadmap

Goal: MoarVM より高速で実用的な Raku (Perl 6) 処理系を Rust で実装する。

KPI: `tools/run_all_roast.sh --save` の pass 数 (現在 257/1427)

開発方針: Phase 1-2 では機能実装を優先し、roast は実行しない。Phase 3 以降でマイルストーンごとに roast を実行して進捗を計測する。

---

## Phase 1: 言語コア (現在ここ)

最低限の Raku プログラムが動く状態。機能実装を優先する。

### 型システム
- [x] Int
- [x] Num (f64)
- [x] Str
- [x] Bool
- [x] Array
- [x] Hash
- [x] Range (`..`, `..^`)
- [x] Pair
- [x] FatRat
- [x] Nil
- [ ] Rat (有理数)
- [ ] Complex
- [ ] Set, Bag, Mix
- [ ] Enum
- [ ] Junction

### リテラル
- [x] 整数リテラル
- [x] 浮動小数点リテラル
- [x] 単一引用符文字列
- [x] 二重引用符文字列 + 変数補間
- [x] 角括弧ワードリスト `<a b c>`
- [ ] 数値中アンダースコア (`1_000_000`)
- [x] 基数表記 (`0x`, `0o`, `0b`)
- [ ] 指数表記 (`1e10`)
- [ ] Q/q/qq フォーム
- [ ] ヒアドク (`q:to/END/`)
- [ ] 正規表現リテラル

### 変数
- [x] `$` スカラー
- [x] `@` 配列
- [x] `%` ハッシュ
- [x] `$_` トピック変数
- [x] `$!` エラー変数
- [x] `$*` 動的変数 (`$*PID`, `$*CWD`, etc.)
- [ ] `&` コード変数
- [ ] `$?FILE`, `$?LINE` コンパイル時変数
- [ ] `$!` (属性アクセス)
- [ ] `$.` (公開属性)
- [ ] `$^` プレースホルダ変数

### 演算子
- [x] 算術: `+`, `-`, `*`, `/`, `%`, `%%`, `**`, `div`, `mod`
- [x] 文字列: `~`, `x`, `xx`
- [x] 比較: `==`, `!=`, `<`, `<=`, `>`, `>=`
- [x] 文字列比較: `eq`, `ne`, `lt`, `le`, `gt`, `ge`
- [x] 論理: `&&`, `||`, `!`, `//`, `and`, `or`, `not`
- [x] 代入: `=`, `:=`, `+=`, `-=`, `~=`, `*=`
- [x] インクリメント: `++`, `--` (前置/後置)
- [x] 三項: `?? !!`
- [x] スマートマッチ: `~~`
- [x] 範囲: `..`, `..^`
- [x] ペア: `=>`
- [ ] `so` (loose bool coercion)
- [ ] `^..`, `^..^` (range variants)
- [x] `<=>`, `leg`, `cmp` (comparison returning Order)
- [x] `eqv` (value equality)
- [ ] `===` (identity equality)
- [x] `?` (boolean context prefix)
- [x] `^` (upto: `^10` → `0..^10`)
- [ ] ビット演算: `+&`, `+|`, `+^`, `+<`, `+>`
- [ ] Junction 演算子: `&`, `|`, `^`
- [ ] メタ演算子: `R`, `X`, `Z`, `[op]`, `op=`
- [ ] Hyper 演算子: `>>op<<`

### 制御構文
- [x] `if` / `elsif` / `else`
- [x] `unless`
- [x] `while`, `until`
- [x] `loop` (C-style)
- [x] `repeat while` / `repeat until`
- [x] `for`
- [x] `for` with pointy block (`-> $x`)
- [x] `given` / `when` / `default`
- [x] `last`, `next`
- [x] `return`
- [x] `die`
- [x] `try` / `CATCH`
- [x] `with` / `without`
- [ ] `orwith`
- [ ] `proceed`, `succeed`
- [ ] `redo`
- [ ] ラベル付きループ
- [ ] `CONTROL { }`
- [x] `warn`
- [ ] `fail`
- [ ] `do { }` ブロック
- [ ] `gather` / `take`
- [x] 文修飾子: `if`, `unless`, `for`, `while`, `until`, `given`, `when`, `with`, `without`

### サブルーチン
- [x] `sub` 宣言
- [x] 複数パラメータ
- [x] 無名 sub / ラムダ (`-> $x { }`)
- [x] `return`
- [x] 名前付きパラメータ
- [x] デフォルト値
- [ ] 型制約 (`Int $x`)
- [ ] Slurpy パラメータ (`*@args`, `*%opts`)
- [ ] `multi sub`
- [ ] `proto sub`
- [ ] `MAIN` sub
- [ ] Closure (レキシカルキャプチャ)

### メソッド (組み込み)
- [x] `.defined`, `.Bool`, `.Str`, `.Int`, `.Numeric`
- [x] `.elems`, `.chars`, `.uc`, `.lc`
- [x] `.push`, `.pop`, `.shift`, `.unshift`, `.reverse`, `.sort`
- [x] `.keys`, `.values`, `.kv`, `.pairs`, `.exists`
- [x] `.split`, `.join`
- [x] `.WHAT`, `.perl`, `.gist`
- [ ] `.map`, `.grep`, `.first`
- [ ] `.flat`, `.unique`, `.squish`
- [ ] `.min`, `.max`, `.minmax`
- [ ] `.sum`, `.pick`, `.roll`
- [ ] `.comb`, `.contains`, `.starts-with`, `.ends-with`
- [ ] `.substr`, `.index`, `.rindex`
- [ ] `.chomp`, `.chop`, `.trim`
- [ ] `.abs`, `.sqrt`, `.ceiling`, `.floor`, `.round`
- [ ] `.base`, `.parse-base`
- [ ] `.Range` (型の範囲)
- [ ] `.new` (コンストラクタ)

### テストモジュール
- [x] `plan`, `done-testing`
- [x] `ok`, `nok`, `is`, `isnt`
- [x] `cmp-ok`, `like`, `unlike`
- [x] `skip`, `skip-rest`, `todo`, `bail-out`
- [x] `subtest`
- [x] `lives-ok`, `dies-ok`
- [x] `eval-lives-ok`
- [x] `throws-like`
- [x] `is-deeply`
- [x] `isa-ok`
- [x] `does-ok`, `can-ok`

### その他
- [x] `EVAL`
- [x] `use` (モジュールロード)
- [x] コメント (`#`, 埋め込みコメント, POD)
- [x] 文字列補間
- [ ] 正規表現 (基本)
- [ ] `say` のフォーマット改善 (.gist 準拠)

---

## Phase 2: オブジェクトシステム

Raku の OOP を実装。roast の S12 系テストを通す。

- [ ] `class` 宣言
- [ ] `has` 属性 (`has $.name`, `has $!private`)
- [ ] `method` 宣言
- [ ] `self`
- [ ] `new` コンストラクタ (自動生成)
- [ ] 継承 (`is Parent`)
- [ ] `role` 宣言と `does`
- [ ] `multi method`
- [ ] `BUILD` / `TWEAK` サブメソッド
- [ ] 型チェック
- [ ] Coercion (`Int(Str)` etc.)
- [ ] `enum`
- [ ] `subset`
- [ ] メソッド解決順序 (MRO, C3)

---

## Phase 3: 正規表現とグラマー

Raku の regex/grammar を実装。roast の S05 系テストを通す。

- [ ] 基本正規表現 (`/pattern/`)
- [ ] `rx//` フォーム
- [ ] `m//` マッチ演算子
- [ ] `s///` 置換演算子
- [ ] 文字クラス、量指定子、アンカー
- [ ] 名前付きキャプチャ (`$<name>`)
- [ ] `token`, `rule` 宣言
- [ ] `grammar` 宣言
- [ ] `proto token` と LTM
- [ ] Action クラス
- [ ] `make` / `made`

---

## Phase 4: 高度な機能

- [ ] Phaser (`BEGIN`, `END`, `ENTER`, `LEAVE`, `FIRST`, `NEXT`, `LAST`)
- [ ] `gather` / `take` (遅延リスト)
- [ ] Junction (`any`, `all`, `one`, `none`)
- [ ] `Promise`, `Supply`, `Channel` (並行処理)
- [ ] `react` / `whenever`
- [ ] `Proc::Async`
- [ ] `IO::Path` 完全版
- [ ] モジュールシステム (`unit module`, `export`, `use`)
- [ ] `MAIN` (コマンドライン引数解析)
- [ ] `CATCH` 型マッチング (`when X::AdHoc`)
- [ ] `use lib`
- [ ] Precompilation

---

## Phase 5: 性能と実用性

MoarVM を超える性能を目指す。

### コンパイラ基盤
- [ ] AST → バイトコード コンパイル
- [ ] レジスタベース VM or ネイティブコード生成
- [ ] 定数畳み込み
- [ ] インライン化
- [ ] 型推論による最適化
- [ ] Escape analysis

### ランタイム
- [ ] GC (世代別 or Reference counting + cycle detection)
- [ ] Native int/num (ボックス化回避)
- [ ] String rope / CoW
- [ ] Hash の最適化 (small hash optimization)
- [ ] Tail call optimization

### 実用性
- [ ] REPL
- [ ] デバッガ
- [ ] エラーメッセージの改善 (位置情報付き)
- [ ] `zef` パッケージマネージャ互換
- [ ] Inline::Perl5 互換レイヤー
- [ ] ネイティブバイナリ出力

---

## 設計方針

1. **Tree-walking interpreter → バイトコード VM** の段階的移行
2. **roast 互換性**を正の指標として使い、仕様準拠を測定
3. **MoarVM のアーキテクチャを参考にしつつ**、Rust の強みを活かした設計
4. **起動速度**を重視 (MoarVM の弱点)
5. **段階的な最適化**: まず正しく動かし、次に速くする
