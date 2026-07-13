# ADR-0006: ベースライン（古典的）インタープリタ最適化の採否と優先順位

- **Status**: Accepted（2026-07-13 tokuhirom 承認）
- **Date**: 2026-07-13
- **Deciders**: tokuhirom, Claude
- **関連**: [ADR-0004](0004-jit-strategy.md)（JIT・threaded dispatch 凍結の判断）,
  [ADR-0005](0005-nanbox-representation-encoding.md)（8B Value）,
  [docs/opcode-design-review.md](../opcode-design-review.md)（opcode 残件 §2/§5/§6）,
  PLAN.md §5, PERFORMANCE.md

> ADR-0004 で JIT（Cranelift, J5 で既定 on・2026-07-13）まで到達した一方、
> CPython / Ruby (YARV) / PHP (opcache) が**JIT 以前から**備えている古典的な
> バイトコード最適化はほぼ未実装であることが監査（2026-07-13）で判明した。
> 本 ADR はその監査結果を記録し、各施策の採否・優先順位・Raku 固有の安全条件を決める。

---

## 1. Context（背景）

- 実行系: Parser → Compiler（`src/compiler/`）→ bytecode（~340 opcode）→ VM。
  コンパイラは AST を**一対一で**opcode 列に落とし、emit 後の最適化パスは
  `compute_upvalues` / `compute_needs_env_sync`（変数アクセス系）のみ。
- 主要言語処理系の対応状況（比較のための整理）:
  - **CPython**: AST レベル定数畳み込み・peephole（jump 最適化）・3.11 adaptive
    specializing interpreter（quickening + inline cache）。
  - **Ruby (YARV)**: peephole・specialized instructions（`opt_plus` 等）・
    operand/instruction unification・call-site inline cache。
  - **PHP (opcache)**: SSA ベースの最適化パス群 — 定数畳み込み・定数伝播・DCE・
    jump 最適化・型推論による opcode 特殊化。
- mutsu が**既に持つ**同等物: スーパーインストラクション（`WhileLoop`/`ForLoop` 等の
  複合 op・`StringConcat(n)`・`JunctionAnyN` 等）、グローバルメソッドキャッシュ
  （`method_resolve_cache` + monomorphic 1-entry `last_method_resolve`）、
  indexed locals、COW env、Symbol インターン、NaN-box small-Int（±2^47 インライン）、
  grammar token の静的 fold（#4460）。
- つまり「変数アクセス・呼び出し・データ表現」の最適化は進んでいるが、
  **「opcode 列そのものを短くする」系の最適化が丸ごと欠けている**。

### 1.1 監査結果（2026-07-13、実装状況の正本）

| 施策 | CPython | YARV | opcache | mutsu の現状 | 判定 |
|---|---|---|---|---|---|
| 定数畳み込み | ✅ | ✅ | ✅ | ❌ `compile_expr_binary` はリテラル同士でも常に演算 op を emit（`expr_binary.rs`） | **採用** (§2.1) |
| `constant` 読みのインライン化 | ✅ (co_consts) | ✅ | ✅ | ❌ BEGIN 時評価はするが読みは `GetLocal`/`GetBareWord`/`GetGlobal`（`stmt.rs` constant 宣言） | **採用** (§2.2) |
| 定数条件の分岐除去・DCE | ✅ | ✅ | ✅ | ❌ `if False {...}` も条件評価+`JumpIfFalse` を emit | **採用** (§2.2) |
| peephole（冗長 op 除去/融合） | ✅ | ✅ | ✅ | ❌ emit 後の書き換えパスなし（変数系の `compute_*` のみ） | **採用** (§2.3) |
| 定数プール重複排除 | ✅ | ✅ | ✅ | ❌ `add_constant` は無条件 `constants.push`（`opcode.rs:2927`） | **採用** (§2.4) |
| per-call-site inline cache (PIC) | ✅ (3.11) | ✅ | 部分 | 部分: グローバルキャッシュ+monomorphic 1-entry のみ、opcode 埋め込みキャッシュなし | **保留** (§3.1) |
| quickening / adaptive specialization | ✅ (3.11) | 部分 | ✅ | ❌ | **却下** (§3.2) |
| threaded dispatch | ✅ (computed goto) | ✅ | ✅ | ❌ | **却下済み**（ADR-0004 §2.5 J0 で凍結 — 再論しない） |
| 小整数キャッシュ | ✅ | ✅ (Fixnum) | — | **実質達成**: NaN-box small-Int はヒープ非確保（層3b） | 対応不要 |
| 文字列インターン（値） | 部分 (fstring) | ✅ (frozen str) | ✅ | ❌ `Value::Str` は都度 `Arc<String>`（Symbol は別途インターン済み） | **保留** (§3.3) |

### 1.2 実測根拠（新設ベンチ、ローカル release・3 回最小値・2026-07-13）

判定を「一般論」でなく mutsu の実測で裏づけるため、対象最適化に感応する
自然なワークロードを `benchmarks/` に 4 本新設して測った:

| ベンチ | interp | JIT on | raku | 比（interp） | 主な感応対象 |
|---|---|---|---|---|---|
| time-parts（タイムスタンプ分解） | 0.73s | 0.75s | 0.21s | **3.5x** | 定数畳み込み・peephole |
| debug-guard（定数ガード付きホット関数） | 0.42s | 0.43s | 0.19s | **2.2x** | constant インライン化・定数条件 DCE |
| poly-call（3 クラス混合の area 集計） | 0.10s | 0.10s | 0.21s | 0.48x | PIC（保留の基準線） |
| word-count（単語頻度カウント） | 0.10s | 0.10s | 0.22s | 0.45x | 文字列インターン（保留の基準線） |

- **time-parts 3.5x / debug-guard 2.2x は現行スイート最悪の bench-fib 1.78x を超える**。
  既存スイートが「関数呼び出し・OOP・コレクション」に寄っていて、
  素朴な式・文の密度が高いコードの弱さを見落としていた。
- **JIT on でも改善しない**（interp と同時間）— この 2 本の遅さは dispatch でなく
  「実行する opcode がそもそも多い」ことに由来し、JIT と独立の直交レバーである証拠。
- opcode ヒストグラム（`MUTSU_VM_STATS=1`）の内訳:
  - time-parts: total 7.6M op/run のうち `LoadConst`=1.7M・`Mul`=1.0M（うち
    リテラル定数式 `60*60*24` 等の再計算が iteration あたり Mul×6 = 600k）、
    加えて `SetSourceLine`=600k・`Mark*`/`SetVarDynamic`=1.5M の administrative op。
  - debug-guard: `if DEBUG` が毎回 `GetBareWord`+`JumpIfFalse`（各 200k）として実行
    される。constant インライン化 + 定数条件 DCE でブロックごと消える形。
- poly-call / word-count は**既に raku 比で優位**（0.48x / 0.45x）。PIC・文字列インターンを
  「他言語にあるから」という理由で実装する緊急性はない、という判定の根拠。

## 2. Decision — 採用する施策（優先順）

すべて**コンパイル時（emit 時/emit 後）の変換**で、実行時の適応機構を持たない。
ADR-0004 の「deopt を作らない」方針と同型: 静的に安全と証明できる場合のみ変換し、
証明できなければ何もしない（フォールバックは「無変換」なので flaky になり得ない）。

### 2.1 定数畳み込み（constant folding）

リテラル同士の純粋演算（算術・比較・文字列連結・論理）を emit 時に評価して
`LoadConst` 1 個に置換する。実装は `compile_expr_binary` の入口で
「両オペランドが畳み込み済み定数か」を見る bottom-up 方式（AST は変更しない）。

**Raku 固有の安全条件**（畳み込んでよいのは以下を全て満たす場合のみ）:

1. **演算子オーバーライド不在**: 同一コンパイル単位に当該演算子の
   `multi sub infix:<op>` 宣言（`sub infix:<op>` 含む）が存在する場合、
   そのファイル全体で fold を無効化する（宣言スコープの精密解析はしない —
   保守的すぎて畳み込み機会を失っても、誤畳み込みよりよい）。
   JIT Tier B が同じ問題を infix-override ガードで解いている
   （pin: `t/jit-tier-b-infix-override.t`）— 同じ検出結果を共有する。
2. **実行時と同一の演算実装**: 畳み込みは VM の native 演算（`builtins/arith.rs` 等）を
   そのまま呼んで評価する。独自の「コンパイル時演算」を書かない
   （Int の BigInt 昇格・Rat 化 `1/3`・型昇格規則が実行時と自動で一致する）。
3. **例外を投げ得る式は畳み込まない**: 評価が `Err`（`0 div 0` 等）になったら
   無変換で残す（実行時に投げる意味論を保存。コンパイル時エラーへの昇格はしない）。
4. **畳み込み結果が value 型のみ**: Int/Num/Str/Bool/Rat。コンテナや Instance に
   なる式は対象外（同一性が観測可能）。

### 2.2 `constant` 読みのインライン化 + 定数条件 DCE

- `constant NAME = ...` は言語仕様上コンパイル時（BEGIN）に確定し再代入不可。
  現在は BEGIN 評価した値をスロット/env に入れて実行時に読んでいる
  （`src/compiler/mod.rs:180` 付近が「GetLocal に変える」ことを意図的に避けているのは
  *宣言前使用の検出*のためで、インライン化自体の否定ではない）。
  値が value 型（Int/Num/Str/Bool/Rat/Nil）の `constant` の読みを `LoadConst` に置換する。
  コンテナ値の constant は同一性が観測可能なので対象外（従来どおり）。
- インライン化で条件がリテラル定数になった `if`/`unless`/`while` は分岐ごと解決する:
  `if False {...}` はブロックを emit しない（elsif/else 連鎖は残りを通常コンパイル）。
  `constant DEBUG = False; if DEBUG { note ... }` というガード付きロギングは
  実コードの頻出パターンで、debug-guard ベンチ（2.2x）がこれを測る。
- **safety**: DCE で消すのは「値が確定した分岐の非到達側」のみ。副作用解析は不要
  （条件式自体がリテラル定数のときだけ発動するため）。ブロック内の `BEGIN`/宣言の
  扱いは Rakudo の意味論（コンパイルはされる）に合わせ、**宣言を含むブロックは
  消さず従来コンパイルに落とす**保守則から始める。

### 2.3 peephole — administrative opcode の削減・融合

time-parts の per-iteration 64 op のうち約 27% が administrative
（`SetSourceLine`・`MarkVarDeclContext`・`MarkExplicitInitializerContext`・
`SetVarDynamic`・`CheckReadOnly`）。古典的な push/pop 除去より、
この mutsu 固有の administrative 列の静的化・融合の方が実測上大きい:

- 連続する `SetSourceLine` の重複除去（値が変わらない限り 1 個）、
  および `my $x = <expr>` の宣言 3-4 op 列の複合 op 化ないし静的フラグ化。
- jump-to-jump は複合ループ op のおかげで現状ほぼ発生しないため優先しない。
- 対象の選定は**ヒストグラム駆動**（`MUTSU_VM_STATS=1` の opcode_histogram）で行い、
  美学による書き換えはしない — [opcode-design-review.md](../opcode-design-review.md) §6
  の既存方針をそのまま peephole にも適用する。

### 2.4 定数プール重複排除（dedup）

`add_constant` に FxHashMap の逆引き（値→index、hashable な value 型のみ）を足して
同値定数を共有する。実行速度への直接効果は小さいが、コンパイラ各所が名前文字列を
使用箇所ごとに push している現状は `CompiledCode` のメモリと cache locality を
無駄にしており、mzef の大規模 populate（数万 dist のコンパイル）でも効く方向。
実装が最小（1 関数）なので最初のスライスに向く。

### 実装順序

依存関係と費用対効果から: **(1) §2.4 dedup →(2) §2.1 畳み込み →
(3) §2.2 constant インライン化+DCE →(4) §2.3 peephole**。
(2) と (3) は同じ「emit 時定数性トラッキング」基盤を共有する。
ゲート: 各スライスで time-parts / debug-guard の改善を bench CI で確認し、
既存 12 ベンチに退行がないこと。

## 3. 保留・却下する施策（と、再開のトリガ）

### 3.1 per-call-site inline cache（PIC）— 保留

- 現状の monomorphic 1-entry（`last_method_resolve`）+ グローバルキャッシュで
  poly-call が **0.48x と既に raku 比優位**。メガモーフィックサイトでの
  1-entry ミス単価は FxHashMap ルックアップ 1 回で、現時点で律速でない。
- 再開トリガ: profile で `method_resolve_cache` ルックアップが上位に入る、
  または JIT J4d（呼び出しインライン化）で call-site 単位のキャッシュ構造が
  どのみち必要になったとき。その場合 **opcode 側でなく JIT の CallMethod shim 側**
  （ADR-0004 J3）に設ける — インタプリタと JIT で二重のキャッシュ機構を作らない。

### 3.2 quickening / adaptive specialization — 却下

CPython 3.11 の主戦力だが、mutsu では **JIT Tier B（NaN-box タグ分岐の
インライン算術）が同じ役割をより大きな天井で担う**。実行時に opcode 列を
書き換える機構は resume/再入・並行実行との相互作用が複雑で、
threaded dispatch と同じ「JIT との二重投資」判断（ADR-0004 §2.5）を適用する。
JIT が頓挫した場合のみ threaded dispatch とセットで再検討。

### 3.3 Value 文字列インターン — 保留

word-count（文字列キーのハッシュ集計）が **0.45x で既に優位**。
ハッシュキーは既に `Symbol` ベースの経路が太く、`Value::Str` の Arc 共有で
コピーは O(1)。再開トリガ: profile で文字列 alloc/hash が上位に入ったとき。
その場合も全域インターンでなく、NaN-box の小文字列インライン化（SSO）を
先に検討する（層3b の設計と整合）。

## 4. Consequences（結果）

- 新設ベンチ 4 本（time-parts / debug-guard / poly-call / word-count）を
  `benchmarks/` に追加 — `bench-ci.sh` の glob に自動収載され、
  採用施策の各スライスの効果と、保留施策の基準線が bench CI 履歴に残る。
- 採用施策はすべて「静的に安全な場合のみ・フォールバックは無変換」なので、
  roast への意味論リスクは低い。オーバーライドガード（§2.1-1）が唯一の
  意味論境界で、pin テストを実装スライスに含めること。
- 畳み込み・インライン化は **JIT の入力（opcode 列）を短くする**ため、
  Tier B のカバレッジ・コンパイル時間にもプラスに働く（独立ではあるが順方向）。
