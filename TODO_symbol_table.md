# Symbol Table (String Interning) Implementation Plan

## Problem

現在、メソッド名・関数名・クラス名・パッケージ名などの識別子はすべて `String` で保持されており、ディスパッチ時に都度文字列比較が行われている。

### 影響箇所

1. **ネイティブメソッドディスパッチ** (`builtins/methods_0arg/`, `builtins/methods_narg.rs`)
   - `match method { "Bool" => ..., "Str" => ..., ... }` で100〜200個の文字列リテラルと比較
   - 呼び出し頻度が非常に高い（ほぼ全てのメソッド呼び出しで通過）

2. **ネイティブ関数ディスパッチ** (`builtins/functions.rs`)
   - 同様に `match name { "say" => ..., "print" => ..., ... }` で40〜80個の比較

3. **スローパスメソッドディスパッチ** (`runtime/methods.rs`, `runtime/methods_mut.rs`)
   - 疑似メソッド (WHAT, WHO, HOW等) 50個以上の文字列比較
   - 型別メソッドディスパッチで200個以上の文字列比較

4. **演算子ディスパッチ** (`runtime/builtins_operators.rs`)
   - 80個以上の演算子文字列比較

5. **データ構造内の名前保持**
   - `Value::Instance { class_name: String, ... }` — インスタンス生成のたびにクラス名をclone
   - `Value::Sub(Arc<SubData>)` — `SubData.name: String`, `SubData.package: String`
   - `AST Expr::MethodCall { name: String, ... }` — パースのたびにString確保
   - `AST Expr::Call { name: String, ... }` — 同上
   - `HashMap<String, FunctionDef>` — 関数テーブルのキーがString

6. **VM定数プール** (`compiler/`, `vm/`)
   - メソッド名は定数プールに `Value::Str(name)` として格納され、`const_str()` で取り出す
   - 取り出し後に `.to_string()` でコピーが発生する箇所がある

## Solution: Symbol型によるString Interning

`Symbol` 型（内部的には `u32` のID）を導入し、全ての識別子を一度だけテーブルに登録する。以降の比較は整数比較（O(1)）になる。

### 設計

```rust
// src/symbol.rs

use std::collections::HashMap;
use std::sync::RwLock;

/// Interned symbol — 整数比較で高速にマッチングできる
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Symbol(u32);

/// Global symbol table (プロセス内で1つ)
pub struct SymbolTable {
    str_to_id: HashMap<String, Symbol>,
    id_to_str: Vec<String>,
}

/// Thread-safe global instance
static SYMBOLS: RwLock<SymbolTable> = ...;

impl Symbol {
    /// 文字列をinternしてSymbolを返す（既存なら既存IDを返す）
    pub fn intern(s: &str) -> Symbol { ... }

    /// Symbolから文字列への逆引き（表示用）
    pub fn as_str(&self) -> &str { ... }
}
```

### `match` 文の書き換えパターン

Before:
```rust
pub fn native_method_0arg(target: &Value, method: &str) -> ... {
    match method {
        "Bool" => { ... },
        "Str"  => { ... },
        _ => None,
    }
}
```

After:
```rust
// モジュールレベルで一度だけ初期化
lazy_static! {
    static ref SYM_BOOL: Symbol = Symbol::intern("Bool");
    static ref SYM_STR:  Symbol = Symbol::intern("Str");
}

pub fn native_method_0arg(target: &Value, method: Symbol) -> ... {
    if method == *SYM_BOOL { ... }
    else if method == *SYM_STR { ... }
    else { None }
}
```

**Note**: Rustの `match` は文字列リテラルに対してコンパイラが最適化（jump table等）を行うため、`Symbol` を使った場合でも `match` は使えない（定数パターンとして使えない）。`if-else` チェーンか、`HashMap<Symbol, fn(...)>` によるテーブルディスパッチに変換する必要がある。ただし、Symbolは `u32` の整数比較なので `if-else` チェーンでも十分高速。

---

## Phase 1: Symbol型の導入と基盤整備 ✅ DONE (PR #685)

**Goal**: `Symbol` 型とグローバルシンボルテーブルを導入。既存コードに影響なし。

### 影響範囲
- **新規**: `src/symbol.rs` (約100行)
- **変更**: `src/main.rs` に `mod symbol;` 1行追加のみ
- **既存コードへの影響なし**

### Tasks

1. `src/symbol.rs` を新規作成
   - `Symbol` struct (`u32` wrapper, `Copy + Clone + Eq + Hash`)
   - `SymbolTable` struct (`HashMap<String, Symbol>` + `Vec<String>`)
   - `OnceLock` or `lazy_static` でグローバルインスタンス
   - `Symbol::intern(s: &str) -> Symbol`
   - `Symbol::as_str() -> &str` (逆引き)
   - `impl Display for Symbol` (デバッグ/表示用)
2. `src/lib.rs` (or `src/main.rs`) に `mod symbol;` を追加
3. 単体テスト
   - intern の冪等性 (`intern("foo") == intern("foo")`)
   - `as_str` の往復
   - 異なる文字列は異なるSymbol

### Merge criteria
- `make test` pass
- `make roast` pass
- 既存コードへの変更なし（新規ファイル追加のみ）

---

## Phase 2: AST内の識別子をSymbolに置き換え ✅ DONE (PR #687, #688, #689)

**Goal**: パーサーが生成するAST内の名前フィールドを `String` → `Symbol` に変換。

### 影響範囲
- `src/ast.rs` 内の約31箇所の `name: String` フィールドを変更
- 波及先: 約**49ファイル**、約**450箇所**（parser/, compiler/, runtime/, vm/ の全域）
- 変更の大半は機械的な型置き換え（`String` → `Symbol`、`&name` → `name.as_str()`、`.clone()` 削除）
- サブフェーズに分けることでPRあたりの差分を抑える

### サブフェーズと見積もり

- **2a**: `Expr::Call.name` と `Expr::MethodCall.name` のみ（最も高頻度）
  - `ast.rs` 2箇所 + 波及先 約**15〜20ファイル**、約**100〜150箇所**
  - 差分: 約200〜300行
- **2b**: `FunctionDef`, `HyperMethodCall`, その他の `Expr` variants
  - `ast.rs` 約10箇所 + 波及先 約**15〜20ファイル**、約**100〜150箇所**
  - 差分: 約200〜300行
- **2c**: `Stmt` 内の識別子（クラス名、ロール名、use/require先など）
  - `ast.rs` 約15箇所 + 波及先 約**20〜25ファイル**、約**150〜200箇所**
  - 差分: 約300〜400行

### Tasks

1. `src/ast.rs` の以下のフィールドを `String` → `Symbol` に変更:
   - `Expr::Call { name: Symbol, ... }`
   - `Expr::MethodCall { name: Symbol, ... }`
   - `Expr::HyperMethodCall { name: Symbol, ... }`
   - `Expr::FunctionDef { name: Symbol, package: Symbol, ... }`
   - `Expr::Variable` 内の名前フィールド（該当する場合）
   - `Stmt` 内の関数名・クラス名フィールド
2. パーサー (`src/parser/`) を修正し、名前文字列をパース時に `Symbol::intern()` する
3. コンパイラ (`src/compiler/`) を修正し、`Symbol` を受け取って定数プールに格納
4. VM (`src/vm/`) のメソッド名・関数名取り出し箇所を修正
5. ランタイム (`src/runtime/`) のAST参照箇所を修正

### 注意点
- `Symbol` は `Copy` なので `.clone()` が不要になり、コードがシンプルになる
- パーサーの出力を受け取る全ての下流コードに波及するが、変更自体は機械的

### Merge criteria (サブフェーズごと)
- `make test` pass
- `make roast` pass
- `cargo clippy -- -D warnings` pass

---

## Phase 3: ビルトインディスパッチのSymbol化 ✅ DONE (PR #690)

**Goal**: ネイティブメソッド/関数のディスパッチを文字列比較から整数比較に変換。

### 影響範囲
- 対象ファイル: **5〜8ファイル** (`builtins/methods_0arg/`, `builtins/methods_narg.rs`, `builtins/functions.rs`, `runtime/methods.rs`, `vm/vm_helpers.rs`, `vm/vm_call_ops.rs`)
- `match method { "Str" => ... }` のアーム約**300〜400個**を書き換え
- ファイル数は少ないが、各ファイル内の変更行数が多い（1ファイルあたり数百行の差分）

### サブフェーズと見積もり

- **3a**: well-known symbols定義 + `methods_0arg` の変換
  - `symbol.rs` にwell-known定義追加 + `methods_0arg/` 5ファイル
  - 約100個のmatchアーム書き換え、差分: 約300〜400行
- **3b**: `methods_narg` の変換
  - `methods_narg.rs` 1ファイル
  - 約80個のmatchアーム書き換え、差分: 約200〜300行
- **3c**: `functions.rs` の変換
  - `functions.rs` 1ファイル
  - 約60個のmatchアーム書き換え、差分: 約150〜200行
- **3d**: `runtime/methods.rs` のスローパス変換
  - `methods.rs` + `methods_mut.rs` 2ファイル
  - 約200個のmatchアーム書き換え、差分: 約500〜700行

### Tasks

1. well-known symbolの事前登録
   - `src/symbol.rs` に `pub mod well_known { ... }` を追加
   - よく使われるメソッド名 (Bool, Str, Int, Num, gist, say, print, etc.) を `lazy_static` で事前intern
2. `builtins/methods_0arg/` の `match method { ... }` を `Symbol` ベースのディスパッチに変換
   - 方式A: `if method == SYM_BOOL { ... } else if ...` チェーン
   - 方式B: `HashMap<Symbol, fn(&Value) -> Option<Result<Value, RuntimeError>>>` テーブル
   - 方式Bの方がスケーラブルだが、初期化コストあり。ベンチマークで判断
3. `builtins/methods_narg.rs` の1-arg/2-argディスパッチも同様に変換
4. `builtins/functions.rs` のネイティブ関数ディスパッチも同様に変換
5. `runtime/methods.rs` の疑似メソッドディスパッチも同様に変換
6. 呼び出し側 (`vm/vm_helpers.rs`, `vm/vm_call_ops.rs`) が `Symbol` を渡すように変更

### 注意点
- 現在の `match method_str { ... }` パターンは Rust コンパイラが最適化する可能性がある
- `HashMap<Symbol, fn>` テーブルは初期化コストと引き換えにO(1)ディスパッチを保証
- ベンチマーク (Phase 5) の結果次第でディスパッチ戦略を選択

### Merge criteria (サブフェーズごと)
- `make test` pass
- `make roast` pass

---

## Phase 4: データ構造内の名前をSymbol化 ✅ DONE (PR #691, #692, #693)

**Goal**: ランタイムのデータ構造内の識別子文字列を `Symbol` に置き換え、clone/比較コストを削減。

### 影響範囲
- `src/value/mod.rs` の `Value` enum 5〜6フィールド + `SubData` 3フィールドを変更
- `Value` をパターンマッチしている箇所: **30〜40ファイル**、約**200〜300箇所**
- 主に `class_name`/`package`/`name` を文字列として参照している箇所に `.as_str()` 追加
- `Display` / `gist` / エラーメッセージ出力で逆引きが必要

### サブフェーズと見積もり

- **4a**: `Value::Instance { class_name }` と `Value::Package` のみ
  - `value/mod.rs` 2箇所 + 波及先 約**20〜25ファイル**、約**100〜150箇所**
  - `class_name == "Foo"` → `class_name == SYM_FOO` or `class_name.as_str() == "Foo"` に書き換え
  - 差分: 約300〜400行
- **4b**: `SubData.name`, `SubData.package`, `Value::Routine`, `Value::Enum` 等
  - 波及先 約**15〜20ファイル**、約**100〜150箇所**
  - 差分: 約200〜300行
- **4c**: `Interpreter.functions` の `HashMap<String, FunctionDef>` → `HashMap<Symbol, FunctionDef>`
  - `runtime/dispatch.rs`, `runtime/mod.rs`, `runtime/registration.rs` 等 約**5〜8ファイル**
  - キー生成方式の変更が必要（`format!()` ベースから `Symbol` ベースへ）
  - 差分: 約100〜200行

### Tasks

1. `Value` enum内の名前フィールド:
   - `Instance { class_name: Symbol, ... }` (現在 `String`)
   - `Package(Symbol)` (現在 `String`)
   - `Routine { package: Symbol, name: Symbol, ... }`
   - `Enum { enum_type: Symbol, key: Symbol, ... }`
   - `CompUnitDepSpec { short_name: Symbol }`
2. `SubData` 内の名前フィールド:
   - `name: Symbol` (現在 `String`)
   - `package: Symbol` (現在 `String`)
   - `params: Vec<Symbol>` (現在 `Vec<String>`)
3. `Interpreter` の関数テーブル:
   - `functions: HashMap<Symbol, FunctionDef>` (現在 `HashMap<String, FunctionDef>`)
   - キー生成を `format!("{name}/{arity}")` から `Symbol` ベースに変更
   - multi-dispatch用の型付きキーは `(Symbol, usize, Vec<Symbol>)` タプルなどに
4. `HashMap<String, Value>` (属性、環境、named引数) は対象外
   - ユーザーが動的に生成する文字列キーを含むため、Symbolize の効果が薄い
   - ただし、属性名 (`$.name` 等) は将来的にSymbol化の候補

### 注意点
- `Value` enumのサイズに影響する（`String` は24bytes, `Symbol` は4bytes → 大幅縮小）
- `Value::clone()` のコストも削減（`String::clone()` がなくなる）
- 表示系 (`Display`, `gist`, `raku`) で `Symbol::as_str()` への逆引きが必要

### Merge criteria (サブフェーズごと)
- `make test` pass
- `make roast` pass

---

## Phase 5: ベンチマークと最適化 ✅ DONE

**Goal**: Symbol化の効果を測定し、ディスパッチ戦略を最終決定。

### ベンチマーク結果 (100,000 iterations, release build)

| Benchmark | Before (avg) | After (avg) | Change |
|---|---|---|---|
| function-call | 6.46s | 4.39s | **-32%** |
| native-function | 0.48s | 0.56s | +17% |
| string-methods | 0.43s | 0.49s | +14% |
| method-dispatch | timeout (60s) | timeout (60s) | N/A (separate issue) |
| object-creation | timeout (60s) | timeout (60s) | N/A (separate issue) |

### 分析

- **function-call**: ユーザー定義関数呼び出しで32%の高速化。HashMap<Symbol>のキー比較がu32整数比較になった効果が大きい。
- **native-function / string-methods**: RwLock読み取りロックの取得オーバーヘッドにより若干の低下。ネイティブメソッドはPhase 3で`Symbol::resolve()`を呼んで`match &str`に戻しているため、Symbol→String→matchの変換コストが加算される。
- **method-dispatch / object-creation**: 60秒でタイムアウト。Symbol化とは無関係のパフォーマンス問題（VM/インタプリタ間の往復コスト等）。

### ディスパッチ戦略の結論

現行のPhase 3方式（API境界でSymbolを受け取り、内部は`match &str`のまま）を維持する。理由:
1. 関数テーブルルックアップ（Phase 4c）で最大の効果が得られており、ディスパッチ内部のmatchを変えても追加効果は限定的
2. Rustの`match &str`は十分に最適化されており、Symbol整数比較への変換は複雑さに見合わない
3. RwLock取得コストの影響を考慮すると、`resolve()`呼び出しを増やす方向は避けるべき
4. 将来的にRwLock除去（thread-local symbol table等）を行えば、native-function/string-methodsの低下も解消可能

---

## 実装上の注意

### Thread safety
- `SymbolTable` は `RwLock` or `DashMap` で保護
- `Symbol::intern()` は write lock、`Symbol::as_str()` は read lock
- 実行中のintern頻度はパース時に集中するため、競合は少ない

### crate選択肢
- **自作**: 上記の `RwLock<HashMap> + Vec` で十分シンプル
- **`string-interner`** crate: 成熟した実装、`DefaultSymbol` が `u32` wrapper
- **`lasso`** crate: thread-safe, zero-allocation resolve
- 外部crateを使う場合は依存関係の増加とのトレードオフ

### 互換性
- `Symbol` → `&str` の変換が容易であること（表示、エラーメッセージ、TAP出力）
- 既存のテストは全て文字列ベースの出力を期待しているため、出力は変わらない
- パーサーのエラーメッセージに含まれる識別子名も `Symbol::as_str()` で復元

### Rustの `match` 最適化について
Rust コンパイラは `match &str` に対して長さチェック→先頭バイト比較→完全比較のような最適化を行う場合がある。Symbol化による高速化の度合いは実測が必要（Phase 5）。ただし、`Value` のサイズ縮小やcloneコスト削減は確実に効果がある。

---

## Summary

| Phase | 効果 | PR数 | 対象ファイル数 | 変更箇所数 | 差分行数(概算) | 依存 |
|-------|------|------|---------------|-----------|---------------|------|
| 1     | 基盤のみ | 1 | 1新規 + 1変更 | ~100行新規 | ~100行 | なし |
| 2a    | AST Call/MethodCall | 1 | 15〜20 | 100〜150 | 200〜300行 | Phase 1 |
| 2b    | AST FunctionDef等 | 1 | 15〜20 | 100〜150 | 200〜300行 | 2a |
| 2c    | AST Stmt識別子 | 1 | 20〜25 | 150〜200 | 300〜400行 | 2b |
| 3a    | methods_0arg | 1 | 5〜6 | ~100 | 300〜400行 | Phase 2 |
| 3b    | methods_narg | 1 | 1〜2 | ~80 | 200〜300行 | 3a |
| 3c    | functions | 1 | 1〜2 | ~60 | 150〜200行 | 3a |
| 3d    | methods スローパス | 1 | 2〜3 | ~200 | 500〜700行 | 3a |
| 4a    | Value Instance/Package | 1 | 20〜25 | 100〜150 | 300〜400行 | Phase 2 |
| 4b    | SubData/Routine/Enum | 1 | 15〜20 | 100〜150 | 200〜300行 | 4a |
| 4c    | 関数テーブル | 1 | 5〜8 | 50〜80 | 100〜200行 | 4b |
| 5     | ベンチマーク | 1 | 数本新規 | 小 | ~100行 | Phase 3,4 |

**合計**: 12 PR、差分合計 約**2,500〜3,700行**

Phase 3 と Phase 4 は互いに独立しており並行して進められる。
