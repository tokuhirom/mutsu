# ③後段/④ output-sink (emit_output) の VM 所有移管

[PLAN.md](../PLAN.md) 最終ゴール「tree-walking Interpreter 実行パス撤去 → dual-store 削除」の **③後段/④**。
[native IO 所有移管](vm-io-ownership.md)の **PR-D Tier-2** で判明した「**File 出力は純粋 handle state で native 化できたが、
Stdout/Stderr 出力は `emit_output` 依存で VM 到達不能**」を解消する。手本は ② registry / ③ native IO（io_handles）の
共有ハンドル playbook（[vm-io-ownership.md](vm-io-ownership.md) / [vm-registry-ownership.md](vm-registry-ownership.md)）。

## 解くべき問題

`IO::Handle.say/print/put/printf/write/spurt` の **Stdout/Stderr 受け手分岐**は `emit_output`（Stdout）/
`stderr_output` バッファ（Stderr）へ書く。これは `Interpreter` が独占保持する出力シンク状態に依存し、VM から到達できない
（native IO Tier-2 で File のみ native 化、Stdout/Stderr は fall through のまま残った）。VM ネイティブ化には VM が
**出力シンク状態**に触れる必要がある。

## 出力シンク状態のマップ（調査確定 2026-06-11）

`emit_output`（`mod.rs:4229`）が依存する `Interpreter` 状態:

| フィールド | 役割 | per-thread? |
|-----------|------|------------|
| `output: String` (`mod.rs:828`) | 非 immediate 時の出力アキュムレータ。`run()` が `self.output.clone()` を返す（`run.rs:865`）。`take_output`/`output()`/`clear_output` で消費 | ○ thread clone が `mem::take` して promise へ（`builtins_system.rs:155`） |
| `stderr_output: String` (`829`) | stderr バッファ（同上） | ○ 同上 |
| `output_emitted: bool` (`846`) | 何か出力されたかのフラグ。`has_output_emitted()` | ○ |
| `immediate_stdout: bool` (`843`) | **真の stdout へ即書きするか**。**CLI 実行は true**（`main.rs:466`）→ emit_output は `std::io::stdout().write_all` 直書き。embedded/EVAL/test は false → buffer | thread clone は true (`socket_thread.rs:386`) |
| `is_thread_clone` + `shared_thread_output: Option<Arc<Mutex<String>>>` (`1131`/`1135`) | thread clone がリアル時系列で出力を交織するための共有バッファ | clone 固有 |
| `tap.subtest_depth()` (`tap_state.rs:130`) | **subtest 中は即書きを抑止**（`subtest_depth()==0 && immediate_stdout` で初めて真の stdout へ） | tap も Interpreter 所有 |

`emit_output` 本体の決定木:
```
output_emitted = true; (Stdout handle の bytes_written 加算)
if tap.subtest_depth()==0 && immediate_stdout: 真の stdout へ write_all+flush
elif is_thread_clone && shared_thread_output: 共有バッファへ push
else: self.output へ push
```

`emit_output` は **37 箇所**から呼ばれる（subtest/test_functions/io/supply/handle/builtins_system/vm_hyper_race_parallel/
main_args …）。`output` 読み書きは **~33 箇所**。

## ★ io_handles と決定的に違う点（戦略上の crux）

1. **出力シンクは「単一所有・末尾消費」**。io_handles は per-thread snapshot + merge-back だったが、`output` は
   `run()` が最後に `clone()` して返すプログラム出力そのもの。thread clone は `mem::take` で promise に渡し、await で
   連結。＝per-thread だが「merge-back」でなく「promise が収集」。
2. **依存が連鎖する**: emit_output の**書き込み決定**は `immediate_stdout`（単純 bool）だけでなく **`tap.subtest_depth()`**
   （TAP 状態機械）にも依存。VM が Stdout 出力を native 化するには TAP subtest 深度も VM 到達可能でなければならない
   → **tap の移管/参照も巻き込む**。これが「本丸が重い」理由。
3. **CLI 実行は immediate**（buffer を経由せず真の stdout 直書き）。よって「buffer を共有化」だけでは CLI path の
   native 化にならない — **書き込み決定（immediate + subtest + thread）ごと** OutputSink へ括り出す必要がある。

## 戦略: OutputSink 抽象 + 共有ハンドル（io_handles playbook）

出力シンクの**状態 + 書き込み決定**を 1 つの `OutputSink` 構造へ括り出し、`Arc<RwLock<OutputSink>>` として VM↔Interpreter
で共有する（io_handles と同型・`lock_reentry.rs` の汎用 guard を再利用）。

```rust
// src/runtime/output_sink.rs
pub(crate) struct OutputSink {
    pub(crate) output: String,
    pub(crate) stderr_output: String,
    pub(crate) output_emitted: bool,
    pub(crate) immediate_stdout: bool,
    pub(crate) is_thread_clone: bool,
    pub(crate) shared_thread_output: Option<Arc<Mutex<String>>>,
}
impl OutputSink {
    // subtest_active は呼び出し側（tap を持つ Interpreter / 将来の VM）が渡す。
    // TAP 移管前は「subtest 中かどうか」だけを bool で受ける（tap 全体は移さない）。
    pub(crate) fn emit(&mut self, text: &str, subtest_active: bool) { ... }
    pub(crate) fn emit_stderr(&mut self, text: &str, subtest_active: bool) { ... }
}
// Interpreter.output_sink: Arc<RwLock<OutputSink>>
```

TAP 依存の扱い: **subtest 中かどうかを bool で OutputSink::emit に渡す**（TAP 状態機械全体は移さない）。Interpreter は
`self.tap.subtest_depth()==0` を計算して渡す。VM ネイティブ Stdout dispatch 時は VM も subtest 深度を知る必要があるが、
これは別途 tap 参照を VM へ渡す小スライス（または「VM が Stdout 出力する時点で subtest 中なら fall through」で回避）で扱う。

## スライス計画（strangler-fig・各 PR 挙動不変・CI が安全網）

- **PR-A = `OutputSink` 構造抽出（plain field のまま）**: `output`/`stderr_output`/`output_emitted`/`immediate_stdout`/
  `is_thread_clone`/`shared_thread_output` を `Interpreter.output_sink: OutputSink`（**まだ `Arc<RwLock>` でない plain
  field**）へ集約。`emit_output`/`emit_stderr` を `OutputSink::emit(text, subtest_active)` へ移し、Interpreter ラッパが
  `self.tap.subtest_depth()==0` を計算して委譲（1 操作1実装）。37 callers と ~33 アクセスはアクセサ経由へ。挙動不変。
- **PR-B = `output_sink` を `Arc<RwLock<OutputSink>>` へ持ち上げ**: io_handles PR-B と同型。`lock_reentry.rs` 汎用 guard 再利用、
  `output_sink()`/`output_sink_mut()` アクセサ、`clone_for_thread` で per-thread の fresh sink（thread は mem::take でなく
  sink ごと差し替え or snapshot）、promise 収集を sink 経由へ。挙動不変。
- **PR-C = VM へ output_sink ハンドル移管 + 最初の VM ネイティブ Stdout/Stderr dispatch**: io_handles PR-C 同型。
  `$fh.say/print/put` の **Stdout/Stderr 受け手**を VM の `output_sink` で native 化。payload は既存の render_* 利用
  （Tier-2a と同じ）。subtest 中は fall through（または VM へ subtest 深度を渡す小工夫）。これで native IO Tier-2 の
  「Stdout/Stderr fall through」が消える。
- **PR-D+ = 読み系（get/lines/read/slurp + ArgFiles `@*ARGS` env / 非UTF8 decode）** の VM 到達可能化。これは別の状態
  （env の `@*ARGS`、encode/decode）依存ゆえ別スライス。
- **最終 fold（④/⑤）**: tree-walk 実行パスが消えたら output_sink / io_handles を plain VM field へ畳む。

## 規律（io_handles と同一）

RwLock ガードを**同一スレッドで再入再取得しない**（debug guard で検出）。emit_output は 37 箇所から呼ばれ、一部は
render_*（メソッド dispatch 再入）の後/前に呼ばれるので、**ガードを跨いで render_* を呼ばない**（payload は guard 外で構築）。
最終防衛線は make roast。

## スコープ / 非ゴール

- スコープ = 出力シンク状態を OutputSink へ括り出し共有ハンドル化し、Stdout/Stderr 出力 dispatch を VM ネイティブ化して
  native IO Tier-2 の Stdout/Stderr fall through を消す。各 PR 挙動不変。
- 非ゴール: TAP 状態機械全体の移管（subtest_active を bool で渡すに留める）、読み系 decode/ArgFiles の移管（PR-D+）、
  output_sink の plain VM field 化（④/⑤）。

## 進捗

- **設計確定（本書, 2026-06-11）**: 出力シンク状態をマップ（output/stderr_output/output_emitted/immediate_stdout/
  thread-clone/tap subtest_depth、emit_output 37 callers）。io_handles playbook（OutputSink 抽出 → Arc<RwLock> 化 →
  VM ハンドル → native dispatch）を採用。crux = TAP subtest 依存の連鎖（subtest_active を bool で渡して回避）。
