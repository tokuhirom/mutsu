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
- **PR-A 完了（2026-06-11）**: `OutputSink` 構造（`src/runtime/output_sink.rs`）に 7 フィールド
  （output/stderr_output/output_emitted/immediate_stdout/is_thread_clone/shared_thread_output/shared_thread_stderr）を集約、
  `Interpreter` の散在フィールドを 1 つの `output_sink: OutputSink`（**まだ plain field**）へ置換。`emit_output` の書き込み
  決定を `OutputSink::emit(text, subtest_active)` へ移し、Interpreter ラッパが `tap.subtest_depth()!=0` を計算して委譲
  （Stdout handle の `bytes_written` 加算は io_handles 依存ゆえラッパ側に残す）。`clone_for_thread` は thread 用 OutputSink
  を構築（shared buffer を親から Arc clone）。~85 アクセスサイト（self/thread_interp 等）を `.output_sink.FIELD` 経由へ
  （`TapState` 抽出と同型）。挙動完全不変（stdout/stderr/thread/subtest/EVAL を raku と一致確認、`make test` cargo 461 緑）。
  次 = **PR-B**（`output_sink` を `Arc<RwLock<OutputSink>>` へ持ち上げ、io_handles PR-B 同型、`lock_reentry.rs` 再利用）。
- **PR-B 完了（2026-06-11）**: `output_sink: OutputSink` → `Arc<RwLock<OutputSink>>`（io_handles PR-B 同型）。
  `OutputSinkReadGuard`/`OutputSinkWriteGuard` 型エイリアス（`lock_reentry.rs` 汎用 guard 再利用）+ `output_sink()`/
  `output_sink_mut()` アクセサ。~85 サイトを guard 経由へ（read/write は**コンパイラが強制**＝write 誤りは E0596/E0594 検出）。
  `output()` は guard 越しに `&str` を返せず `String`(clone) 返しへ。`clone_for_thread` は thread の OutputSink を
  `Arc<RwLock>` で構築。**crux: Rust 2021 の if-let temporary 寿命** — `if let ... = self.output_sink()... { ...
  self.output_sink_mut()/emit_output ... }` は read guard が body 跨ぎで生き、write-while-read の**実行時 reentry panic**
  （コンパイルは通る＝accessor が `&self`）。thread/supply の drain 5 箇所を「Arc を `let` で clone-out → guard を `;` で
  drop → body」へ再構成。挙動不変（threads/supply/subtest/warn/EVAL を reentry panic ゼロ確認、cargo 461 緑）。
  次 = **PR-C**（VM へ output_sink ハンドル移管 + VM ネイティブ Stdout/Stderr dispatch）。
- **PR-C 完了（2026-06-11）**: VM へ output_sink ハンドル移管 + **VM ネイティブ Stdout/Stderr 出力 dispatch**。
  これで native IO Tier-2 で残った「Stdout/Stderr fall through」が消え、`$fh.say/print/put/printf/print-nl` の**全ターゲット
  （File/Stdout/Stderr）が VM ネイティブ**に。
  - `Interpreter::output_sink_handle()`（Arc clone）+ VM `output_sink` フィールド/`output_sink_mut()` アクセサ（io_handles PR-C 同型）。
    `Interpreter::subtest_active()`（`tap.subtest_depth()!=0`、TAP は interpreter 所有のまま bool で VM へ）。
  - `OutputSink::emit_stderr(text, subtest_active)` 新設（Stderr 分岐を 1 impl 化、interpreter の Stderr 分岐が委譲）。
    `IoHandleState` に `is_stdout_target`/`is_stderr_target`/`add_bytes_written` + `native_text_write` を
    `prepare_text_payload`(closed+nl_out+bytes 計上、target 非依存) と write に分割。`native_print_nl` は撤去
    （print-nl = content="" + newline=true で統一、trying="write")。
  - VM `vm_emit_stdout`（Stdout handle の bytes_written scan-bump ＝ emit_output 同型 + sink.emit）/`vm_emit_stderr`
    （sink.emit_stderr）。`try_native_io_handle_output` を File/Stdout/Stderr の 3 分岐へ拡張（target を payload 構築前に
    判定、Socket/非UTF8 File/Stdin は fall-through）。Stdout は **bytes_written 二重加算**（prepare の receiver + emit の scan）
    ＝**main と同一の既存挙動**（raku は 1 回＝既存差・スコープ外）。subtest 中も sink.emit が subtest_active で正しく buffer。
  - **検証**: 新 `t/io-handle-stdout-stderr-native.t`(6, is_run で subprocess の実 stdout/stderr 捕捉) mutsu PASS・期待値 raku 一致。
    `$*OUT/$*ERR` 出力が **method-fallback 0%**。subtest indent / tell 二重加算は **main と同一**（pre-PR-C で確認）＝既存・不変。
    `make test` 緑。次 = **PR-D+**（読み系 get/lines/read/slurp + ArgFiles `@*ARGS`/非UTF8 decode の VM 到達可能化）。
- **PR-D（読み系・第1スライス `get`）完了（2026-06-11）**: File+UTF8 ターゲットの `.get`（1 行読み）を VM ネイティブ化。
  **調査確定: 読み系も書き込み先で割れる** — `read_line_from_handle_value` の **File+UTF8/bin 分岐は純粋 handle state**
  （`read_record_with_separators(file, seps, chomp)`、UTF-8 lossy、`self` 再入なし）。**ArgFiles**（`@*ARGS` env 依存）/
  **Stdin**（`std::io::stdin`）/**非UTF8 File**（`decode_with_encoding` 再入）/**Socket** は fall-through。
  - `read_record_bytes`/`read_record_with_separators`（純粋・static）を `pub(crate)` 化（record 読みの 1 impl）。
    `IoHandleState::read_line_native()`（closed 検査 + seps/chomp + `read_record_with_separators`）追加。
  - VM `try_native_io_handle_read`（早期ゲート、byte_output の次、mut/非mut 両パス）: `get`・引数なし・**File+UTF8
    （`can_native_text_write`）** のみ native、それ以外 fall-through。結果は interpreter の `get` と同じく `Str`/`Nil` 整形。
  - **検証**: 新 `t/io-handle-get-native.t`(10) mutsu/raku 双方 10/10（chomp/EOF→Nil/:!chomp/custom nl-in/latin1 fall-through/
    closed dies）。`MUTSU_VM_STATS` で File+UTF8 get が native（latin1 get は get=1 で fall-through）。io-handle.t not-ok 数
    main と同一(24=既存)。`make test` 緑。
  - **次 = PR-D2+**: 同パターンで `lines`(lazy Seq)/`words`/`slurp`(read all→String/Buf)/`read`(N bytes→Buf)/`getc`/`readchars`
    を File+UTF8 で native 化。**ArgFiles/Stdin/非UTF8 decode の真の撲滅は `@*ARGS`(env)/`decode_with_encoding` の VM 到達
    可能化が前提**（env 移管＝別campaign、tracks A/B/C と絡む）。
- **PR-D2（読み系・bulk read `slurp`/`read`）完了（2026-06-11）**: File+UTF8 の `.slurp`(→Str) と `.read`(→Buf) を native 化。
  - `IoHandleState` に `can_native_slurp_string(has_bin_arg)`（File+UTF8・非:bin・非bin-mode のゲート）/`slurp_string_native`
    （`read_to_end`+UTF-8 lossy）/`read_bytes_native(count)`（count>0=1回 read up to count／count=0=全read、encoding 非依存）追加。
    `make_buf`（Buf 構築）を `pub(crate)` 化。
  - VM `try_native_io_handle_read` を get/slurp/read の 3 メソッドへ拡張。**slurp** は :bin/非UTF8/bin-mode を fall-through
    （Buf/decode は interpreter）、**read** は File のみ（raw bytes＝encoding 非依存）で `Interpreter::make_buf` へ。junction fall-through。
  - **検証**: 新 `t/io-handle-slurp-read-native.t`(10) mutsu/raku 双方 10/10（slurp 全体/部分読み後/EOF空、read N→Buf/EOF空、
    :bin→Buf fall-through、latin1 decode fall-through、closed dies）。`MUTSU_VM_STATS` で String slurp/read が native（:bin slurp は
    slurp=1 で fall-through）。io-handle.t not-ok=24（既存）。`make test` 緑。
  - **lines/words は lazy（`LazyIoLines`）＝メソッド dispatch を native 化しても実 read は iterator 消費時で、value 組立のみ
    ＝効果薄**。`getc`/`readchars`(char 読み) は次スライス候補。残る ArgFiles/Stdin/非UTF8 は env/decode 移管が前提（不変）。
