# ③ native IO 状態の VM 所有移管（IO handle 共有ハンドル設計）

[PLAN.md](../PLAN.md) 最終ゴール「tree-walking Interpreter 実行パス撤去 → dual-store 削除」の **③** のうち、
台帳 [vm-interpreter-fallback-ledger.md](vm-interpreter-fallback-ledger.md) §1 の **native-method IO**
（`IO::Handle`/`IO::Pipe` の `native_io_*` dispatch）撲滅。設計の親は ③ 設計
[vm-state-ownership.md](vm-state-ownership.md)、手本は ② [vm-registry-ownership.md](vm-registry-ownership.md)。

## 解くべき問題

VM の catch-all メソッド dispatch（`vm_call_method_compiled.rs:424` 他）は、`IO::Handle`/`IO::Pipe` 受け手の
メソッドを `self.interpreter.call_method_with_values` の generic dispatch へ落とす（§1 native-method fork）。
これを VM ネイティブ dispatch に置換するには、VM が **IO handle 状態**に触れる必要がある。その状態は今
`Interpreter` が独占保持する:

- `Interpreter.handles: HashMap<usize, IoHandleState>`（`src/runtime/mod.rs:851`）
- `Interpreter.next_handle_id: usize`（同 852）

`IO::Handle` 値は handle id を保持し、`handles` を引く。`IoHandleState` は `fs::File`/socket/listener/
バッファ等を持つ大型構造（`mod.rs:529`）。IO ロジック本体は ~10k 行（io.rs/native_io.rs/handle.rs/
builtins_io.rs）だが、**状態アクセス（`self.handles`/`next_handle_id`）はわずか ~36 サイト**（7 ファイル）に
局所化されている。

## なぜ ② registry の playbook がそのまま使えるか（調査確定 2026-06-11）

env は「最ホット・単一所有・既に COW」ゆえ Arc<RwLock> 不可（③設計 §核心）。だが **IO handle は env と異なり
② registry と同型**で、共有ハンドル足場が使える:

1. **スレッド間は live 共有でなく snapshot**。`clone_for_thread`（`mod.rs:5283-5314`）が `handles` を
   `try_clone()` で per-thread にディープコピーし、子スレッドで開いた handle は `new_handles` Vec +
   `next_handle_id` 突き合わせ（`builtins_system.rs:133-149`/`1594-1600`）で親へ merge-back する。
   ＝ registry の per-thread snapshot と同じ。よって lock はスレッド間競合のためでなく、**スレッド内の
   VM↔Interpreter peer アクセス**のためだけに要る（ping-pong で非同時）。
2. **IoHandleState 非Clone・blocking IO は問題化しない**。registry は Clone で guard を即 drop（clone-out）
   できたが handle は `fs::File` を持ち Clone 不可・IO はブロッキングし得る。しかし **per-thread snapshot ゆえ
   その thread の lock を同時に握る他者はいない**（単一スレッド実行）。よって guard を blocking IO 跨ぎで
   保持しても deadlock しない。唯一の危険は**同一スレッドの再入再取得**で、これは ② と同じ debug 実行時
   guard で検出する。
3. **既存コードは既に held-across-reentry を避けている**。`handle.rs:159-165` が示す通り、現行コードは
   `self.handles.get_mut(&id)` を他メソッド（`encode_with_encoding` 等）呼び出しの前に drop→後で再 borrow
   する構造（borrow checker が現状強制）。Arc<RwLock> 化で borrow checker の強制は外れるが、コード構造は
   既に規律に沿っており、debug guard が将来の違反を捕捉する。

→ **③ の中で IO handle だけは「共有ハンドル足場（②方式）」が正しい**。env/型/current_package は
フォールバック撲滅後に plain field へ畳む（④/⑤）が、handle は ② と同じく Arc<RwLock> 足場 → ④/⑤ で plain
VM field へ畳む。

## 表現

```rust
// src/runtime/io_handles.rs
pub(crate) struct IoHandleTable {
    pub(crate) map: HashMap<usize, IoHandleState>,
    pub(crate) next_id: usize,
}
// Interpreter.io_handles: Arc<RwLock<IoHandleTable>>
```

- 再入検出 guard は ② の `reentry_check`（lock アドレス keyed thread-local）を **汎用モジュール
  `src/runtime/lock_reentry.rs` へ昇格**し registry/IO 双方で共有（「1 操作 = 1 実装」）。panic メッセージは
  lock 名を引数化。
- `io_handles()`/`io_handles_mut()` アクセサ（②の `registry()`/`registry_mut()` と同型）。
- `clone_for_thread`: `Arc::new(RwLock::new(IoHandleTable { map: <try_clone deep copy>, next_id }))` で
  per-thread snapshot（意味論不変）。
- `new_handles` merge-back（`builtins_system.rs`）: 子の io_handles snapshot から open handle を抜き出し
  親へ merge、`next_id` を突き合わせ（既存ロジックを accessor 経由へ）。

## ★ 実装で判明した crux（2026-06-11・registry と決定的に異なる点）

PR-A 着手時、registry の機械変換（`self.FIELD` → accessor）がそのまま効かないことが判明した:

- **`handle_state_mut(&mut self, h) -> Result<&mut IoHandleState, _>`（`handle.rs:114`）が `&mut IoHandleState`
  を返し、~32 箇所（handle.rs / native_io.rs）から `let state = self.handle_state_mut(h)?; <use state>` で
  使われている。** `Arc<RwLock>` 化すると `&mut IoHandleState` は guard 内部への参照になり、guard を一緒に返さ
  ないと寿命が切れる（guard+参照は自己参照構造で安全には書けない）。registry は read を `.cloned()` で即 drop
  できたが、IO は **`&mut` 返しアクセサが IO ロジックに織り込まれている**ため不可。
- さらに `write_to_handle_value_trying`（`handle.rs:165-169`）は `state` borrow 中に `self.emit_output()` を
  呼ぶ（現在は per-path NLL で成立）。guard 化すると guard 跨ぎ呼び出し＝再入 deadlock になる箇所がある。

→ **`Arc<RwLock>` lift は「`handle_state_mut` の `&mut`-返し API を closure 形（`with_handle_mut(id, |state| …)`）
または guard 返しへ再設計し、~32 呼び出し側を再入監査込みで書き換える」大規模リファクタ**。registry 方式の
軽い抽出ではない。よってスライスを分割する（下記）。

## スライス計画（strangler-fig・各 PR 挙動不変・CI が安全網）

- **PR-A = `handle_state_mut` の closure 形 API への再設計（lock 前の地ならし）**: `&mut`-返しを
  `with_handle_mut`/`with_handle`（closure が `&mut IoHandleState` を受ける）へ置換し ~32 呼び出し側を変換。
  **この段階はまだ plain field のまま**（borrow checker が再入を引き続き強制）。closure 化で guard 跨ぎ再入が
  構文上不可能になり、PR-B の lock 化が安全になる。各サイトで「closure 本体が他の IO 呼び出しに再入しないか」を
  監査（再入するものは guard drop 相当の構造へ）。挙動不変。
- **PR-B = handles を共有ハンドルへ持ち上げ**: `reentry_check` を `lock_reentry.rs` へ汎用化、`IoHandleTable`
  新設、`Interpreter.handles`/`next_handle_id` → `io_handles: Arc<RwLock<IoHandleTable>>`、`with_handle_mut`/
  accessor を guard 経由へ、`clone_for_thread`/`new_handles` merge/`flush_all_handles` 追従。挙動不変。
- **PR-C = VM へ io_handles ハンドル移管**（② #2895 相当）。
- **PR-D+ = IO メソッド dispatch の VM ネイティブ化**: catch-all の `IO::Handle`/`IO::Pipe` 受け手を
  `try_native_io_method`（VM）へ。IO ロジック本体は共有ハンドル経由で再利用し generic dispatch fork を消す。
- **最終 fold（④/⑤）**: tree-walk 実行パスが消えたら io_handles を plain VM field へ畳む。

## 規律（② と同一・最重要）

RwLock ガードを**同一スレッドで再入再取得しない**（read-while-write / write-while-any は debug panic）。
IO 操作中に別の handle メソッドへ再入する経路が将来生じたら、guard を drop してから呼ぶ。最終防衛線は
make roast のタイムアウト（静的には捕捉できない＝必ずスモークテスト + roast で実行確認）。

## スコープ / 非ゴール

- スコープ = native IO 状態を ② 方式の共有ハンドルへ持ち上げ、IO メソッド dispatch を VM ネイティブ化して
  §1 native-method IO fork を消す。各 PR 挙動不変。
- 非ゴール: env/型/current_package の plain field 畳み込み（④/⑤）、IO ロジック本体の書き換え（再利用する）、
  `Proc::Async`/socket の並行モデル刷新（PLAN §8.3）、スレッド間 handle の真共有（snapshot 維持）。

## 進捗

- **設計確定（本書, 2026-06-11）**: ③native IO は env と異なり ② registry 方式（Arc<RwLock> 共有ハンドル）が
  適用可能と確定（per-thread snapshot ゆえ非Clone/blocking IO が問題化しない）。状態アクセスは ~36 サイトに
  局所化。
- **crux 発見（着手時, 2026-06-11）**: `handle_state_mut` の `&mut IoHandleState` 返し API（~32 呼び出し側）が
  guard 寿命と両立せず、registry の軽い抽出にならないと判明。スライスを「PR-A=closure 形 API 再設計（plain
  field のまま地ならし）→ PR-B=lock 化 → PR-C=VM ハンドル → PR-D=VM ネイティブ dispatch」に分割。
  次の着手 = PR-A（`with_handle_mut` 化）。Arc<RwLock> 試行コードは revert 済み（本 doc に知見を残す）。
