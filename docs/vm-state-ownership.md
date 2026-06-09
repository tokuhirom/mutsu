# ③ 実行状態の VM 所有移管（interpreter ブリッジ撤去・構造設計）

[PLAN.md](../PLAN.md) 最終ゴール「tree-walking Interpreter 実行パス撤去 → dual-store 削除」の
**ステップ③**。①（個別フォールバック撲滅）→ ②（宣言レジストリの VM 所有化, #2760-2775, 完了）に続く
**最大の山**。本書は③の確定設計。前提: [vm-registry-ownership.md](vm-registry-ownership.md)（②）、
一次台帳 [vm-interpreter-fallback-ledger.md](vm-interpreter-fallback-ledger.md)、
[vm-dual-store.md](vm-dual-store.md)（locals↔env、レバー B）。

## ③が解くべき状態

`VM` は `interpreter: Interpreter` を**値で所有**（`src/vm.rs:88`）。VM ネイティブコードが借用している
Interpreter 状態のうち、②で移した宣言レジストリ（`Arc<RwLock<Registry>>`）以外の**実行状態**:

- **env**（変数ストア本体, `Interpreter.env: Env`）— 圧倒的最ホット。`self.interpreter.env`/`env_mut` =
  VM ツリーだけで **483 サイト**。
- **型検査**: `type_matches_value`（VM 8 + runtime 26 ファイル）, `var_type_constraint`/`var_type_constraints`,
  `var_hash_key_constraints`。
- **readonly 追跡**: `readonly_vars`/`mark_readonly`/`unmark_readonly`（VM 4 + runtime 2）。
- **let/temp 復元**: `let_saves`/`restore_let_saves`/`discard_let_saves`（VM 5 + runtime 2）。
- **state 変数**: `state_vars`/`our_vars`/`once_values`（VM 0 + runtime 3 — ほぼ tree-walk 側）。
- **`current_package`**（VM 8 + runtime 33）。
- **multi 解決**: `resolve_function_with_types`/`has_multi_*`/`has_proto`。

これらが VM へ移れば **interpreter ブリッジ自体が不要**になり、④（carrier 確定）→ ⑤（dual-store 機構削除）
へ進める。

## 核心: ③は②と方式が決定的に異なる（共有ハンドル方式は採れない）

②は registry を `Arc<RwLock<Registry>>` 足場へ持ち上げた。③で同じことを env にしてはならない:

1. **env には「真の同時共有」が無い。** env は**ただ1つの `Interpreter` オブジェクトにだけ存在**し、
   ping-pong（`Interpreter::run_block_raw` → `VM::new(self)` → `vm.run()` → `*self = interp`,
   `src/runtime/run.rs:956`）で **VM ネスト間を値で往復**する。VM ネイティブ → `interpreter.call_function`
   → 内部で新たな `VM::new` … という再帰も、その都度 Interpreter を**move**するだけで、生きた env は
   常に1個。スレッド跨ぎは `clone_for_thread` の**スナップショット**（書き戻し無し）。
   → ②で Arc<RwLock> が要ったのは「`Interpreter` が `spawn_user_thread` でスレッド境界を越え、registry を
   複数スレッドが触る」から。**env は単一所有者なので、エンドステートは plain な VM フィールド一択**で、
   足場の Arc/lock は不要。
2. **env を Arc<RwLock> 化すると perf が破綻する。** env は全変数 read/write の最ホットパス
   （483 サイト + runtime 深部）。②の registry でさえ遷移期 RwLock 取得で release microbench +2-4%。
   env に lock を1アクセスごとに乗せれば桁違いの退行になる（`Env` は既に内部 `Arc<HashMap>` COW なので、
   さらに外側 lock を被せる意味も無い）。

→ **③は「フィールドを共有ハンドルへ持ち上げる」方式（②の playbook）では進められない。**

## なぜ「フィールド再配置を先に」も採れない — 強制される戦略

では「env を Interpreter から VM の plain field へ今すぐ移す」が直接できるか。**できない**:

- 高価値 state は `src/runtime/` の **tree-walk コードに浸透**している（`type_matches_value` 26 ファイル、
  `current_package` 33 ファイル、env は事実上全 runtime）。これらの tree-walk メソッドは、VM 実行中に
  **§1/§2 フォールバック経由でユーザコードを実行**し、その最中に `self.env`/`self.type_matches_value`/
  `self.current_package` を読む。
- フィールドを Interpreter から物理的に抜くと、これら数千の `self.env` アクセスが全部壊れる。env を引数で
  全 runtime tree-walk 呼び出しグラフに通すのは非現実的。

→ 結論: **③はフィールド再配置駆動ではなく、フォールバック撲滅駆動である。**
台帳 §1/§2 の tree-walk 実行パス（catch-all メソッド dispatch / 関数 dispatch / native-method）を
**VM ネイティブ実装に置換**し、各撲滅が「その state の tree-walk 読者」を1クラスずつ消す。最後の
tree-walk 実行パスが消えた時点で env/型/state/current_package は runtime/ に読者を持たなくなり、
**plain な VM フィールドへ畳める**（この最終 fold が ④/⑤）。**③のスライス ＝ 台帳 §1/§2 の行**。

## 状態 × 結合度マップ（調査確定 2026-06-08）

| state | VM サイト | runtime/ 結合 | 単独移管可否 | ③での扱い |
|---|---|---|---|---|
| `env`/`env_mut` | 483 | 全域 | 不可（最ホット・全域） | フォールバック撲滅後に最後に畳む |
| `type_matches_value` | 8 | 26 ファイル | 不可（tree-walk 深部） | §1 撲滅で読者が減る |
| `current_package` | 27 | 33 ファイル | 不可（tree-walk 深部） | §1/§2 撲滅で読者が減る |
| `var_type_constraint(s)` | 33 | 8 | 困難 | 型検査と同時 |
| `readonly_vars`/`mark_readonly` | 8 | 3 | **比較的局所** | 早期に VM へ寄せられる候補 |
| `let_saves`/`restore_let_saves` | 10 | 3 | **比較的局所** | 同上（が env 残存中は decoupling 価値小） |
| `state_vars`/`our_vars`/`once_values` | 0 | 3 | runtime 専属 | tree-walk 撲滅に追従 |

要点: **局所的な state（readonly/let）を先に VM へ移しても、env が Interpreter に残る限り
decoupling 価値は小さい**（Interpreter オブジェクトは消えない）。価値が出るのは tree-walk 実行パスが
消えて env が畳めた瞬間。よって投資は §1/§2 撲滅に集中する。

## 台帳 §1/§2 = ③のスライス（撲滅順）

一次状態は [vm-interpreter-fallback-ledger.md](vm-interpreter-fallback-ledger.md)。残る tree-walk:

- **§1 catch-all メソッド dispatch**（最大）: `vm_call_method_compiled.rs:427/857`,
  `vm_call_method_mut_ops.rs:303` の `interpreter.call_method_with_values` =
  生きた Instance/Buf/Failure メソッド fork。**残る主 tree-walk**。
- **§1 native-method**（IO 系）: `native_io_*` がファイルハンドル等の interpreter 所有状態を要求 →
  state（handles）移管前提。
- **§2 関数 dispatch**: `vm_call_func_ops.rs`（builtin-shadow / multi-dispatch / final else）,
  `vm_call_dispatch.rs` catch-all, `vm_dispatch_helpers.rs:356-384` Routine 値の関数解決。
  **②（registry の VM 所有）で構造前提が整った**＝ここが ③ の最初の現実的着手点。

撲滅の必須手順（②/dedup と同じ）: native 実装が authoritative であることを **EVAL 経路で同値確認**
（`mutsu -e 'say EVAL(q{...})'` が raku と一致）してから tree-walk を外し、`make roast` で確認。

## スライス計画（strangler-fig・各 PR 挙動不変・CI が安全網）

> 順序は「②で構造前提が整った §2 関数 dispatch」→「§1 catch-all メソッド dispatch（本丸）」→
> 「native-method（IO state 移管と同時）」→「env/型/state を plain VM フィールドへ畳む（④/⑤）」。

- **PR-1（§2 関数/Routine dispatch の VM ネイティブ化）**: `vm_dispatch_helpers.rs` Routine 値解決と
  `vm_call_func_ops.rs` の multi/sub 解決を、②で VM アクセス可能になった `Registry` メソッド
  （`resolve_*` / `has_multi_*`）を使って VM ネイティブ dispatch へ。builtin 優先順位
  （Routine が builtin を指す場合）を VM 側で再現。interpreter は終端（EVAL/carrier）のみ。
- **PR-2 以降（§1 catch-all メソッド dispatch）**: 生きた Instance メソッド呼びを
  `call_compiled_method_*` 統一 dispatch へ寄せ、ユーザ定義クラスメソッドを全て bytecode 実行に。
  native/reflective/MOP のみが共有末端（carrier）へ。Buf/Failure の native メソッドは builtins/ へ降ろす。
- **PR-n（native-method IO）**: `handles` を VM 所有へ移し `native_io_*` を VM ネイティブに。
- **最終 fold（④/⑤）**: tree-walk 実行パスが消えたら env/型検査/readonly/let/state/current_package を
  `Interpreter` から `VM` の plain フィールドへ移し、`Interpreter` オブジェクトと ping-pong を撤去、
  `env_dirty`/`saved_env_dirty`/`ensure_locals_synced`/`sync_locals_from_env` を削除。

## スコープ / 非ゴール

- スコープ = **tree-walk 実行パスの VM ネイティブ化**（台帳 §1/§2 の行を消す）。各 PR は挙動不変。
- 非ゴール（後段）: env/state の plain フィールド畳み込み（④/⑤、Interpreter 撤去後）、§C carrier の
  最終確定（EVAL サブ VM / 正規表現埋め込み `{}` / pseudo-package）、`type_metadata` の Arc-ptr keying 解消
  （🟣第一級コンテナ）、スレッド間 registry の真共有（PLAN §8.3 concurrency）。

## 進捗

- **設計確定（本書, 2026-06-08）**: ③は②と異なり**フォールバック撲滅駆動**（共有ハンドル方式不可・
  env は plain field 終状態）と確定。状態×結合度マップを作成。スライス計画 = 台帳 §1/§2 の撲滅。
  次の着手 = PR-1（②解禁の §2 関数/Routine dispatch の VM ネイティブ化）。
- **PR-1 完了（Routine dispatch, 2026-06-08）**: `vm_dispatch_helpers.rs::vm_call_on_value` の Routine 値解決の
  3 つの生 `interpreter.call_function`（qualified / bare / 末端）を統一エントリ `call_function_compiled_first`
  へ寄せ、ユーザ定義 sub/multi/proto を compiled bytecode 実行に。**builtin 優先保全**: builtin 名を持つ
  Routine（`&SETTING::...::not` → `Routine{GLOBAL, "not"}`）のみ `is_builtin_function` ガードで `call_function`
  の builtin 優先を維持（平 user `&not` は `Value::Sub` で Routine 枝に来ない）。naive 変換で
  `S02-names/SETTING-6e.t` 回帰 → 実証して修正。pin = `t/routine-value-dispatch.t`(10)。台帳 §2 の Routine 行を消化。
- **PR-2 完了（builtin-shadow fork, 2026-06-08）**: ユーザ sub が同名 builtin を shadow する関数 dispatch fork
  （`vm_call_func_ops` の `exec_call_func_op` + `exec_call_func_slip_op`、`user_function_matches_call` 枝）の
  うち **plain 単一候補かつ compilable** な def を `compile_and_call_function_def` で OTF compile し bytecode 実行に
  （native arm へ落とさず shadow された builtin を回避）。compilable 判定を `def_is_otf_compilable` ヘルパへ集約。
  **回帰を 2 段で発見・修正**: ① proto/multi も単一候補 OTF compile し proto'd multi 候補 dispatch が壊れた
  （S06-multi/proto・subsignature・type-based が mid-file abort）→ `!has_proto && !has_multi_candidates` ガード。
  ② `user_function_matches_call` 枝は builtin-shadow 専用ではなく「compiled_fns に無い args 一致ユーザ sub」全般が来る。
  `def_is_otf_compilable` が nested `sub`+`when` 制御フローを捕捉できず Test::Util `is-deeply-junction` が壊れた →
  **`is_builtin_function` ガードで実際の builtin shadow に限定**（教訓: フォールバック枝の*真の到達集合*を仮定せず実測せよ）。
  pin `t/builtin-shadow-dispatch.t`(9)。**次は §1 catch-all メソッド dispatch（本丸）または §2 非proto multi fork。**
- **PR-3 完了（§1 catch-all メソッド dispatch — 実態訂正 + compile-on-demand, 2026-06-09）**: §1 catch-all を
  「本丸＝残る主 tree-walk（ユーザーメソッド）」として着手したが、**`MUTSU_VM_STATS` + プローブで到達集合を実測した
  結果、その前提が誤りと判明**（PR-2 の教訓「真の到達集合を仮定せず実測せよ」がここでも効いた）。実態:
  通常宣言メソッド（multi 含む）・submethod・role 合成・継承・`.^add_method` は**全て登録時に
  `compile_class_methods` で bytecode 化済**（`.^add_method` は Sub リテラルの `compiled_code` を継承）。catch-all
  到達の Instance トラフィックは native coercion（`Exception.Stringy`）/ MOP（`.does`/`.^does`）/ role-qualified
  （resolve=None）のみで、これらは ③ state 所有 / builtins 降ろし / 別の解決バグ依存。**残る唯一の compile-gap は
  `.^add_multi_method`**（`compiled_code: None` 固定）。対応として dispatch チョークポイントに `populate_uncompiled_method`
  （resolve 済み def が `compiled_code==None` のとき `compile_class_methods`/`compile_role_methods` で冪等 compile→
  再解決→`dispatch_compiled_method`）を追加し、ユーザーメソッド分を確実に bytecode 化（owner が非 user class or 空
  ボディなら interpreter フォールバック温存）。pin `t/method-otf-dispatch.t`(14)。**結論: §1 catch-all の残りは native/
  MOP dispatch であり、その撲滅は state 所有移管（env/handles を VM へ）と builtins への native メソッド降ろしが本体**
  （単独の routing では消えない）。env 畳み込みの価値はこの native dispatch を VM ネイティブ化した後に出る。
  **次は §2 非proto multi fork（VM 側 multi 候補解決）、または native メソッドの builtins 降ろし（Buf/Failure）。**
- **PR-4 完了（§2 非proto multi fork, 2026-06-09）**: `vm_call_func_ops.rs::dispatch_func_call_inner` の
  非proto multi fork（`has_multi_candidates && !has_proto`）を PR-2 型へ拡張: ②で VM アクセス可能になった
  `resolve_function_with_types` で winner を解決し、`def_is_otf_compilable` かつ非state body なら
  `compile_and_call_function_def` で OTF compile/bytecode 実行。**関数パスの ambiguity は `None`+`pending_dispatch_error`
  で表現**（`Some(def)`=unambiguous）＝メソッド側 `dispatch_ambiguous` とは別機構。ambiguity/where/default/code-param/
  no-match/proto/末端は `call_function_fallback` 維持（正規例外を投げる）。**nextsame/callsame/callwith は compiled 候補から
  でも機能**（`compile_and_call_function_def` が同じ `push_multi_dispatch_frame` を張るため＝実測で redispatch 除外不要を確認）。
  落とし穴: ① otf_call_cache の name 汚染（type-blind cache が型違い候補を誤再利用）→ insert/lookup 双方に
  `!has_multi_candidates_cached` ガード。② signature alternates の共有 state（`(A)|(B){state $x}` の state_group 共有が
  per-alternate fingerprint で割れる）→ `function_body_declares_state` で state 持ち multi body を OTF 除外
  （`t/multi-signature-alternates.t` 回帰を発見・修正）。slip 経路 multi は既存の別バグ（`|ms()`=Nil）で対象外。
  pin `t/multi-otf-dispatch.t`(25)。multi-probe fallback 5→2、S06/S12/S14 全緑、make test PASS。
  **副次的に既存 flaky を修正**: `push_multi_dispatch_frame` が callsame の current 候補を HashMap 順 first match で
  決めていた（最狭が後宣言だと ~50% シード依存 flake、`callsame.t` 等 whitelist の間欠 fail = CI #2788 失敗の実体）。
  interpreter inline frame と同じ決定的 winner（`resolve_function_with_alias`）で特定するよう修正＝全 VM 多重経路を決定化。
  redispatch 除外は不要に（callsame は OTF 候補でも正しく動く）。
  **次は §2 catch-all 末端（`call_function` final else＝③ state 所有移管本体）、または native メソッドの builtins 降ろし。**
