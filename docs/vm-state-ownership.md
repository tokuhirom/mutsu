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
| `current_package` | 27 | 33 ファイル | **共有ハンドルで移管済**（下記） | registry dispatch read の VM 化を解禁 |
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
- **PR-5 完了（§1 catch-all = plain `@`-array mutators, 2026-06-09）**: §1 catch-all の**実態を計測訂正**。catch-all 受け手の
  型名を一時計装して whitelist sample 全体で到達集合を実測した結果、PR-3 の「残りは native/Buf/Failure」推定が誤りと判明＝
  実際の支配トラフィックは **配列ミューテータ（`Array.append`=16721, `Array.shift`=5796, `Array.splice`）+ iterator protocol
  （`pull-one`/`skip-one`/`push-exactly`）+ coercion（`List.Set/Bag/Mix`）**。最大の `Package.new`=38698 は③コンストラクタ
  （user BUILD/属性デフォルト実行）で③ブロック。`Package.new` を除く**最大 tractable カテゴリ＝配列ミューテータ**を着手。
  `vm_call_method_mut_ops.rs` に `try_native_array_mut` を追加し、**plain untyped `@`-array への append/prepend/unshift/pop/shift**
  を VM ネイティブ化（`env.get_mut`+`Arc::make_mut`＝interpreter primary branch と同一）。typed/shaped/lazy/shared/metadata持ち/
  非env-bound は保守的に interpreter 維持＝behavior-invariant。**type_metadata の Arc-ptr keying aliasing**（既知🟣ハザード）を
  `make_mut` 再確保が顕在化させたため、native path 出力に `unregister_container_type_metadata` を防御適用（untyped 保証下で安全）。
  pin `t/native-array-mut.t`(31)。
- **PR-6 完了（§2 catch-all 末端の実測 + junction constructor 降ろし, 2026-06-09）**: `call_function_compiled_first` 末端
  （`vm_call_dispatch.rs:79`）を `END:` 計装で実測した結果、**末端はほぼ枯渇**と判明（PR-1〜4 ＋ resolve 済み def の全 OTF compile
  により高トラフィックの fallback は消えた）。残る末端到達は diffuse な少量＝junction constructor / **lexical `&`-変数の名前呼び出し**
  （末端残余の最大カテゴリ＝`-> &op { op(…) }`〔S03-operators/set_*.t の `END:op`〕や `my &junction = ::("&any")`〔autothreading.t〕で
  Callable を bareword 呼び・正しく動作・将来 VM 寄せ候補）/ `__mutsu_*` 内部・並行 CAS〔lever B〕/ no-match エラー生成〔carrier〕。
  唯一の pure-builtin カテゴリ `any`/`all`/`one`/`none` を `builtins::build_junction` へ降ろし（interpreter の `builtin_junction` も
  同 fn へ委譲＝重複解消）、`try_native_function` の Instance-guard を junction ctor に限り bypass。pin `t/native-junction-ctor.t`(24)。
  **教訓: §2 末端は計測すると既にほぼ drained。残りは③ state 所有（並行）/ niche / エラー carrier で「高トラフィック撲滅」フェーズは終了。**
  **次は ③ state 所有の本丸（env/型/handles を VM plain field へ畳む④/⑤の前段）、coercion〔`List.Set/Bag/Mix`〕の builtins 降ろし、
  または iterator protocol（`pull-one`/`skip-one`/`push-exactly`）の VM ネイティブ化。**

- **`current_package` 共有ハンドル移管（2026-06-12, registry 撤去 enabler）**: registry の VM ネイティブ dispatch read
  （`has_proto`/`has_multi_candidates`/`resolve_function_with_types`/…）は `current_package` に結合しており
  （`format!("{pkg}::{name}")` の FQ 名構築）、`current_package` が Interpreter の plain `String` field である限り
  VM は `self.interpreter.current_package()` バウンス経由でしか読めず、これら dispatch read を VM 自前の `registry`
  ハンドルで処理できなかった（`vm.rs::registry_mut` のコメントが明記していたブロッカー）。本スライスで `current_package`
  を `registry`/`io_handles`/`output_sink` と同型の **`Arc<RwLock<String>>` 共有ハンドル**へ持ち上げた:
  - field `String` → `Arc<RwLock<String>>`。アクセスは `current_package()`（read-clone→owned `String`）/
    `set_current_package()`（write）のみ。ガードはアクセサ外へ出ないので**再入を跨いで lock を保持しない**
    （registry アクセサと同じ規律＝再入安全・デッドロック無し）。型変更でコンパイラが全 ~130 read / ~27 write を強制列挙。
  - `clone_for_thread` と埋め込み regex/grammar サブインタプリタ構築は**スナップショット**（fresh lock。thread-local
    registry 意味論＝子はコピー、書き戻し無し）で Arc を共有しない。
  - VM に自前ハンドル clone（`current_package_handle()`）+ VM 側 `current_package()`/`set_current_package()` を追加。
    VM の ~55 サイトを `self.interpreter` バウンスから自前ハンドル経由へ。VM は Interpreter を値所有するため
    両 peer は ping-pong 中も同一 lock 越しに同じ値を観測＝挙動不変。save/restore 意味論も保存。
  - 検証: cargo test 461/0、clippy/fmt 緑、package/`our sub`/継承/multi/`module our $x`/grammar token/`start{}` threading
    スモーク全一致。（`t/native-array-mut.t` subtest 26 は PR-5 既知の type-meta Arc-ptr keying aliasing flaky で本変更と無関係。）
  - **registry 撤去の payoff（2026-06-12, #2929/#2933）**: `Registry::has_proto`/`has_multi_candidates`/
    `has_declared_function`/`has_multi_function`(current_package, name) を pure `impl Registry` メソッド化し、
    VM dispatch を `self.registry`+`self.current_package()` で native 読みへ寄せた。**pure-predicate dispatch read は枯渇**。

## 重要な設計所見: env は `current_package` の共有ハンドル playbook を**使えない**（2026-06-12）

ユーザー方針「③ env 移管に着手」(2026-06-12) を受けた調査で、**env は registry/current_package/io_handles/output_sink
の `Arc<RwLock>` 共有ハンドル方式が原理的に使えない**ことを確定した（current_package が doc の悲観的予想に反して移管できた
ので env も…という期待は否定される）:

1. **再入跨ぎの in-place 変異**: current_package は read-clone-drop（ガードを即 drop、再入を跨がない）だから lock 化できた。
   env は `env_mut()` で**変異を再入跨ぎで保持**する（opcode 実行中に env を握ったままユーザコードへ再入）。
   `Arc<RwLock<Env>>` で write guard を保持したまま再入＝**自己デッドロック**。clone-per-access は変異が伝播しない。
2. **最ホットパスの perf**: env は全変数 read/write（VM 483 サイト + runtime 全域）。registry でさえ遷移期 RwLock で
   +2-4%。env に lock を1アクセスごとは桁違いの退行（`Env` は既に内部 `Arc<HashMap>` COW＝外側 lock は無意味）。

→ **③ env はハンドル移管駆動ではなく、本書冒頭どおり「フォールバック撲滅駆動」**（§1/§2 tree-walk fallback を native 化 →
最後の reader が消えたら env を plain VM field へ fold ＝④/⑤）が唯一の道、と再確認。

### method-fallback landscape 実測（2026-06-12, `MUTSU_VM_STATS`、method-heavy whitelist sample）

catch-all (`vm_call_method_compiled.rs` の native/Buf/Failure fork) に残る method fallback の支配カテゴリ:
`iterator`=280（`.iterator` 取得・最大）、`new`=99（③ ctor・env/BUILD 依存）、`push-exactly/at-least/all/until-lazy`=~200
（iterator PUSH 協定＝外部バッファ writeback＝第一級コンテナ Phase 2）、`can`=56（MOP carrier）、`raku`=40、`bool-only/count-only`、
`map`/`grep`（lazy/block 形）、`EVAL`（carrier）、`Mix` 等。**env 非依存で tractable な最大カテゴリ＝`.iterator` 構築**。

### スライス: `.iterator` 構築の VM ネイティブ化（本 PR）

`.iterator` の主経路（Range/Set/Bag/Mix/List/Array → `Iterator` instance `{items, index:0, is_lazy?, known_count?}`）は
**純粋な値構築**（env/再入なし）。これを `src/builtins/iterator_construct.rs::build_iterator_instance` の単一実装へ抽出し、
interpreter (`dispatch_iterator_method` の pure tail) と VM (`try_native_iterator_construct`、catch-all 直前) が共有
（**1 操作 = 1 実装**）。`Seq`（consumed-state 追跡 + `squish` の env 変異＝interpreter 所有）と既構築 Iterator instance
（identity 返し）は fall through/分岐。実測 range-iterator.t の `iterator` fallback 720→120（残 120 = Seq 等）。
挙動不変（bag/mix/range/set-iterator・gather・S32-list/iterator roast 緑、S07-hyperrace/basics の既存 3 fail は本変更無関係）。

## 調査記録: dynamic 変数の closure 跨ぎ可視化が「instance mutation writeback」に依存（2026-06-10）

トラック A（PLAN.md 再編 Phase I）の lexical `&`-var dispatch スライスを試みたところ、`code()`/`&code()` で束縛
Callable を VM (`vm_call_on_value`→`call_compiled_closure`) で実行すると、**caller が rebind した dynamic 変数
（`$*ERR` 等）への書き込みが caller に伝播しない**回帰が出た。**これは私の変更前から `&code()` で再現する pre-existing
バグ**（`sub cap(&code){ my $*ERR=FakeIO.new; &code() }` で `code` 内の `note` が rebound `$*ERR` に届かない）。
`main` の `exec_call_func_op`（~185-207）のコメントが既にこの罠を明記し、純 lexical `&`-var を意図的に interpreter
ターミナルに残している。

### 完全な根本原因（計測で確定）

1. `note` → `write_to_named_handle("$*ERR")` → `get_dynamic_handle` → `self.interpreter.env().get("$*ERR")`。
   env 経由で **instance を解決**（同一 instance id を確認＝別インスタンスではない）。
2. `$*ERR.print(...)` は変異メソッド → `overwrite_instance_bindings_by_identity(class, id, updated)`
   （`methods_mut.rs:415`）で「同 id の instance を保持する全 env binding」を更新して writeback する。
3. **その writeback が `self.env.values_mut()` を使う＝ scoped env では overlay tier のみを走査**（`src/env.rs`:
   「insert/remove/get_mut/iter は overlay のみ。parent は immutable `Arc<Env>`」）。closure frame の overlay には
   caller の `$*ERR` binding が無い（それは parent tier）ので、変異が **caller の binding に届かず**、frame 復帰で消える。
4. interpreter (`call_sub_value`) は `new_env = saved_env.clone()`（caller env の **flat clone**）で closure を走らせ、
   exit 時に「caller に既存の変数」へ side-effect を merge（`resolution.rs` ~1317/~1564）。flat なので
   `values_mut()` が caller binding を直接更新でき、merge で伝播する。だから interpreter 経路は正しく動く。

### 結論: これは ③ dual-store + value-identity（Phase 3）の山であり quick fix ではない

「dynamic var を env に置く」だけでは不十分（env は scoped overlay で parent immutable）。本質は
**instance/コンテナを値で保持し、変異の writeback が現フレーム overlay にしか効かない**こと。`$*ERR` に限らず、
closure 内の任意の「caller 変数が保持する instance/配列/ハッシュへの変異メソッド」が同じ構造で壊れうる
（map/for は **同一フレーム実行**なので overlay=caller で偶然成立しているだけ）。

### 修正オプション（次セッションの設計対象）

- **(A) writeback を env チェーン全走査に**: `overwrite_*_bindings_by_identity` を overlay だけでなく parent tier も
  更新。だが parent は `Arc<Env>` 共有・immutable で、`Arc::make_mut` は O(1) scoped 共有を壊し他 holder にも影響＝
  scoped-env 設計と衝突。要再設計。
- **(B) closure exit-writeback を instance-by-id 伝播に拡張**: 現 exit writeback（`vm_closure_dispatch.rs` ~637-700）は
  宣言済み/捕捉ローカルのみ伝播。これを「overlay で変異された同 id instance を restored caller env へ反映」に拡張＝
  interpreter の merge を模倣。**最も局所的**だが、変異 instance の追跡が要る（現状 untracked）。実測では overlay に
  `$*ERR`（未 capture-skip 時の captured clone）があっても伝播しなかった＝exit writeback の対象集合に入っていない。
- **(C) 第一級 instance/コンテナセル（Phase 3）**: instance を共有セル（`ContainerRef` 類似）で参照化し、変異を
  by-reference に。全フレームで可視＝構造的解決。長期の正攻法だが最大の改修。

### 付随して確認した正しさ事項（修正時に同梱すべき）

- dynamic var（`*` twigil）は Raku では **lexical capture されない**。`call_compiled_closure` の captured-env merge
  （`vm_closure_dispatch.rs` ~230）が captured `$*ERR`（stale clone）を overlay に入れて live parent を shadow する。
  capture-skip（`is_dynamic_var_name` で twigil `*` を除外）は **読み取り側の正しさには必要**だが、writeback 不達
  （上記 3）が残るため **単独では不十分**（capture-skip だけでは依然 caller に伝播しない）。(A)/(B)/(C) と同梱が必要。
- 制御フロー名（`return`/`take`/`emit`/`callsame`/… `call_function` の match が直接処理）は lexical `&`-var dispatch から
  必ず除外（さもないと `&r=&return` 再束縛で無限再帰。`is_builtin_function` に無い名が複数あるので明示リスト要）。

**よってトラック A の lexical `&`-var dispatch は (A)/(B)/(C) のいずれかが入るまで保留**（[[project_lexical_amp_var_blocked]]）。

## Phase II 着手: env fold feasibility 実測（2026-06-14, Phase I クローズ後）

ユーザー方針「Phase II 着手（env fold 可否調査 + 第1スライス）」(2026-06-14) を受けて、env fold が今どの構造に
ブロックされているかを **コード実測 + `MUTSU_VM_STATS` per-name histogram** で確定した。結論は本書冒頭の設計
（③＝フォールバック撲滅駆動・env は plain field 終状態）を**より精密化**する。

### 実測した3つの事実

1. **VM の env 直接アクセス = 481 サイト**（`self.interpreter.env`/`env_mut`）。env が VM field 化すれば機械的
   rename だが、`VM::env()` accessor 化は **borrow-checker 衝突**（accessor は `&self` 全体借用、現状の
   `self.interpreter.env()` は `self.interpreter` 部分借用）を誘発し、env 読みと他 self フィールドが交錯する
   サイトでは純 sed にならない。＝seam 化も非自明。
2. **env fold を物理ブロックするのは残る tree-walk 委譲 ~15 サイト**（`call_sub_value`/`call_function[_fallback]`/
   `call_method_with_values`/`run_block_raw`）。interpreter のこれら tree-walk メソッドが param-binding で
   `self.env` を直接操作してから内側 VM を ping-pong で回すため、env を VM へ抜くと壊れる。
3. **残 tree-walk の支配トラフィックは carrier/concurrency**（whitelist sample 実測）: method-fallback top =
   `Promise.at`=2000 / `await`=1200（＝**concurrency = Track C**）、`WHAT`（MOP carrier）、`EVAL`（carrier）、
   残りは niche（`map`/`CALL-ME`/`tree`/`parse` 各 ~10-20）。**汎用ユーザーコード tree-walk は枯渇**。

### 核心の再認識: env fold に「孤立した第1スライス」は存在しない

- **hot な per-call-frame state（env / readonly_vars / let_saves）は一括 fold しかできない**: いずれも関数呼び毎に
  save/restore される最ホット state。`Arc<RwLock>` handle playbook（registry/io_handles/output_sink/current_package
  の4実績）は **per-access lock で perf 破綻**（env と同じ理由）＝早期移管不可。plain field 化は tree-walk reader
  撲滅後にしか不可。→ **readonly_vars/let_saves を「先に寄せる」案は perf 上も不可**（doc 旧記述の「比較的局所＝早期
  候補」を**訂正**: これらは env と同じく hot で、最終 fold まで動かせない）。
- **env を読む carrier は撲滅不可能な Raku セマンティクス**: `type_matches_value`（221-910行・~690行）は subset の
  `where` 句を **`eval_block_value`/`call_sub_value` + `self.env` 読み書き**で評価する（line 514/523/533）＝
  user code を走らせる carrier。同様に EVAL・正規表現埋め込み `{}`・`await`/Promise `.then` クロージャも env を読む。
  これらは「消す」のではなく、**最終的に env が VM 所有になった時、carrier 実行に env を貸す（env-loan: VM が env を
  一時 move-in→interpreter 実行→move-back、または `&mut env` を渡す）**機構が要る。＝Phase II の最終 fold は
  「全 tree-walk 撲滅 → env を抜く」ではなく「**env を VM 所有にし、carrier には env-loan で渡す**」設計。

### 唯一クリーンに早期移管できる cool state = `instance_type_metadata`（次スライス候補）

型メタ副テーブルは Q2 で全コンテナ値へ埋め込み済（#2952〜2985）だが、**Instance 値の型メタだけは
`Interpreter.instance_type_metadata: HashMap<u64, ContainerTypeInfo>` の副テーブルに残る**（mod.rs:1037）。
アクセスは **insert(4716) / get(4787 = `container_type_metadata` 内) / clone(5589 = clone_for_thread) の 5 サイトのみ**
＝borrow-held-across-reentry なし・cool（instance 構築時 write / 型検査時 read）＝**handle playbook が最もクリーンに効く**。
これを VM-readable handle 化すれば、`type_matches_value` の **非 subset パス（簡単型名 = registry + value_type +
instance_meta で判定、subset は rare）を VM-native 化** でき、41 bounce の大半を除去（subset のみ carrier fallback）。

### Phase II の現実的な進め方（実測で確定。PLAN.md「Critical path」が正準）

- **critical path（本体）= carrier の env-loan 機構**: env を VM 所有にし、EVAL/subset-where/regex-`{}`/Promise-`.then`
  が VM 所有 env を借りて実行できるようにする。これが入って初めて env を Interpreter から抜ける（→ dual-store 削除 →
  Interpreter 撤去）。env/readonly/let の hot state はこの最終 fold で**一括**（早期分割は perf 不可）。
- **off-critical-path（任意・並行）= cool side-table の handle 移管**: `instance_type_metadata` 等を handle 化すれば
  VM-native dispatch（type_matches 非subset fast-path 等）を解禁できる（current_package が registry pure-predicate
  dispatch を解禁した前例と同型）が、**これは env fold を直接進めない＝critical path ではない**（bounce 数削減の
  perf/cleanliness 寄り）。最終 fold 面の縮小として CP-3 に同梱 or 任意で進める。
- **concurrency tree-walk（await/Promise）は Track C** と並走（env fold とは独立の最大残トラフィック）。
