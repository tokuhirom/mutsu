# VM → Interpreter フォールバック台帳

最終ゴール「tree-walking Interpreter 実行パスの撤去 → dual-store 削除」（[PLAN.md](../PLAN.md) ①〜⑤）の
**進捗台帳**。VM (`src/vm/`) が今も Interpreter (`src/runtime/`) に委譲しているサイトを 1 箇所ずつ列挙し、
撲滅のたびに行を消す。関連: [vm-interpreter-dedup.md](vm-interpreter-dedup.md)（重複削除）、
[vm-dual-store.md](vm-dual-store.md)（locals↔env）、[vm-decoupling.md](vm-decoupling.md)（dispatch）。

## 用語

- **真フォールバック** = 本来 bytecode で実行すべきだが今は tree-walk Interpreter に委譲しているもの。
  コードに `// TODO: compile to bytecode` を付与。これらを撲滅するのが ①。
- **CARRIER** = 撲滅対象ではない本質的委譲（reflection / MOP / EVAL サブ VM / メタプロ hook / mode 状態）。
  Interpreter が共有レジストリ・実行状態を所有するための参照であって tree-walk ではない（④ で「分離 or 明示」）。
  コードに `// CARRIER:` を付与。③で env/レジストリ所有が VM に移れば単なる共有参照になる。

可視化の現状: `grep -rn "TODO: compile to bytecode" src/vm/` = 18 マーカー、`grep -rn "// CARRIER:" src/vm/` = 8 マーカー。
（1 マーカーが近接する複数サイトを束ねる箇所あり。下表が正準。）

## §1 — メソッドディスパッチの真フォールバック（`call_method_with_values` / `_mut_`）

| file:line | 受け手 / メソッド | 難易度 | ブロッカー / 依存 |
|---|---|---|---|
| `vm_call_method_compiled.rs` native-method (非mut) | IO::Pipe/IO::Handle 等の組み込みクラスメソッド | HARD | ③（実装 `native_io_*` がファイルハンドル等の interpreter 所有状態を要求。当初「個別可」は誤り） |
| `vm_call_method_compiled.rs` catch-all (非mut) | generic Instance/Buf/Failure メソッド | HARD | ③ state 所有移管（残る主 tree-walk） |
| `vm_call_method_compiled.rs` native-method (mut) | 同上 mut | HARD | ③（同上） |
| `vm_call_method_compiled.rs` catch-all (mut) | 同上 mut | HARD | ③ |
| `vm_call_method_mut_ops.rs` catch-all | generic mut メソッド | HARD | ③ |
| `vm_call_method_mut_ops.rs` array-backed instance | `is Array` storage の push/pop/shift | MEDIUM | 第一級コンテナ Phase 2 |
| `vm_data_ops.rs` shared push | `@a.push` (threaded) | HARD | lever B（共有セル所有） |
| `vm_data_ops.rs` shaped push | shaped 配列 push | MEDIUM | shaped 次元メタ検査の VM 化 |
| `vm_data_ops.rs` non-simple push | closure-captured / 非Array push | MEDIUM | 第一級コンテナ Phase 2（ContainerRef） |
| ~~`vm_smart_match.rs` key-method~~ | ~~smartmatch のキーメソッド抽出~~ | — | **✅消化 (PR2)**: 統一 compiled-first へ |
| `vm_call_helpers.rs` hyper temp | temp-bind した item への hyper メソッド | MEDIUM | 第一級コンテナ Phase 2 |
| `vm_register_ops.rs` react loop | `run_react_event_loop[_drain]` | HARD | lever B（async state 所有） |

## §2 — 関数ディスパッチの真フォールバック（`call_function` / `call_function_fallback`）

| file:line | 文脈 | 難易度 | ブロッカー / 依存 |
|---|---|---|---|
| ~~`vm_var_get_ops.rs` 0-arg term~~ | ~~0引数のユーザ/multi 関数 term~~ | — | **✅消化 (PR3)**: cold fallback を統一 compiled-first へ（OTF compile 追加） |
| ~~`vm_var_get_ops.rs` pkg-qualified~~ | ~~`Module::func` を term 位置で~~ | — | **✅消化 (PR3)**: 同上 |
| `vm_call_func_ops.rs` builtin-shadow / multi-dispatch / final else | `call_function_fallback` 直呼び等 | HARD | ② レジストリ / VM 側 multi 解決 / ③ |
| `vm_call_dispatch.rs` catch-all | `call_function_compiled_first` 末端 | HARD | ③ |
| ~~`vm_dispatch_helpers.rs` Routine call_function~~ | ~~Routine 値の関数解決~~ | — | **✅消化 (③ PR-1)**: 3 サイトを統一 compiled-first へ（builtin 名 Routine は builtin 優先維持） |

## §C — CARRIER（撲滅対象外・文書化して残す。④で確定）

| file:line | 種別 | 理由 |
|---|---|---|
| `vm_call_method_compiled.rs` pseudo-method (非mut/mut) | MOP reflection | DEFINITE/WHAT/WHO/HOW/WHY/WHICH/WHERE/VAR は反射、bytecode 形なし |
| `vm_call_method_compiled.rs` ^metamethod (非mut/mut) | MOP | `Foo.^bar` メタメソッドは MOP 所有 |
| `vm_register_ops.rs` `.VAR` + `trait_mod:<is>` | コンテナ反射 + メタプロ hook | `.VAR` 反射 + ユーザ trait ハンドラ |
| `vm_register_ops.rs` `trait_mod:<is>` 各サイト | メタプロ hook | sub/型/変数への is トレイト適用（複数箇所） |
| `vm_var_get_ops.rs` pseudo-package | 反射スコープ解決 | `SETTING::`/`OUTER::`/`CALLER::`/`DYNAMIC::` |
| `vm_var_get_ops.rs` test-function | mode 状態 | `make-temp-dir` 等の Test ハーネス dispatch |
| `vm_var_get_ops.rs` call-chain | MOP dispatch stack | `callsame`/`nextsame`/`callwith`/`nextwith`/`nextcallee`/`lastcall` |
| `vm_call_func_ops.rs` / `vm_call_dispatch.rs` carrier 分岐 | EVAL サブ VM | `EVAL`/`EVALFILE` は compile→サブ VM 実行。`is_interpreter_carrier_function` で runtime 判定済み |

## 進捗ログ

- **2026-06-08 (PR-1, #2755 merged)**: ① の起点。全フォールバックを `// TODO: compile to bytecode` / `// CARRIER:`
  で可視化し本台帳を新設。併せて EASY 撲滅 1 件: `succ`/`pred`（`vm_var_assign_ops.rs` の `increment_value_smart`/
  `decrement_value_smart`）の生 `interpreter.call_method_with_values` を統一 compiled-first ディスパッチ
  `try_compiled_method_or_interpret` に差し替え（§1 から 2 サイト消化）。
- **2026-06-08 (PR-2)**: 同手法で value-receiver の生 `call_method_with_values` をさらに 2 サイト消化 →
  `vm_smart_match.rs` smartmatch キーメソッド、`vm_dispatch_helpers.rs` Routine の method-dispatch 分岐
  （`&?ROUTINE.dispatcher()(self,…)`）を統一 compiled-first ディスパッチへ。ユーザ定義メソッドは compiled bytecode
  実行になり、native/reflective のみが共有末端へ。t/smartmatch-method-dispatch.t 追加。S03-smartmatch 全 pass。
  （補足: fat-arrow `$o ~~ (k => v)` が False を返すのは Pair 分岐到達前の別パースバグで本作業の対象外。コロンペアは正常。）
- **2026-06-08 (PR-3)**: §2 の cold な term 位置フォールバック 2 件（`vm_var_get_ops.rs` の 0-arg term /
  pkg-qualified）を生 `interpreter.call_function` から統一エントリ `call_function_compiled_first` へ寄せ、
  単純ユーザ sub の OTF compile を追加（interpreter は終端のみ）。あわせて native-method 行を **③-blocked** に訂正
  （`native_io_*` がファイルハンドル等の interpreter 所有状態を要求するため「個別可」は誤りだった）。

- **2026-06-08 (② 抽出/read/write-through 完了, #2760-2772 + 本 PR)**: 宣言レジストリの VM 所有化（phase ②）が
  PR-A（抽出・全フィールドを `Registry` へ）→ PR-B（lookup/MRO/型マッチを `impl Registry` メソッド化）→ PR-C
  （`register_*_decl` の write-through 整理）まで完了。一次情報は `docs/vm-registry-ownership.md`。PR-C は
  **登録パス＝CARRIER 性**（`register_class_decl`/`register_sub_decl`/`register_enum_decl`/`register_role_decl` は
  登録中に `eval_block_value`/`run_block_raw`/`call_function`＝クラス本体・trait ハンドラ・属性デフォルト・enum 変種値・
  パラメタ化 role 本体を実行する＝実行トリガ）を確認し、`registry()`/`registry_mut()` を**再入検出ラッパ guard**へ
  差し替えて「RwLock ガードを再入を跨いで保持しない」規律を **debug ビルドで実行時に強制**（同一ロック再取得を
  ブロッキング呼び出し手前で位置付き panic、ロックアドレスでキー付けして別 registry 同時保持の正当ケースは許容）。
  これで §2 の関数 dispatch fallback（`vm_call_func_ops.rs` multi/sub 解決, `vm_dispatch_helpers.rs` Routine）撲滅の
  構造前提（②）が整った。残る §1/§2 は ③（state 所有移管）が前提。

- **2026-06-08 (③ PR-1, Routine dispatch)**: ③設計を `docs/vm-state-ownership.md` に確定（③は②と異なり
  **フォールバック撲滅駆動**＝共有ハンドル方式不可・env は plain field 終状態。状態×結合度マップ作成）。最初の
  スライスとして `vm_dispatch_helpers.rs` の Routine 値 dispatch（vm_call_on_value）の 3 つの生
  `interpreter.call_function`（qualified / bare / 末端）を統一エントリ `call_function_compiled_first` へ寄せ、
  ユーザ定義 sub/multi/proto を compiled bytecode 実行に。**builtin 優先の保全**: `&SETTING::...::not` は
  `Routine{GLOBAL, "not"}` に解決される（accessors.rs）＝ユーザ `sub not` が名前を shadow していても builtin を
  指す意図。`Interpreter::is_builtin_function` ガードで、builtin 名を持つ Routine のみ `call_function` の
  builtin 優先を維持（平の user `&not` は `Value::Sub` で Routine 枝に来ない）。最初の naive 変換で
  `S02-names/SETTING-6e.t` が回帰（user `sub not` + `&SETTING::not`）し実証。pin = `t/routine-value-dispatch.t`(10)。
  S06/S02-magicals/S02-names/S03-smartmatch whitelist 137 件緑。

### 重要な現状認識（2026-06-08, PR-3 時点）
**「生ディスパッチを統一エントリへ降ろすだけ」で消せる安いサイトは枯渇した。** 残る §1/§2 のフォールバックは
すべて構造的ブロッカー（②宣言レジストリ / ③state 所有移管 / 第一級コンテナ Phase 2 / lever B）が前提であり、
個別の routing では消えない。とくに §1 の catch-all 群（残る主 tree-walk）と native-method（IO 系）は **③** が、
mut 系 push/hyper/array-backed は **Phase 2** が、shared push/react は **lever B** が前提。
**次の実質的進捗は ② または ③ の構造リファクタであり、設計を要する**（VM が `interpreter: Interpreter` を所有
しつつ interpreter 側も同 state で tree-walk する双方向所有を解く必要がある）。

## 撲滅の順序（PLAN.md ①〜⑤ に対応）

1. ~~EASY/MEDIUM な §1/§2 の個別撲滅~~ — **枯渇（PR-1〜3 で消化）**。残りは下記の構造ブロッカー前提。
2. ② 宣言レジストリ（class/role/enum/subset/sub/token）の VM 所有化。
3. ③ env/型検査/readonly/let の VM 所有移管（最大の山。catch-all 群・native-method はここで消える）。
4. ④ §C carrier の最終確定（所有が VM に移れば単なる共有参照）。
5. ⑤ `env_dirty`/`saved_env_dirty`/`ensure_locals_synced`/`sync_locals_from_env` 削除。
