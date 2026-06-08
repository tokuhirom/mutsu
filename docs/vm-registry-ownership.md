# ② 宣言レジストリの VM 所有化（構造リファクタ設計）

[PLAN.md](../PLAN.md) の最終ゴール「tree-walking Interpreter 実行パス撤去 → dual-store 削除」の
**ステップ②**。①（個別フォールバック撲滅）は安いサイトが枯渇し、残りは構造的ブロッカー（②/③）前提。
本書は②の確定設計。一次台帳は [vm-interpreter-fallback-ledger.md](vm-interpreter-fallback-ledger.md)、
dual-store は [vm-dual-store.md](vm-dual-store.md)。

## 解くべき問題: 双方向所有ノット

- `VM` は `interpreter: Interpreter` を**値で所有**（`src/vm.rs`）。
- `Interpreter` は VM への参照を持たないが、**共有実行状態（env・宣言レジストリ・型システム）を独占保持**。
  VM ネイティブコードはそれらに `self.interpreter.<field>` 経由でしか触れない。
- 実行制御は `Interpreter.run_block_raw → VM::new(self) → vm.run() → (interp,result) を返す → *self=interp`
  で **VM↔Interpreter を ping-pong**。状態は Interpreter に「閉じ込められて」いる。

②＝この閉じ込められた状態のうち**宣言レジストリ**（class/role/enum/subset/sub/token + 関連メタ ~30
フィールド）を、VM と Interpreter が対等に触れる場所へ持ち上げる。③（env/型/state 移管）の前提であり、
台帳 §2 の関数 dispatch フォールバック（multi/sub 解決）撲滅の前提でもある。

## 設計を縛る制約（調査で確定）

1. **`Value` は `Send + Sync`**（`src/value/mod.rs` でコンパイル時 assert、内部は全て `Arc`）。
   `Interpreter` 自体が `std::thread::spawn` のクロージャへ move される（`spawn_user_thread<F: Send>`）。
   → **`Rc<RefCell>` は Send 違反で不可。** interior-mutable 共有は `Arc<RwLock>`/`Arc<Mutex>` 一択。
2. **レジストリは現在スレッドごとに deep-clone（スナップショット）**（`clone_for_thread`、書き戻し無し）。
   → ②は**この意味論を厳守**（`start {}` 内宣言は親へ漏れない／親の宣言は子から見える）。
3. **登録処理は登録中にユーザコードへ再入する**。`register_class_decl`/`register_sub_decl`/
   `register_enum_decl` は `eval_block_value`/`run_block_raw`/`call_function("trait_mod:<is>")` を登録の
   最中に呼び、クラス本体文・BEGIN phaser・trait ハンドラ・属性デフォルト・enum 変種値・パラメタ化 role
   本体を実行する。その再入コードは registry を**再帰読み書き**し得る。
   → `&mut Registry` を関数全体で借用する「plain field + &mut」案は遷移期に**借用エラー**で破綻。
   ping-pong で registry が VM↔Interpreter を往復する点も合わせ、**遷移期は共有ハンドル一択**。

## 決定（ユーザー方針 2026-06-08）

- **エンドステート = VM 単独所有の plain field**（Interpreter オブジェクト消滅後）。これが確定の方向。
- **遷移期の表現 = `Arc<RwLock<Registry>>` 足場**。Interpreter が保持し各 VM へハンドル複製。
  ③/④で Interpreter 実行パスと ping-pong を撤去した時点で **plain な VM フィールドへ畳む**
  （Arc/lock が要るのは「2者が共有するから」だけ。所有者が1者になれば自然消滅）。
- **規律（重要）**: RwLock ガードを**再入を跨いで保持しない**（短い critical section で acquire→読み書き→
  drop、`eval_block_value`/`run_block_raw`/`call_function` の前に必ず drop）。RwLock は非再入なので必須。
- **スコープ = 構造リファクタのみ（挙動不変）**。台帳 §2 fallback の実消化は ② 完了後の別 PR。

## `Registry` 構造体（`src/runtime/registry.rs`）

`Interpreter` から `Registry` へ移す宣言レジストリ群（論理グループ）:

- **Functions/Subs**: functions, our_scoped_functions, proto_functions, proto_subs, token_defs, proto_tokens
- **Classes**: classes, cunion_classes, hidden_classes, class_stubs, package_stubs, hidden_defer_parents,
  class_trusts, class_how_values, class_composed_roles, class_enum_roles, class_subs,
  attribute_build_overrides, class_attribute_defaults, class_attribute_is_types, class_attribute_deprecated
- **Roles**: roles, user_declared_roles, role_candidates, role_parents, role_hides, role_type_params,
  class_role_param_bindings
- **Enums**: enum_types — **Subsets**: subsets

保持・複製: `Interpreter.registry: Arc<RwLock<Registry>>` + `registry()`/`registry_mut()` ヘルパ。
`clone_for_thread` は `Arc::new(RwLock::new(self.registry.read().clone()))` で中身 deep-clone
（スナップショット意味論を厳守）。全フィールド移行後、現在 ~30 行の個別 `.clone()` が 1 行に畳まれる。

**境界（②に含めない）**: `type_metadata`/`array_type_metadata` 等の `Arc::as_ptr` keyed 副テーブル
（🟣第一級コンテナ案件）、env/型検査本体/readonly/let/state/current_package（=③）。

## 移行手順（strangler-fig、各 PR は挙動不変・CI が安全網）

- **PR-A（抽出）**: フィールド群を Registry へ移し全アクセスを lock 経由へ。グループ単位の増分:
  1. enum/subset（最小・再入浅い、機構確立）
  2. class メタ群（小型・clone-cheap）
  3. classes（ホット経路・大きい `ClassDef`。**naive clone 回避**＝Registry アクセサで最小データを返す）
  4. roles 群（perf 敏感）
  5. functions/subs/tokens（移行後 `clone_for_thread` 1 行化）
- **PR-B（read 側移行）**: lookup/MRO/型マッチの registry 読み部を Registry メソッド化、VM の ~15-20 read
  サイトを `self.registry.read()` へ。③/§2 dispatch 撲滅の前提が整う。
- **PR-C（write-through 整理）**: register_*_decl の registry 書き込みを write-lock ブロックへ整理、
  再入跨ぎ guard 無しを保証、台帳注釈更新（登録が実行をトリガする CARRIER）。

## 進捗

- **PR-A slice 1（#2760）**: 機構確立 + enum/subset 移行。`src/runtime/registry.rs` 新設、
  `registry()`/`registry_mut()` ヘルパ、`clone_for_thread` 配線（snapshot 維持）、~75 サイト変換、
  再入ハザード（subset `where` 評価/ base-chain walk）をガード巻き上げで安全化、`resolve_subset_base_type`
  は `String` 返しへ。build/clippy/make test 緑、whitelist enum/subset/coercion 15 件 PASS。
- **PR-A slice 2（本 PR）**: class メタ群 14 フィールド移行（cunion_classes, hidden_classes, class_stubs,
  package_stubs, hidden_defer_parents, class_trusts, class_how_values, class_composed_roles, class_enum_roles,
  class_subs, attribute_build_overrides, class_attribute_defaults, class_attribute_is_types,
  class_attribute_deprecated）。read=`registry()`/write=`registry_mut()` へ ~130 サイト変換。
  `clone_for_thread` の 14 行個別 clone を削除（Registry 全体 clone に吸収）。`class_composed_roles` の
  builtin seed は `Interpreter::new` で `Registry::default()` に投入。参照返却アクセサ 3 本
  （`class_composed_roles`/`class_attribute_default`/`class_attribute_deprecated`）を owned 返し
  （`.cloned()`）へ変更し VM/runtime 呼び出し側を追従（ガードへの参照を返せないため）。再入ハザードを
  ガード巻き上げで安全化: `collect_transitive_roles` 前の `class_composed_roles` clone、`call_sub_value`
  前の `attribute_build_overrides` clone、`has_class_scoped_subs` の二重 read を単一 let-bound guard へ。
  VM 直アクセスは `vm.rs` の `package_stubs.insert/remove` を `registry_mut()` 経由へ。build/clippy/make test
  緑、whitelist class/role/attribute roast 緑（非 whitelist の private-method 既存失敗は不変）。
- **PR-A slice 3（本 PR, #2762 後）**: `classes: HashMap<String, ClassDef>` 移行（~178 サイト）。
  `ClassDef` を `pub(crate)` 化、Registry から `Debug` derive を除去（ClassDef/MethodDef/AST graph が非 Debug）。
  builtin classes は `Interpreter::new` で `Registry { classes, ..default() }` に投入。`clone_for_thread` の
  `classes.clone()` を削除（Registry 全体 clone に吸収＝snapshot 維持）。**perf 方針（#2746 の轍回避）**:
  whole-`ClassDef` の naive clone は一切せず、ホットパス（`resolve_method_with_owner_impl`/`class_mro`/
  `compute_class_mro`/type matching）は従来同様 targeted 投影 clone（`mro.clone()`/`methods.get(m).cloned()`）の
  まま `registry()` ガード経由に。release microbench（病的な純メソッドディスパッチ 30万回）で **+2–4%**＝
  遷移期 RwLock 取得コストのみ（実ワークロードでは <1%、Interpreter 撤去後に plain field へ畳めば消滅）。
  **再入安全性**: ① `clone_for_thread` がスレッド毎に独立 Arc を生成するため lock はスレッド間非共有→単一スレッド
  内の再帰 read は futex 実装で安全。② 真の危険＝read ガード保持中の同一ロック write（read→write 昇格 deadlock）
  は皆無を確認（write は別オブジェクト `nested` か、guard drop 後）。③ `&mut self` 再入を跨ぐ read ガードは
  edition-2021 の `if let` 一時値スコープ罠を含め全て `let` 巻き上げ／clone-out で解消（resolution.rs の
  multi/private dispatch、methods_object の BUILD、private-zeroarg fast-path の cache 書き込みを guard 外へ）。
  全 `get_mut` ボディは registry 非再入を確認。build/clippy/make test 緑。
- 残（フィールド別 total/writes）: roles(110)/role_parents(45)/role_candidates(21)、functions(96)/token_defs(35)。

## 完了の定義（②）

`Interpreter` から宣言レジストリ ~30 フィールドが消え `registry: Arc<RwLock<Registry>>` 1 本に。VM の
registry 系 lookup が `self.registry.read()` 経由へ。`clone_for_thread` のレジストリ複製が 1 行化（snapshot
不変）。台帳の §2 前提「②レジストリ」を満たした旨を更新。③（env/型/state 移管）と §2 撲滅へ進める状態。

## 非ゴール

env/型/state 移管（③）、登録の完全 VM ネイティブ化（③/④後）、台帳 §1/§2 の実消化（②完了後の別 PR）、
`type_metadata` の Arc-ptr keying 解消（🟣第一級コンテナ）、Arc<RwLock>→plain field の最終畳み込み
（Interpreter 撤去後＝④/⑤）、スレッド間 registry の真共有（スナップショット廃止、PLAN §8.3 の concurrency）。
