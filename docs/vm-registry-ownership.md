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
- **PR-A slice 4（本 PR, #2763 後）**: roles 群 7 フィールド移行（roles, user_declared_roles, role_candidates,
  role_parents, role_hides, role_type_params, class_role_param_bindings、~200 サイト）。`RoleCandidateDef` を
  `pub(crate)` 化（`RoleDef` は既に Debug/Clone/pub(crate)）。builtin roles seed は `Interpreter::new` の
  registry ブロックで `registry.roles = {..}` に投入、`clone_for_thread` の 7 個別 clone 削除。**参照返却アクセサ
  2 本**（`get_role_def -> Option<&RoleDef>`、`class_role_param_bindings -> Option<&HashMap>`）を owned 返しへ。
  VM 側は両アクセサ経由のみ（`.is_some()`/`.map()` なので呼び出し側不変）。再入安全化: ① **read→write 同一ロック
  deadlock を 2 件発見・修正**（registration_class.rs の role hidden フラグ→hidden_classes、role_hides→
  hidden_defer_parents。read ガード保持中に `registry_mut()` を呼ぶ＝借用チェッカ未捕捉のデッドロック）。
  Python ブレース対応スキャナで全 registry フィールドの read-get ボディ内 `registry_mut` を網羅検査し 0 を確認。
  ② `&mut self` 再入を跨ぐ read ガード（`if let` scrutinee は本クレート edition2024 でもボディ全体で生存）を
  clone-out/`let` 巻き上げで解消（methods.rs role 型punning、methods_object の new() role bindings、smart_match の
  parametric parents、resolution の class+role メソッド表 or_else 単一ガード化）。③ 一文 2 read ガード
  （`classes.contains_key || roles.contains_key` 3 箇所）を単一束縛ガードへ。build/clippy/make test 緑。
  挙動不変確認（ロール合成/継承/パラメタ化/punning/conflict/`but`; Stack配列属性の Nil は main と同一の既存ギャップ）。
- **PR-A slice 5 = PR-A 最終（本 PR, #2764 後）**: functions/subs/tokens 群 6 フィールド移行（functions,
  our_scoped_functions, proto_functions, token_defs, proto_subs, proto_tokens）。`FunctionDef` は既に
  Debug/Clone/pub(crate)。builtin seed 無し（全ユーザ定義）。**これで宣言レジストリが全て Registry に入り、
  `clone_for_thread` の個別フィールド clone がゼロ＝registry 全体 clone 1 本のみ**（②の完了の定義を達成）。VM 直
  アクセス無し。snapshot_routine_registry は単一 read ガード化、restore は read（our_scoped 収集）→guard drop→
  write の 2 段に分離。regex/grammar サブインタプリタ構築の struct リテラル 9 箇所（`Interpreter { functions:..,
  token_defs:.., ..Default }`）は `copy_decl_registry_into(&mut interp)` ヘルパへ（別 Arc 間 snapshot コピー）。
  **★重大: write→write 同一ロック deadlock を発見・修正**: `match self.registry_mut().functions.entry(k) {
  Occupied => { ... self.registry_mut().functions.entry(k2) ... } }` ＝外側 write ガード保持中に arm 内で再度
  `registry_mut()`。**借用チェッカは別 `registry_mut()` 呼び出しを別借用と見るため未捕捉**で、**スキャナでも当初
  見逃し→multi sub 宣言が実行時ハング**（スモークテストで顕在化）。`insert_multi_overload` ヘルパ（単一 write
  ガードで base→`__m{N}` fallback）に統合。**教訓: read→write だけでなく write→write/write→read も危険。
  検出スキャナは `match/if-let self.registry_mut()` ブロック本体の registry 再アクセスも走査せよ（下記）。
  最終防衛線は make roast のタイムアウト**（make test+51823 roast サンプル緑、回帰ゼロ）。

## PR-B（read 側移行）進捗

- **PR-B slice 1 = MRO/クラス lookup の Registry メソッド化（本 PR）**: MRO 計算は VM の全メソッド
  ディスパッチが叩く最ホットの registry-read。これを `impl Registry`（`src/runtime/registry.rs`）の
  pure メソッドへ降ろした:
  - `Registry::compute_class_mro(&self, name, stack) -> Result<Vec<String>>` — `self.classes` のみ参照
    する純 C3 線形化。再帰は registry 内に閉じ、ユーザコード再入なし。
  - `Registry::class_mro(&mut self, name) -> Vec<String>` — builtin 階層（Match/Capture/CompUnit…）+
    parameterized（`Foo[Bar]`）+ `ClassDef::mro` キャッシュ参照 + compute + キャッシュ書き戻しを**単一
    write ガード**で実施。従来 `Interpreter::class_mro` は最大 5 本の `registry()`/`registry_mut()` を
    個別取得していたのを 1 本に集約（ホットパスの lock 取得削減）。
  - `Registry::class_mro_cached(&self, name) -> Option<Vec<String>>` — readonly（キャッシュ or 単一要素）。
  - `Registry::class_has_method(&mut self, name, method) -> bool` — MRO walk による method 有無判定。
  Interpreter 側 `class_mro`/`compute_class_mro`/`class_mro_readonly`/`class_has_method` は薄い委譲
  ラッパへ（65 + 多数の呼び出し側は無改変）。挙動不変、build/clippy/make test 緑、S12/S14 roast 緑。
- **VM 直読みについて（調査結果）**: 現状 VM が registry フィールドを直接読むサイトは皆無で、`package_stubs`
  の 2 write のみ（`vm.rs`）。VM の registry 系アクセスは全て `self.interpreter.<wrapper>()` 経由
  （`type_matches_value` 40+ サイト、`class_mro` 等）。よって「VM ~15-20 read サイトを `self.registry.read()`
  へ」は実コードと不一致＝VM への registry ハンドル追加は本 slice では不要（②非ゴールの Arc→plain 畳み込み
  と一体で ③/④へ）。read 側移行の実体は wrapper の Registry メソッド化＝本 slice。
- **PR-B slice 2 = メソッド解決/型マッチの registry-read メソッド化（本 PR）**: dispatch ホットパスに散在する
  同型の pure-registry read を `impl Registry` の owned 返しメソッドへ集約（ガード即 drop の規律を維持）:
  - `Registry::get_method_overloads(&self, class, method) -> Option<Vec<MethodDef>>` — `classes.get(c).methods.get(m).cloned()`。
    `resolution.rs` の同型 5 サイト（`resolve_method_with_owner_impl` のオーバーロード read、
    `resolve_methods_per_mro_level` の read + any_multi 再チェック、private 解決 2 サイト）を変換。
  - `Registry::get_role_param_bindings(&self, class) -> Option<HashMap<String,Value>>` — 同 4 サイト変換。
  - `Registry::is_hidden_class` / `is_hidden_defer_parent(class, owner) -> bool`（後者は所有 set を返さず
    述語化＝`&self`-only 呼び出し側の gratuitous clone を回避）。`should_skip_defer_method_candidate` を変換。
  - `Registry::composed_roles_seed(&self, mro) -> Vec<String>` — composed-role 推移 walk の**シードのみ**
    （MRO→`class_composed_roles`、push 順保持・dedup/sort 禁止＝LIFO walk の first-match-wins が load-bearing）。
    `type_matching.rs` の Block A（Package, `resolved_constraint.is_some()`）/ Block B（Instance,
    `roles.contains_key`）両方のシードに適用。**推移 walk 本体はインライン据え置き**: gate が
    block A=`resolve_role_key`（env+lock 再入）/ block B=`roles.contains_key`、match が `type_matches` /
    exact `== constraint` で非自明に異なり、`resolve_role_key` gate を Registry メソッド内（read ガード保持中）
    から呼ぶと std `RwLock` 非再入で deadlock するため。
  - **変換しない（意図的）**: `resolve_all_methods_with_owner` の class-OR-role `.or_else` fallback（非同型）、
    private zero-arg fast-path（borrow 走査で matched 1 件のみ clone のホットパス最適化＝Vec 全 clone 退行回避）。
  - **対象外（別件）**: `methods_classhow.rs`/`methods_walk.rs` の composed-role walk は構造差（VecDeque BFS vs
    LIFO Vec、base strip タイミング差、二重 map closure）で walk 順が変わるため据え置き。`functions.get(&Symbol)`
    lookup は周辺キー構築が `current_package`/`env` 依存のため Interpreter 側据え置き。
  挙動不変、build/clippy/make test 緑、S12/S14（typecheck/parameterized/generic-subtyping 含む）roast 緑。
- **残（PR-B 後続）**: `resolve_method_with_owner_impl` の残る tie-break/candidate 収集（type-distance 計算等）は
  既に owned clone 後の純メモリ処理で registry-read は本 slice で集約済み。次は **PR-C**（register_* の
  write-through 整理）。

## PR-C（write-through 整理）進捗 — **②完了**

- **再入跨ぎ guard 無しを *実行時* 保証（本 PR の核心）**: 従来 `registry()`/`registry_mut()` は std の
  `RwLockReadGuard`/`RwLockWriteGuard` を直接返していたため、再入で同一ロックを再取得すると**サイレントに
  デッドロック**し、make roast の ~13 分タイムアウトでしか捕捉できなかった。静的スキャナはコード形状の盲点が
  あり（PR-A slice 5 の `match self.registry_mut()...{}` arm 内 write→write は借用チェッカ・スキャナ双方を
  すり抜けて実行時ハング）。本 PR で `registry()`/`registry_mut()` を**再入検出ラッパ guard**
  （`RegistryReadGuard`/`RegistryWriteGuard`、`src/runtime/registry.rs`）へ差し替え:
  - debug ビルドのみ、取得直前に thread-local の保持状況を検査し、**`.read()`/`.write()` のブロッキング呼び出し
    の手前**で位置付き panic（デッドロックする代わりに即・明示的に落ちる）。
  - **ロックのアドレスでキー付け**（thread-global ではない）。単一スレッドが*別の*レジストリの guard を同時保持
    する正当ケース（`self.registry_mut().classes = nested.registry().classes.clone();` ＝ self の write guard と
    sub-interpreter `nested` の read guard。別ロックなのでデッドロックしない）を誤検出しないため。
  - 許可/禁止行列は std `RwLock` の実デッドロック条件に一致: 同一ロックで write-while-any / read-while-write は
    panic、read-while-read（nested read。`a().x && b().y` で両 temporary guard が文末まで生存）は許容。
  - release は `#[cfg(debug_assertions)]` で完全に消える（ホット `registry()` read 経路に thread-local bookkeeping を
    乗せない）。CI の release make roast はタイムアウトが最終防衛線のまま。
  - 検証: debug バイナリで t/ + S10/S11/S12/S14/S05/S04 等 6000+ ファイル実行・**再入 panic ゼロ**（false-positive
    無し、潜在再入バグ無し）。threading（start/promise/channel）も clean。
- **write-through 整理（register_class_decl）**: 連続する再入無し write/read クラスタを単一 guard ブロックへ集約し
  ロック取得回数を削減＋「この区間は guard 保持＝再入禁止」境界を構文で明示。① prologue の prev_* 5 read を
  単一 read guard ブロックへ（全て owned/cloned）。② `restore_previous_state` ロールバッククロージャの 10 個の
  `this.registry_mut()` を単一 write guard へ（本体に再入無し）。③ stub クリアの `class_stubs.remove` +
  `package_stubs.remove` 連続 2 write を単一 write guard へ。上記の実行時 guard が安全性を担保（誤って read 保持中に
  クロージャを呼べば即 panic するが、6000+ 実行で発火せず＝呼び出し点に held guard 無しを確認）。
- **台帳注釈**: `docs/vm-interpreter-fallback-ledger.md` 進捗ログに②抽出フェーズ完了＋登録が実行をトリガする
  CARRIER 性を記録。

## 完了の定義（②）— **PR-A/B/C 完了（#2760/2762/2763/2764/2767/2769/2772/本PR）**

`Interpreter` から宣言レジストリ全フィールドが消え `registry: Arc<RwLock<Registry>>` 1 本に。`clone_for_thread`
のレジストリ複製は registry 全体 clone 1 行のみ（個別フィールド clone ゼロ、snapshot 不変）。VM の registry 系
lookup は accessor 経由（多くは既に owned 返し）。**PR-A**（抽出）→ **PR-B**（lookup/MRO/型マッチを Registry
メソッド化）→ **PR-C**（register_* の write-through 整理＋再入跨ぎ guard 無しを実行時 guard で保証）まで全完了。
**次は ③**（env/型/state 移管＝interpreter ブリッジ撤去の最大の山）。

## PR-D（③ 後続: registry dispatch read の VM ネイティブ化）進捗

②で registry は VM ハンドル化済みだが、VM の dispatch 判定（`has_proto`/`has_multi_candidates`）は
`self.interpreter.has_proto(...)` バウンス経由だった（これら wrapper が `current_package` に結合し、
`current_package` が Interpreter 所有の plain field だったため）。`current_package` を共有ハンドル化した
直後スライス（`docs/vm-state-ownership.md` の current_package 移管エントリ）の上に構築:

- **`has_proto`/`has_multi_candidates` を pure `impl Registry` メソッド化**（`registry.rs`、`current_package`
  を引数で受ける純 registry+scope read・env/再入なし）。`Interpreter::has_proto`/`has_multi_candidates` は
  `self.registry().has_proto(&self.current_package(), name)` の薄い委譲へ（**1 操作 = 1 実装**）。
- **VM に `registry()` read accessor 追加**（`RegistryReadGuard`、`registry_mut()` と同規律＝再入を跨いで
  保持しない）+ VM ネイティブ `has_proto`/`has_multi_candidates`（VM 自前の `registry`+`current_package`
  ハンドルで読む）。VM の ~21 サイトを `self.interpreter.has_*` → `self.has_*` へ。`has_multi_candidates_cached`
  も VM ネイティブ呼びへ。`RegistryReadGuard` を `runtime` から re-export。
- **`resolve_function_with_types` は据え置き**（`self.env.get(...)` の prefix-package 可視判定 +
  `choose_best_matching_candidate` の `where` 節＝ユーザコード再入に結合＝pure 化不可。env 移管まで interpreter
  bounce が正解）。**`has_proto_token` も据え置き**（`mro_readonly` MRO walk に結合）。
- 挙動不変、build/clippy/make test 緑、S06-multi（proto/syntax/type-based/subsignature/lexical-multis/callsame）
  /S10-packages/S11-modules roast 緑。

## 非ゴール

env/型/state 移管（③）、登録の完全 VM ネイティブ化（③/④後）、台帳 §1/§2 の実消化（②完了後の別 PR）、
`type_metadata` の Arc-ptr keying 解消（🟣第一級コンテナ）、Arc<RwLock>→plain field の最終畳み込み
（Interpreter 撤去後＝④/⑤）、スレッド間 registry の真共有（スナップショット廃止、PLAN §8.3 の concurrency）。
