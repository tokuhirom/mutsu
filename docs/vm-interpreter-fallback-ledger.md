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
| `vm_call_method_compiled.rs` catch-all (非mut) | **native/Buf/Failure/MOP のみ**（ユーザーメソッドは全て bytecode 化済） | HARD | ③ state 所有移管。**③ PR-3 で実測訂正**: ここはユーザーメソッド tree-walk では**なかった** |
| `vm_call_method_compiled.rs` native-method (mut) | 同上 mut | HARD | ③（同上） |
| `vm_call_method_compiled.rs` catch-all (mut) | 同上 mut（native/Buf/Failure/MOP のみ） | HARD | ③ |
| `vm_call_method_mut_ops.rs` catch-all | generic mut メソッド（plain untyped `@`-array mutators ＋ mutable Buf write methods は除く） | HARD | ③ |
| ~~`vm_call_method_mut_ops.rs` plain `@`-array mutators~~ | ~~append/prepend/unshift/pop/shift~~ | — | **✅消化 (③ PR-5)**: `try_native_array_mut` で VM ネイティブ。typed/shaped/lazy/shared/constrained は interpreter 維持 |
| ~~`vm_call_method_mut_ops.rs` mutable Buf write methods~~ | ~~write-bits/write-ubits/write-num*/write-int*/write-uint*~~ | — | **✅消化 (③ PR-7)**: `try_native_buf_mut` で VM ネイティブ。pure 変換は `builtins/{buf_bits,buf_write_num,buf_write_int}` に一本化。type-object/Blob/malformed-arity は interpreter 維持 |
| ~~`vm_call_method_mut_ops.rs` 単純 array-backed Iterator~~ | ~~pull-one/skip-one/skip-at-least/skip-at-least-pull-one/sink-all~~ | — | **✅消化 (③ PR-9)**: `try_native_iterator` で VM ネイティブ（`$it.pull-one` は CallMethodMut＝mut パス）。`items`+`index` 自己完結のみ。squish（コールバック）/ lazy（gather/coroutine, `is_lazy`）/ push-*（外部バッファ）/ count-only/bool-only は interpreter 維持 |
| `vm_call_method_mut_ops.rs` array-backed instance | `is Array` storage の push/pop/shift | MEDIUM | 第一級コンテナ Phase 2 |
| `vm_data_ops.rs` shared push | `@a.push` (threaded) | HARD | lever B（共有セル所有） |
| `vm_data_ops.rs` shaped push | shaped 配列 push | MEDIUM | shaped 次元メタ検査の VM 化 |
| `vm_data_ops.rs` non-simple push | closure-captured / 非Array push | MEDIUM | 第一級コンテナ Phase 2（ContainerRef） |
| ~~`vm_smart_match.rs` key-method~~ | ~~smartmatch のキーメソッド抽出~~ | — | **✅消化 (PR2)**: 統一 compiled-first へ |
| ~~`vm_call_method_compiled.rs` QuantHash coercion~~ | ~~`.Set`/`.Bag`/`.Mix`/`.SetHash`/`.BagHash`（list-like 受け手）~~ | — | **✅消化 (③ PR-8)**: `try_native_quanthash_coerce` で VM ネイティブ。pure 折り畳みは `builtins/quanthash_coerce` に一本化。`.MixHash`（型メタ登録）/ Instance/Package 受け手は interpreter 維持 |
| `vm_call_helpers.rs` hyper temp | temp-bind した item への hyper メソッド | MEDIUM | 第一級コンテナ Phase 2 |
| `vm_register_ops.rs` react loop | `run_react_event_loop[_drain]` | HARD | lever B（async state 所有） |

## §2 — 関数ディスパッチの真フォールバック（`call_function` / `call_function_fallback`）

| file:line | 文脈 | 難易度 | ブロッカー / 依存 |
|---|---|---|---|
| ~~`vm_var_get_ops.rs` 0-arg term~~ | ~~0引数のユーザ/multi 関数 term~~ | — | **✅消化 (PR3)**: cold fallback を統一 compiled-first へ（OTF compile 追加） |
| ~~`vm_var_get_ops.rs` pkg-qualified~~ | ~~`Module::func` を term 位置で~~ | — | **✅消化 (PR3)**: 同上 |
| `vm_call_func_ops.rs` builtin-shadow（slip + 通常 2 サイト） | ユーザ sub が同名 builtin を shadow | 部分消化 | **✅単一候補の compilable 分は ③ PR-2 で OTF compile 化**（proto/multi/複雑 sig は call_function_fallback 維持） |
| `vm_call_func_ops.rs` multi-dispatch fork / final else | 非proto multi / `call_function` 末端 | 部分消化 | **✅非proto multi の unambiguous/OTF-compilable/非state 候補は ③ PR-4 で OTF compile 化**（ambiguity/where/state-alternate/proto/末端は fallback 維持） |
| `vm_call_dispatch.rs` catch-all | `call_function_compiled_first` 末端（実測でほぼ枯渇＝下記 PR-6 注記） | HARD | ③（残=lexical-alias-to-builtin / `__mutsu_*` 内部 / 並行〔lever B〕/ no-match エラー生成） |
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

- **2026-06-08 (③ PR-2, builtin-shadow fork)**: ユーザ定義 sub が同名 builtin を shadow する関数 dispatch
  fork（`vm_call_func_ops.rs` の `exec_call_func_op` 通常パス + `exec_call_func_slip_op` の 2 サイト、
  `user_function_matches_call` 枝）を、解決した def が **plain 単一候補かつ compilable** な場合のみ OTF compile
  （`compile_and_call_function_def`）へ降ろし bytecode 実行に。native arm へは落とさない（shadow された builtin を
  拾わないため）。compilable 判定は新ヘルパ `def_is_otf_compilable`（既存 OTF 枝のガードを集約）。**回帰を 2 段で発見・修正**:
  ① 当初 proto/multi も `resolve_function_with_types` の単一候補を OTF compile してしまい、proto'd multi の候補
  dispatch が壊れた（whitelisted S06-multi/proto・subsignature・type-based が mid-file abort = exit 255/Failed:0）→
  `!has_proto && !has_multi_candidates` ガード追加。② **`user_function_matches_call` 枝は builtin-shadow 専用ではなく
  「compiled_fns に無い（モジュール/動的登録）args 一致ユーザ sub」全般**が来る。`def_is_otf_compilable` は nested
  `sub` 宣言内の `when` 制御フロー等を捕捉できず、Test::Util `is-deeply-junction`（nested `junction-guts` + `when`）が
  OTF compile されると `when`-succeed が関数全体を脱出し eigenstate を取りこぼした（`t/test-util-is-deeply-junction.t`
  / `throws-like-any` 回帰）→ **`Interpreter::is_builtin_function(name)` ガードで実際の builtin shadow に限定**
  （非builtin のモジュール sub は従来通り tree-walk）。pin = `t/builtin-shadow-dispatch.t`(9)。S06 whitelist 86 件緑
  （S32-str gb2312/shiftjis は main でも落ちる既存環境依存）。残: 非proto multi fork（VM 側 multi 候補解決が前提）、
  catch-all 末端（③）、非builtin モジュール sub の tree-walk（compiled_fns 拡充 or 安全な汎用 OTF gate が前提）。

- **2026-06-09 (③ PR-3, §1 catch-all = ユーザーメソッド compile-on-demand + 実態訂正)**: §1 catch-all
  （`vm_call_method_compiled.rs` の `try_compiled_method_or_interpret` line 457 / `try_compiled_method_mut_or_interpret`
  line ~919）の dispatch チョークポイントに、**解決済みユーザーメソッド def が `compiled_code == None` の場合の
  compile-on-demand** を追加（新ヘルパ `populate_uncompiled_method`: `compile_class_methods`/`compile_role_methods`
  で canonical registry を冪等に compile→再解決→`dispatch_compiled_method` で bytecode 実行。owner が user
  class/role でない or 空ボディなら `None` で interpreter フォールバック温存）。pin `t/method-otf-dispatch.t`(14)。
  **最大の収穫は実態の実測訂正**: `MUTSU_VM_STATS=1` + プローブで catch-all 到達集合を計測した結果、台帳が
  「catch-all = 残る主 tree-walk（ユーザーメソッド）」としていたのは**誤りだった**。実際は:
  ① **通常宣言メソッド（multi method 含む）・submethod・role 合成・継承・`.^add_method` は全て登録時に
  `compile_class_methods` で bytecode 化済**（`.^add_method` は method リテラル Sub の `compiled_code` を引き継ぐ、
  `methods_classhow.rs:640`）。② catch-all に来る Instance トラフィックは **native coercion（`Exception.Stringy`）/
  MOP（`.does`/`.^does`）/ role-qualified（`WithAttr.AccessesAttr::meth` は resolve=None で resolved ブロックに
  入らない）** のみ＝③/builtins/別解決バグ依存。③ `populate_uncompiled_method` が実際に発火する唯一の compile-gap は
  **`.^add_multi_method`**（`compiled_code: None` 固定, `methods_classhow.rs:702`）＋将来の同種サイトの defensive
  カバー。よって本 PR は §1 catch-all の**ユーザーメソッド分を確実に bytecode 化**（残りは native/MOP/別解決バグ）。
  S12/S14/S06-multi/metamodel whitelist 全緑、make test PASS。**教訓: フォールバックカウンタは「tree-walk」ではなく
  「interpreter ブリッジ経由」を数える**（ブリッジ先の `run_block_raw` 自体は VM compile ping-pong で実は bytecode）。

- **2026-06-09 (③ PR-4, §2 非proto multi fork)**: `vm_call_func_ops.rs::dispatch_func_call_inner` の
  非proto multi fork（`has_multi_candidates && !has_proto`）を、PR-2 の builtin-shadow 型と同様に
  `resolve_function_with_types`（②で VM アクセス可能）で winner を解決 →`def_is_otf_compilable` かつ非state body なら
  `compile_and_call_function_def` で OTF compile/bytecode 実行に。**関数パスの ambiguity は `None`+`pending_dispatch_error`
  で表現される**（`dispatch.rs` choose_best_matching_candidate）ため `Some(def)` = unambiguous winner（メソッド側の
  `dispatch_ambiguous` フラグとは別機構）。resolve 前に stale `pending_dispatch_error` を `take` でクリア（interpreter の
  `resolve_function_with_alias` と同じ作法）。ambiguity/where/default/code-param/no-match/proto/末端は従来どおり
  `call_function_fallback`（正規の `X::Multi::Ambiguous`/`X::Multi::NoMatch` を投げる）。**nextsame/callsame/callwith は
  compiled 候補からでも正しく機能**（`compile_and_call_function_def` が interpreter と同じ `push_multi_dispatch_frame` を
  張るため。実測で確認＝redispatch 除外は不要だった）。**2 つの落とし穴を対処**: ① **otf_call_cache の name 汚染** —
  `compile_and_call_function_def` の name-cache insert を `!has_multi_candidates_cached` で条件化＋lookup 側にも同ガード
  （type-blind な name cache が `f(5)`→Int 候補を後続 `f("hi")` に誤再利用するのを防止。fingerprint キーの
  `otf_compile_cache` は per-候補で安全なので維持）。② **signature alternates の共有 state**（`(A)|(B){ state $x }` が
  compile 時 state_group で 1 セル共有するが、OTF は body fingerprint＝per-alternate-sig キーで別 state になる）→
  **state 宣言を含む multi body は OTF 除外**（新ヘルパ `function_body_declares_state` 再帰スキャン。`t/multi-signature-alternates.t`
  回帰を発見・修正）。slip 経路（`exec_call_func_slip_op`）の multi は **既存の別バグで `|ms()` が Nil を返す**ため
  本 PR では対象外（follow-up）。**③ 既存 flaky バグも修正**: `push_multi_dispatch_frame`（`accessors.rs`）が
  callsame/nextsame の「remaining 候補」frame の current を `resolve_all_matching_candidates().first()`＝**HashMap 順
  first match**で決めていたため、最狭候補が後宣言だと callsame が誤候補（or 同一候補）へ再ディスパッチし、
  プロセスのハッシュシードで ~50% flake（`roast/S06-advanced/callsame.t` 等 whitelist が間欠 fail。CI #2788 の
  失敗の実体）。**interpreter の inline frame と同じ決定的 winner（`resolve_function_with_alias`）で current を特定**する
  よう修正（全 VM 経路 = Path A / otf cache / compile_and_call を一括で決定化）。これで callsame は OTF 候補からでも
  正しく動くため redispatch 除外は不要に。pin `t/multi-otf-dispatch.t`(25, callsame-when-winner-declared-last 含む)。
  実測 multi-probe で fallback 5→2（残2=nextsame/callsame builtin 自体）。S06/S12/S14 whitelist 全緑（10x 決定的）、
  make test PASS。**教訓: 「自分の PR の CI fail」が既存 flaky の顕在化のこともある — シード依存の非決定性を実測で確定せよ。**
  **さらに回帰を1件発見・修正（PR-2 の轍）**: multi fork が **native Test ルーチン**（is-eqv/is-deeply 等。Rust 実装だが
  multi stub が registry に登録されている）を OTF compile し、native handler を迂回して `S16-io/words.t`・`S32-io/slurp.t` を
  決定的に破壊（is-eqv 経由）。非-builtin OTF パスと同じ `!is_interpreter_handled_function(name)` ガードを multi fork にも
  追加して解消（is-eqv は call_function_fallback → native test handler へ正しく回る）。ローカル full make roast PASS で確認。

- **2026-06-09 (③ PR-5, §1 catch-all = plain `@`-array mutators の VM ネイティブ化)**: `MUTSU_VM_STATS` に
  catch-all 受け手の型名（`Class.method`）を一時計測する計装を入れ、whitelist sample 全体で **catch-all 到達集合を実測**。
  PR-3 が「残りは native/Buf/Failure」と推定していたが、**実態は Buf/Failure ではなく配列ミューテータ + iterator protocol + coercion**
  だった（top: `Package.new`=38698〔③コンストラクタ・user BUILD/属性で③ブロック〕, `Array.append`=16721, `Array.shift`=5796,
  `Any.AT-POS`=5795, iterator `pull-one`/`skip-one`/`push-exactly` 系, `Array.splice`=381, `List.Set/Bag/Mix` coercion …）。
  `Package.new` を除く**最大の tractable カテゴリ＝配列ミューテータ**（append+shift+splice ≈ 22900）。本 PR は
  `vm_call_method_mut_ops.rs::exec_call_method_mut_op` に `try_native_array_mut` を追加し、**plain untyped `@`-array
  （`ArrayKind::Array`）への `append`/`prepend`/`unshift`/`pop`/`shift`** を VM ネイティブに（`env.get_mut` + `Arc::make_mut`
  で interpreter の primary branch と同一セマンティクス、empty pop/shift は `make_empty_array_failure_what(..,"Array")`）。
  **保守的フォールスルー**: typed（`var_type_constraint` Some）/ shaped / lazy（kind が `Array` 以外）/ shared
  （`shared_vars_active`）/ metadata 持ち（`container_type_metadata` Some）/ 非env-bound receiver は interpreter 維持。
  **落とし穴: type_metadata の Arc-ptr keying aliasing**（🟣第一級コンテナの既知ハザード）— `make_mut` 再確保で得た新 Arc
  ポインタが、解放済み typed array の stale `array_type_metadata` エントリと衝突し untyped 配列が `Array[Int]` に誤型付け
  （フルファイル文脈でのみ・アロケータ依存で間欠再現）。native path は untyped を保証済みなので、変異後の env 配列に対し
  `unregister_container_type_metadata` で防御的に stale エントリを除去（安全＝生きた別配列が同ポインタを持つことは不可能）。
  pin `t/native-array-mut.t`(31, aliasing ケース含む)。S32-array whitelist 全緑、make test PASS、array 系 whitelist 156/156。
  **残る catch-all**: `Package.new`〔③〕, `Any.AT-POS`/iterator protocol〔値型/iterator state〕, coercion〔builtins 降ろし候補〕, splice。

- **2026-06-09 (③ PR-6, §2 catch-all 末端の実測 + junction constructor の builtins 降ろし)**: `vm_call_dispatch.rs:79`
  の `call_function_compiled_first` 末端（`call_function` final else）に `END:` プレフィックス計装を入れ、whitelist sample
  全体で**末端到達集合を実測**。結論: **末端はほぼ枯渇**（PR-1〜4 ＋ line 63-67 の「resolve できた def は全て OTF compile」
  により、ユーザー関数はもう末端に来ない）。sample 全体で末端到達は diffuse で少量、内訳は ①`any`/`all`/`one`/`none`
  junction constructor（pure builtin・本 PR で降ろし）, ②**lexical `&`-変数の名前経由呼び出し**（末端残余の*最大*カテゴリ。
  `-> &op { … op(…) }` ＝S03-operators/set_*.t の `END:op`〔各348等〕で集合演算子 Callable をパラメータ束縛して呼ぶ、
  および `my &junction = ::("&any"); junction(|$_)` ＝S03-junctions/autothreading.t の `END:junction=56`。束縛された Callable
  〔user sub / 演算子 / builtin Routine〕を bareword 名で呼ぶため末端で lexical 解決＝正しく動作。将来スライス: VM 側で
  lexical `&`-var 束縛を検出し既存 Routine/compiled dispatch へ寄せる）, ③`__mutsu_*` 内部（CAS 等・並行は lever B）,
  ④no-match エラー生成（`notthere`＝末端で正規例外を投げるのが正しい）。
  **高トラフィックの builtin は末端に残っていない**（`split`/`index`/`comb` 等は既に native か、他4サイト〔vm_call_func_ops〕
  経由で Instance-guard fallback）。本 PR は唯一の pure-builtin カテゴリ＝**junction constructor を `builtins/functions.rs::build_junction`
  へ降ろし**（one-arg flatten rule 込み・state 不使用）、`native_function` で any/all/one/none を全 arity ルート、interpreter の
  `builtin_junction` も同 fn へ委譲して**重複実装を解消**（[[feedback_dedup_over_perf]]/[[feedback_placement_audit]]）。
  junction 構築は型非依存で安全なので `try_native_function` の Instance-arg ガードを any/all/one/none に限り bypass
  （`any($instance)` も native）。pin `t/native-junction-ctor.t`(24, Instance-arg 含む)。S03-junctions whitelist 全緑、make test PASS。
  **結論: §2 末端は「高トラフィック撲滅」フェーズを終えた。残る最大カテゴリは lexical `&`-var の名前呼び出し（正しく動作・将来 VM 寄せ候補）、
  他は③ state 所有〔並行 CAS〕/ エラー生成 carrier。**

- **2026-06-09 (③ PR-7, §1 catch-all = mutable Buf write methods の VM ネイティブ化 + builtins 降ろし)**: PR-5 と同型で
  `vm_call_method_mut_ops.rs::exec_call_method_mut_op` に `try_native_buf_mut` を追加し、**mutable `Buf` インスタンスへの
  `write-bits`/`write-ubits`/`write-num32|64`/`write-int8..128`/`write-uint8..128`** を VM ネイティブに（`overwrite_instance_bindings_by_identity`
  ＋ `env.insert` ＝ interpreter の instance-mutate ブランチと同一 writeback。エイリアス〔同一 instance id を複数変数が保持〕も
  正しく観測）。**保守的フォールスルー**: type-object 受け手（`buf8.write-...` は fresh buf を返す別経路）/ immutable `Blob`
  （interpreter が "Cannot modify immutable Blob" を投げる）/ malformed arity・offset/bits parse 失敗は interpreter 維持＝
  behavior-invariant。**dedup/placement（[[feedback_dedup_over_perf]]/[[feedback_placement_audit]]）**: 純粋バイト変換を
  builtins へ一本化＝`src/builtins/buf_bits.rs` 新設（`read_bits`/`write_bits` を `impl Interpreter` から降ろし、methods_mut の
  read-bits 重複も解消）＋ `buf_write_num.rs`/`buf_write_int.rs` を `runtime/` → `builtins/` へ移設（22 call-site path 更新）。
  これで Buf バイナリ read/write 変換の authoritative home は `builtins/` に統一され、VM と interpreter が単一実装を共有。
  実測: `read-write-bits.t` の write-bits/write-ubits fallback 3123→3（残3=type-object 形式）。pin `t/native-buf-mut.t`(23,
  エイリアス/buf 伸長/Blob dies 含む)。S03-buf/S03-operators/S32-container/S02-types/signed-unsigned-native 全緑、cargo test 458/0。
  （write-int.t は 128-bit 未対応の既知 pre-existing blocker＝非whitelist、本変更と無関係。）

- **2026-06-09 (③ PR-8, §1 = QuantHash coercion の VM ネイティブ化 + builtins 降ろし)**: フォールバックの**広がり実測**
  （whitelist 145 ファイル・distinct ファイル数）で `new`(63 files・③ ctor) に次ぐ tractable な pure カテゴリ＝
  **`.Set`/`.Bag`/`.Mix`/`.SetHash`/`.BagHash`/`.MixHash`（~11 files）** を確認し着手。`vm_call_method_compiled.rs` の
  catch-all 直前に `try_native_quanthash_coerce` を追加（PR-7/PR-5 と同位置＝ユーザーメソッド解決後なので shadow しない）。
  **dedup/placement 降ろし**: `dispatch_to_set`/`dispatch_to_bag_with_what`/`dispatch_to_mix`（+ helpers `pair_weight`/
  `mix_pair_weight`/`mix_add_item_with_keys`/nested `add_item`/`flatten_into`）を `impl Interpreter` の `&self` メソッド
  （実体は self 非依存・相互再帰のみ）から **`src/builtins/quanthash_coerce.rs` の pure free fn** へ降ろし
  （`to_set`/`to_bag`/`to_mix`）。interpreter の `dispatch_method_by_name_2` と `methods_dispatch_new`（mix_pair_weight）は
  builtins へ委譲＝単一実装。`is_lazy_for_coerce`/`is_lazy_for_set_ops` は他 6 ファイルでも使うので runtime 据え置き
  （`pub(crate)` 化して builtins から参照）。methods_collection.rs 961→315 行。**保守的フォールスルー**: `.MixHash`
  （`register_container_type_metadata`＝interpreter 所有の型メタ登録が必要）/ Instance（`__baggy_data__`・user coercion）/
  Package（型オブジェクト）受け手は interpreter 維持＝behavior-invariant。実測: set-op テストで Set/Bag/Mix fallback → 0。
  pin `t/native-quanthash-coerce.t`(26)。whitelisted set/bag/mix 29 件全緑、cargo test 458/0。（set.t/bag.t の既知 fail は
  BLOCKERS.md 記載の pre-existing〔bag.t 215=BigInt weight・252=Foo instance union, set.t 226=typed-hash bind〕で本変更と無関係。）
  **次候補: iterator protocol（pull-one/skip-one/push-exactly）の VM ネイティブ化、または `AT-POS` native。本丸は `new`（③ ctor）。**

- **2026-06-09 (③ PR-9, §1 = 単純 array-backed Iterator protocol の VM ネイティブ化)**: PR-8 の広がり実測で次点だった
  iterator protocol（pull-one=2466/skip-one=2130 等、4-5 files・高カウント）を着手。`vm_call_method_mut_ops.rs` の
  `try_native_buf_mut` の隣に `try_native_iterator` を追加し、**`items`(Array)+`index`(Int) 自己完結な `Iterator` インスタンス**への
  `pull-one`/`skip-one`/`skip-at-least`/`skip-at-least-pull-one`/`sink-all` を VM ネイティブに（index 前進 →
  `overwrite_instance_bindings_by_identity`(env) + `overwrite_instance_in_locals`(locals) で identity writeback＝interpreter の
  mutating iterator dispatch と同一。エイリアス・sub ローカルスロットも正しく前進）。**重要な発見**: `$it.pull-one` は
  **CallMethodMut にコンパイルされる**（`expr_method.rs:110` 変数受け手は全て CallMethodMut）＝当初 non-mut パスに置いて発火せず、
  mut パスへ移して解決。**保守的フォールスルー**: squish iterator（`squish_source`＝ユーザー `as`/`with` コールバック）/ lazy
  iterator（gather/coroutine＝`is_lazy` 属性、materialized items snapshot でなく interpreter コルーチン pull）/ push-*（外部
  バッファ arg へ array-identity writeback が必要）/ count-only/bool-only（predictive 扱い）は interpreter 維持＝behavior-invariant。
  **gather 回帰を1件発見・修正**: 初版が `is_lazy` 付き iterator も掴み `gather{...}.iterator.pull-one` が IterationEnd 即返し →
  `is_lazy` 除外で解消（除外後は interpreter フォールスルー＝pre-existing 動作）。実測: List/Array/finite-Range iterator の pull-one
  fallback → 0。pin `t/native-iterator.t`(18, エイリアス/sub ローカル/Range 含む)。S07-iterationbuffer/*-iterator 全緑、cargo test 458/0。
  （gather.t の Failed:1 は BLOCKERS.md 記載の pre-existing take-rw〔test 38〕で本変更と無関係。）
  **次候補: `AT-POS` native、coercion 以外の broad な §1、または本丸 `new`（③ ctor）の設計。**

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
