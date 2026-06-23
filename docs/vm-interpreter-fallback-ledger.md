# VM → Interpreter フォールバック台帳

最終ゴール「tree-walking Interpreter 実行パスの撤去 → dual-store 削除」（[PLAN.md](../PLAN.md) ①〜⑤）の
**進捗台帳**。VM (`src/vm/`) が今も Interpreter (`src/runtime/`) に委譲しているサイトを 1 箇所ずつ列挙し、
撲滅のたびに行を消す。関連: [vm-interpreter-dedup.md](vm-interpreter-dedup.md)（重複削除）、
[vm-single-store.md](vm-single-store.md)（locals↔env 一本化の**現行設計** = Slice F）、
[vm-dual-store.md](vm-dual-store.md)（同・撤回試行の履歴）、[vm-decoupling.md](vm-decoupling.md)（dispatch）。
個々のフォールバック撲滅は今や Slice F（単一ストア収束）の前段ステップとして理解する。

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
| ~~`vm_call_method_mut_ops.rs` array-backed instance~~ | ~~`is Array` storage の push/pop/shift~~ | — | **✅消化 (#3058)**: `native_array_storage_mut` で `is Array`-backed instance の backing storage への push/pop/shift/unshift/append/prepend を VM ネイティブ化。`write_back_array_storage_instance` で instance 再構築。richer メソッド（join/sort/map/splice/AT-POS 等）は interpreter 維持 |
| `vm_data_ops.rs` shared push | `@a.push` (threaded) | HARD | lever B（共有セル所有） |
| `vm_data_ops.rs` shaped push | shaped 配列 push | MEDIUM | shaped 次元メタ検査の VM 化 |
| `vm_data_ops.rs` non-simple push | 非Array/非ContainerRef な ArrayPush ターゲット（cold） | LOW | **probe で cold 確認 (2026-06-14)**: ArrayPush opcode は単一引数 push かつ *local* array 限定で emit。closure-captured / multi-arg push は CallMethodMut へ流れ **#3060 で native 化済**（下記）。残る ArrayPush 非Array 分岐は whitelist で発火例ゼロ＝cold |
| ~~`vm_call_method_mut_ops.rs` CallMethodMut push~~ | ~~closure-captured / multi-arg `@a.push`~~ | — | **✅消化 (#3060)**: `try_native_array_mut` に `push` arm を追加。`@a.push(x)` は単一引数 local のみ ArrayPush opcode、それ以外（captured / multi-arg）は CallMethodMut で来るのを VM ネイティブ化。typed/shaped/lazy/shared/constrained は interpreter 維持 |
| ~~`vm_smart_match.rs` key-method~~ | ~~smartmatch のキーメソッド抽出~~ | — | **✅消化 (PR2)**: 統一 compiled-first へ |
| ~~`vm_call_method_compiled.rs` QuantHash coercion~~ | ~~`.Set`/`.Bag`/`.Mix`/`.SetHash`/`.BagHash`/`.MixHash`（list-like 受け手）~~ | — | **✅消化 (③ PR-8 + MixHash slice)**: `try_native_quanthash_coerce` で VM ネイティブ。pure 折り畳みは `builtins/quanthash_coerce` に一本化。**`.MixHash` も追加**（旧除外理由「型メタ登録＝interpreter 所有 state」は #2952 のコンテナ値埋め込み化で stale＝`Value::Mix` の Arc にメタ埋め込みで pure value op・新 `to_mixhash`）。Instance/Package 受け手のみ interpreter 維持 |
| `vm_call_helpers.rs` hyper temp | temp-bind した item への hyper メソッド | MEDIUM | 第一級コンテナ Phase 2 |
| ~~`vm_register_ops.rs` react loop~~ | ~~`run_react_event_loop[_drain]` / `run_whenever_with_value`~~ | — | **✅消化 (Stage 1+2, #3010/#3027/#3029/#3031/#3038/#3039)**: 駆動ループの **4 箇所二重化**（react/await-promise/tap×2）を単一エンジンへ統合（tap×2→1 #3010、react↔await-promise を `SupplyDrivePolicy { React, Promise }` + `drive_react_subscriptions` へ #3027）。**Stage 2 = ループ所有権の逆転完了**: `run_react_event_loop`/`drive_react_subscriptions`/`run_react_consumer`/`replay_static_supply` を `impl Interpreter`→`impl VM`（新 `vm/vm_react_loop.rs`）へ移設 #3038、whenever body/LAST/CLOSE callback を `call_sub_value`(tree-walk)→`VM::call_react_callback`（`vm_call_map_block` でトピック束縛しつつコンパイル済みバイトコード実行）へ #3039。await/Promise 経路は薄い `Interpreter::drive_react_subscriptions` ブリッジ（mem::take/VM/restore）で到達。**Stage 3 follow-up = supply `QUIT` handler も VM ネイティブ化**: VM 駆動ループ内の 2 箇所（`vm_react_loop.rs` の React/Channel quit 経路）が `self.interpreter.call_supply_quit_handler`（tree-walk `call_sub_value`）へ戻っていたのを、`VM::call_supply_quit_handler`（`call_react_callback` 経由でコンパイル済みバイトコード実行）へ差し替え。**これで駆動ループのどのコールバックも tree-walk へ戻らない**。`Interpreter::call_supply_quit_handler` は Interpreter 単独の on-demand/tap supply 経路（`native_supplier_methods`/`native_supply_mut_methods`）からのみ使用（それ自体が別系の tree-walk runtime で、ここは対象外）。`supply_emit_buffer` は Interpreter field のまま（`self.interpreter.` 経由で VM から到達でき、global 化は不要）。 |

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

- **2026-06-09 (③ PR-10, §1 catch-all = plain `@`-array `splice` の VM ネイティブ化)**: PR-5 の
  `try_native_array_mut`（append/prepend/unshift/pop/shift）の姉妹として `vm_call_method_mut_ops.rs` に
  `try_native_array_splice` を追加し、**plain untyped `@`-array（`ArrayKind::Array`）への `splice` の単純形**を VM
  ネイティブに。interpreter の `methods_mut.rs` の `splice` 枝と同一の `drain`+`insert`（削除要素を `Value::real_array`
  で返す）＋ `Arc::make_mut` writeback＋make_mut 再確保後の stale type-metadata 防御除去で behavior-invariant。
  **保守的フォールスルー**（`None`→interpreter）: offset/count が plain 非負 `Int` 以外（WhateverCode/`Whatever`/
  `Str`/`Num`）・offset 範囲外（`X::OutOfRange`）・count 負（`X::OutOfRange`）・lazy 置換（`X::Cannot::Lazy`）・
  typed（`var_type_constraint` Some）/ shaped / shared（`shared_vars_active`）/ metadata 持ち配列。**実証**:
  splice_check で 16 形全て raku 一致、`@j := @i` エイリアスは **interpreter splice 経路（WhateverCode offset で
  fallthrough）も同じく `[1 2 3 4]`** ＝ pre-existing な container-identity ギャップ（🟣第一級コンテナ Phase 2）で
  splice 固有でなく挙動不変。pin `t/native-array-splice.t`(28, 置換平坦化・末尾 splice・count クランプ・独立 removed
  list・fallthrough X::OutOfRange/X::Cannot::Lazy 含む)。`S32-array/splice.t` の untyped 領域（最初60サブテスト）
  not-ok ゼロ・push/pop/shift/unshift/S03-binding/arrays/S09-multidim/methods 全緑、cargo test 458/0、make test PASS。
  （splice.t 全体は非whitelist の pre-existing fail＝`array[int]`/`array[int8]` typed-array メタデータ問題で、native
  経路は typed array で必ず bail＝interpreter 維持のためカウント不変。）

- **2026-06-10 (③ PR-11, §1 = native default construction を typed `$` 属性へ拡張)**: §1 catch-all 最大カテゴリ
  `Package.new`（コンストラクタ）の native 化範囲を拡張。`is_native_default_constructible`/`build_native_default_instance`
  （`methods_object.rs`、VM の `try_compiled_method_or_interpret` と interpreter の `dispatch_new` が共有）が従来
  **untyped `$` 属性のみ**だったのを、**simple class 制約付き `$` 属性**（`has Int $.x` 等）へ拡張。**保守的フォールスルー**で
  divergence を全て interpreter に委ね behavior-invariant に保つ: ① 提供値が型不一致（`!type_matches_value`）→ None
  （interpreter が `X::TypeCheck::Assignment`／coercion）、② arg も default も無い typed 属性 → None（未初期化 typed 属性は
  **型オブジェクト**＝`Int` であり Nil でない。合成は interpreter）、③ typed default の値が型不一致 → None。ゲートは
  **native/coercion/parametric 型を除外**（`is_simple_native_ctor_constraint`＝大文字始まり・`(` `[` なし）し、native
  lowercase（`int`/`num`/`str`、default 0/""）は interpreter 維持。**重要な落とし穴を roast が捕捉**: 型制約の source of truth は
  `ClassAttributeDef` tuple の制約スロットではなく `attribute_types` map（tuple 側は None）＝当初 tuple を読んで型チェックが
  効かず `Int $.x = "str"` を受理する回帰 → `collect_attribute_type_constraints` 由来に修正。さらに **`is built` トレイト／MOP
  `Attribute.set_build`**（カスタム build closure。`attribute_built` map ＋ registry `attribute_build_overrides`）を持つクラスは
  pure-data 構築不可なのでゲートで除外（`S12-attributes/defaults.t` の set_build 実行時型チェック回帰を捕捉・修正）。
  pin `t/native-ctor-typed-attrs.t`(30, 提供値/型オブジェクト未初期化/Str/Real/mixed/継承/相互参照 default/subset 型/
  型不一致 dies/native int は interpreter/ループ構築)。S12/S14/S03-binding/roles whitelist 184 件全緑、cargo test 458/0。
  **残: typed `@`/`%` 属性・required・where・coercion 型・native 型は依然 interpreter（本丸 ③ env 実行の手前で止める保守設計）。**

- **2026-06-10 (③ PR-12, §1 = native construction を untyped `@`/`%` 属性へ拡張)**: PR-11 の続きで native default
  構築を **untyped `@`/`%`-sigil 属性**（`has @.items`/`has %.map`）へ拡張。**behavior-invariance の対象は raku ではなく
  現 interpreter**（実測で `items => 5`→`5`〔scalar を wrap せず〕、typed `@.nums` の不正要素→NODIE〔構築時に要素型
  チェックしない〕＝raku と異なる pre-existing 挙動）。interpreter 共有の `coerce_attr_value_by_sigil` を再利用するので
  提供値 coercion（List/Range→Array, array-of-Pairs→Hash）が完全一致。未提供 default = 空 Array/Hash（型オブジェクト
  合成不要＝`$` typed より単純）。**保守的フォールスルー**: typed 要素（`Int @.nums`＝container type metadata 登録が
  必要・Arc-keying ハザード）/ `is Type` トレイト（`class_attribute_is_types`）/ **default_expr を持つ `@`/`%`**（shape は
  default に encode＝`has @.a[2]`、提供有無に関わらず fallthrough）は interpreter 維持。**roast が shaped 落とし穴を捕捉**:
  当初「default 持ち＋未提供」のみ fallthrough としたが、shaped `@.a[2]` が**提供された**場合 coerce で shape 喪失
  （`S12-introspection/attributes.t` の `.a.shape`=(2,) 回帰）→「`@`/`%` が default_expr 持ちなら無条件 fallthrough」へ修正。
  pin `t/native-ctor-array-attrs.t`(27, 空 default/list・array・range 提供/hash・pairs 提供/mixed typed-$/継承/defaulted
  fallthrough/typed-element fallthrough/ループ構築)。S12/S14/binding/roles 184 件全緑、cargo test 458/0、make test PASS。
  **残: typed `@`/`%`・`is Type`・shaped・required・where・coercion 型・native 型は依然 interpreter。**

- **2026-06-10 (③ PR-13, §1 = native construction を `is required` 属性へ拡張)**: PR-11/12 の続きで native default
  構築を **`is required` 属性**へ拡張（従来ゲートが `!is_required` で reject）。提供された required 属性は native 構築
  （`$` typed は型チェック込み）、**未提供の required 属性は fallthrough**（interpreter が `X::Attribute::Required`）。
  ゲートから `!is_required` を外し、build 先頭に「required かつ未 provided → None」検査を追加。**behavior-invariant**:
  required `$` 未提供は interpreter が die（raku 一致）、required `@`/`%` 未提供は interpreter が **enforce しない**
  （NODIE＝raku と異なる pre-existing ギャップだが native は fallthrough で interpreter 挙動に一致）。pin
  `t/native-ctor-required-attrs.t`(14, 全 required 提供→build / 未提供 `$`→dies / 型不一致→dies / required `@`/`%`
  提供→build / 継承 required)。S12/S14/binding/roles 184 件全緑、cargo test 458/0、make test PASS。
  （pin 作成中に **連続 bare block `} { ` のパース不可**と **クラス名 `Q` のクォート演算子衝突**という 2 つの
  pre-existing パーサ制約に遭遇＝テストをトップレベル構造＋非衝突名に変更して回避。本 slice とは無関係。）
  **残: where・coercion 型・native 型・typed `@`/`%`・`is Type`・shaped は依然 interpreter。**
- **2026-06-14 (③ ctor, §1 = native construction を定義性 smiley `:D`/`:U`/`:_` 属性へ拡張)**: TWEAK(#3028)/
  where(#3030)/BUILD(#3032) に続く ctor slice。従来ゲートが `attribute_smileys.is_empty()` で reject していた
  `has Int:D $.x` 等を native 構築。ゲートから smiley 検査を外し、build path で `enforce_attribute_smiley_constraints`
  （full path と同一ヘルパ）を **where と同じ位置＝post-assembly/pre-BUILD で 1 回 + post-BUILD で再チェック**。
  **interpreter baseline に厳密一致（raku ではなく）**: ① `:U` 提供 defined / `:D` 提供 undefined は die
  （"default value of attribute" メッセージ＝interpreter の既存挙動。raku は "assignment to" だが本 slice の対象外の
  pre-existing 差）、② **BUILD が smiley 違反 → die（post-BUILD 再チェック、full path 3929 に一致）**、
  ③ **TWEAK が smiley 違反 → die しない**（interpreter は post-TWEAK で smiley を再チェックせず代入時チェックも
  しないため、native も再チェックしない＝baseline 一致。raku は die）。bare `:D`（default 無し・required 無し）は
  parse time に `X::Syntax::Variable::MissingInitializer` で弾かれ構築に到達しない。pin
  `t/native-ctor-smiley-attrs.t`(21, `:D`/`:U`/`:_` × default/required/provided/継承/mixed/BUILD-dies/TWEAK-lives/
  Str:D)。S12-attributes/smiley.t(54) + S12-class/attributes-required.t(11) 含む構築系 whitelist 118 件全緑、
  cargo test 461/0、make test PASS。**残: coercion 型・native 型・typed `@`/`%`・`is Type`・shaped・同名再宣言・
  does-Role 属性・CUnion・custom BUILDALL/new は依然 interpreter。**

- **2026-06-14 (③ ctor 第2波 = post-assembly フェーズ方式, #3028〜3036)**: 「pure-data only」原則を破り、pure-data
  組み立て後に **submethod 実行 / 制約検証 / role mixin を post-assembly フェーズ**として走らせて user-code-running shape も
  native 化。`.new` 本経路の各フェーズを共有ヘルパに抽出（`run_tweak_phase`/`run_build_phase`/`enforce_attribute_where_constraints`/
  `apply_attribute_does_role_mixins`）し interpreter/native 両方から呼んで byte-identical 保証。**TWEAK(#3028)**（`TWEAK(:$y)`
  引数渡し＝コンストラクタ2系統で扱い違い）→ **where(#3030)**（TWEAK 前後両方で enforce＝assignment-time セマンティクス）→
  **BUILD(#3032)**（`fail`→Failure / カスタム BUILD で named-arg 自動代入抑制 / positional 拒否＝`S.new("pos")` pre-existing バグ
  も修正）→ **is-rw(#3034)**（`ClassAttributeDef` pos3=is_rw を is_required と誤読していたバグ修正、未提供 is-rw が native 化＋
  `@`/`%` required 強制も正しく）→ **does-Role(#3036)**（role を属性値に mixin、raku 非準拠の文書化済み近似なので native==interpreter
  で検証）。各 pin テスト（`t/native-{tweak,where,build,isrw,doesrole}-construct.t`）。S12/S14 whitelist 全緑、make test PASS。
  **★再計測（重要）**: ctor 5スライス後も whitelist sample の `new` method fallback はほぼ不変（5230→5225）。**大半（4686）は
  単一テスト `S03-buf/read-write-bits.t` の `Buf.new`＝組み込み型コンストラクタ**（`is_native_default_constructible` は user 定義
  `registry().classes` のみ対象なので正しく対象外）。**＝ユーザークラス ctor native 化は完遂。これ以上 ctor を削っても `new`
  fallback 数は動かない。** 次の実質ターゲットは別カテゴリ: **組み込み型 `.new`**（Buf.new 等）/ **`name`=3257（MOP）** /
  **`op`=1418（Routine 値 lexical &-var `&infix:<(|)>` 束縛、今回の Sub/WeakSub 限定 fix の除外分）** / iterator push-* protocol
  （Track B）/ coercion（builtins 降ろし）。残る ctor 難ケース: required-after-BUILD / 未設定 class-typed + BUILD / `is built`/
  MOP set_build / BUILDALL / custom new / CUnion。
- **2026-06-23 (§1 = `.MixHash` coercion の VM ネイティブ化)**: §D（状態所有）の実測駆動スライス。`MUTSU_VM_STATS` で
  catch-all の支配カテゴリを計測したところ、PR-8 が **明示的に除外**していた `.MixHash`（QuantHash coercion の唯一の
  残り）が最頻だった。PR-8 の除外理由は「`.MixHash` は container type metadata を登録＝interpreter 所有 state」だったが、
  **これは #2952〜2985 のコンテナ値メタ埋め込み化で stale**＝`.MixHash` が生む `Value::Mix` の型メタ（`value_type=Real`/
  `declared_type=MixHash`）は **interpreter 副テーブルでなく Mix の `Arc<MixData>` に埋め込まれる**（`tag_container_metadata`
  の `embed_type_info!` 経路。副テーブル `register_container_type_metadata` を踏むのは Instance 等の `other =>` 枝のみ）。
  ∴ `.MixHash` は env/state を一切触らない pure value op で、`.Set`/`.Bag`/`.Mix`/`.SetHash`/`.BagHash` と同型に native 化可能。
  新 pure fn `builtins::quanthash_coerce::to_mixhash`（`to_mix(_, "MixHash")` → mutable flip + メタ埋め込み）を VM の
  `try_native_quanthash_coerce` に追加（method match に `"MixHash"` 追加）。**dedup**: interpreter の `dispatch_method_by_name_2`
  の `Mix|MixHash` 分岐を `to_mixhash` 委譲へ（`tag_container_metadata` 直書きを撤去）＝**1 操作 = 1 実装**。受け手ゲートは
  既存 5 兄弟と同一（list-like のみ native、Instance/Package は interpreter 維持）。変数受け手（`%h.MixHash`）は
  `CallMethodMut` ルーティングで mut パスに行き 6 兄弟全て一律 fallback＝**既存挙動と完全 parity**（将来 mut パスへ
  まとめて降ろす別スライス候補）。挙動不変（native==interpreter＝同一 `to_mixhash`）。pin `t/native-mixhash-coerce.t`(22,
  raku/mutsu 双方 PASS・Str 要素で `.WHICH` キー差を回避)。mix.t 244/244・set.t・categorize 緑。
  （bag.t 625 "Foo instance union" は BLOCKERS.md 記載の pre-existing で本変更と無関係。）
- **2026-06-23 (§1 = QuantHash coercion を mut パスでも VM ネイティブ化)**: 上記 MixHash slice の follow-up。**変数受け手**
  （`@a.Set`/`%h.MixHash`）は `CallMethodMut` にコンパイルされ非mut パスの `try_native_quanthash_coerce` を踏まず、
  6 兄弟（`.Set`/`.Bag`/`.Mix`/`.SetHash`/`.BagHash`/`.MixHash`）が一律 mut catch-all で interpreter fallback していた
  （MixHash slice の「parity」注記参照）。`vm_call_method_compiled.rs` の **mut catch-all**（`try_compiled_method_mut_or_interpret`
  末尾・`try_native_first` の後）に同じ `Self::try_native_quanthash_coerce` を追加。coercion は**新しい** Set/Bag/Mix 値を返し
  受け手変数を変異させない（writeback 不要）ので非mut パスと完全同型＝挙動不変。Instance/Package 受け手は fall through。
  実測: `@a.{Set,Bag,Mix,SetHash,BagHash,MixHash}` の fallback 6→0（残 `^name` は MOP）。pin
  `t/native-quanthash-coerce-mut-path.t`(19, raku/mutsu 双方 PASS)。mix.t 244/244・set.t・categorize 緑。
  （bag.t 252 / classify.t 40〔junction classify・非whitelist〕は stash 確認で main でも fail＝pre-existing・本変更と無関係。）

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
