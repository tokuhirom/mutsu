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
| `vm_call_method_compiled.rs` native-method (非mut) | Instance の native(Rust)メソッド | MEDIUM | native メソッドを VM から直接呼ぶ経路。lever A 系 |
| `vm_call_method_compiled.rs` catch-all (非mut) | generic Instance/Buf/Failure メソッド | HARD | ③ state 所有移管（残る主 tree-walk） |
| `vm_call_method_compiled.rs` native-method (mut) | 同上 mut | MEDIUM | 同上 |
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
| `vm_call_func_ops.rs` builtin-shadow | builtin を上書きするユーザ sub | MEDIUM | ② |
| `vm_call_func_ops.rs` multi-dispatch | ユーザ multi 候補優先 | HARD | VM 側 multi-candidate 解決 |
| `vm_call_func_ops.rs` final else | EVAL 以外の tree-walk 関数 | HARD | ③（else 分岐のみ真フォールバック。carrier は別） |
| `vm_call_dispatch.rs` catch-all | `call_function_compiled_first` 末端 | HARD | ③ |
| `vm_var_get_ops.rs` 0-arg term | 0引数のユーザ/multi 関数 term | MEDIUM | ② |
| `vm_var_get_ops.rs` pkg-qualified | `Module::func` を term 位置で | MEDIUM | ② |
| `vm_dispatch_helpers.rs` Routine call_function | Routine 値の関数解決（method 部は消化済） | MEDIUM | ② レジストリ / multi 解決 |

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

## 撲滅の順序（PLAN.md ①〜⑤ に対応）

1. EASY/MEDIUM な §1/§2 の個別撲滅（native-method 直呼び、Routine dispatch、0-arg/pkg-qualified term、
   array-backed instance — うち container 依存は Phase 2 と合流）。
2. ② 宣言レジストリ（class/role/enum/subset/sub/token）の VM 所有化。
3. ③ env/型検査/readonly/let の VM 所有移管（最大の山。catch-all 群はここで消える）。
4. ④ §C carrier の最終確定（所有が VM に移れば単なる共有参照）。
5. ⑤ `env_dirty`/`saved_env_dirty`/`ensure_locals_synced`/`sync_locals_from_env` 削除。
