# PLAN.md — mutsu 2026年ロードマップ

## 方針

**実用的な Raku インタープリタ**として使ってもらえる品質を目指す。
起動 25 倍速い強みを活かし、CLI ツール・スクリプティング用途をメインターゲットとする。
最終的には **mutsu でウェブブログが作れる** レベルのライブラリ互換性を実現する。

### 🚫 鉄則: インタープリタの重複実装を決して許さない（ユーザー方針 2026-06-07）

mutsu には**2つのエンジン**がある — バイトコード VM（`src/vm/` ＋ pure native `src/builtins/`）と、
レガシーな tree-walking Interpreter（`src/runtime/`）。同じ Raku 操作（builtin 関数・メソッド・演算子・
coercion）が**両方に二重実装されている**ことが最大のメンテナンス負債である。実害は perf ではなく:
**AI/人が調査時にどちらが正かわからず惑わされる・間違った方を直す・片方だけ直してもう片方を放置して
drift する**。

したがって本プロジェクトの第一原理は **「1 操作 = 1 実装」**:

1. **新規実装・修正は VM/native 層に 1 回だけ書く。** 同じ処理を Interpreter 側に二度書かない。
2. **Interpreter が同じ処理を必要とする経路**（EVAL / 正規表現の埋め込み `{}` ブロック / carrier）は、
   単一の native 実装に**委譲**する（`Interpreter::call_function` の catch-all →
   `call_function_fallback` → `crate::builtins::native_function`、または共有ヘルパ）。**再実装しない。**
3. **重複が見つかったら native を authoritative にして Interpreter 側のコピーを削除する。**
   手順・優先マップ・落とし穴は [docs/vm-interpreter-dedup.md](docs/vm-interpreter-dedup.md)。
4. レビュー観点: PR が `src/runtime/` に既存 native と重なるロジックを足していないか必ず確認する。

これは下の「🔴 最優先: VM decoupling」と同じ目標の**言い換え**（decoupling = 重複の解消）であり、
測り方を「フォールバック率」ではなく「**残っている重複実装の数**」に置く。

過去の実装状況は [news/](news/) を参照。
パフォーマンス詳細は [PERFORMANCE.md](PERFORMANCE.md) を参照。
roast ブロッカー分析は [TODO_roast/BLOCKERS.md](TODO_roast/BLOCKERS.md) を参照。

---

## 🔴 最優先: 重複実装を消す（＝ tree-walking Interpreter の廃止 = VM decoupling）

**roast を1件ずつ潰すより、上記「鉄則: 1 操作 = 1 実装」を達成すること（＝ Interpreter の重複実装を
全廃し VM/native を唯一の実装にすること）を最優先する**（ユーザー方針 2026-06-03 / 重複削減として再定義 2026-06-07）。

VM は今も Interpreter を共有実行状態 + フォールバック先として使う（[ANALYSIS.md](ANALYSIS.md) §1）。
**strangler-fig** で段階的に剥がす。進捗台帳: [docs/vm-interpreter-dedup.md](docs/vm-interpreter-dedup.md)（重複解消の手順・マップ）、
[docs/vm-decoupling.md](docs/vm-decoupling.md)（dispatch）、[docs/vm-dual-store.md](docs/vm-dual-store.md)（locals↔env）。
CI（`make test` + 包括的 `make roast`）が全マージをゲートするので大胆にやり CI を安全網にする
（CLAUDE.md「Refactor boldly」）。

### 地ならし（完了。詳細は news/2026-06.md）
重複削除を安全にするための土台は完了済み:
- **レバー A（ディスパッチ native 化、実質完了）**: sprintf/accessor/.new/.map/.grep/.subst/.sort（method）/
  Test ディスパッチ層を native 化、TAP 状態を `TapState` 化 (#2659)、EVAL/pseudo-package を carrier 分類。
  普通のコードでの interpreter フォールバックは実測ほぼ 0%。
- **レバー B（locals↔env 二重ストア解消、実質完了）**: 全パスを scoped/overlay env に変換し dirty 機構を撤廃、
  dispatch 4経路 + smartmatch の per-call pull を撲滅。`env_dirty` フラグは**残るが「interpreter ブリッジの
  安全網」専用**に縮小済み — ブリッジ（＝ Interpreter 実行パス）を消せばフラグも自然消滅する（下記）。
- **レバー C（クロージャ upvalue 化）**: スライス 1/2/2b/3/3b 完了。**非ループ兄弟クロージャ共有**も Phase 1
  第1スライスで解消（≥2 個の兄弟クロージャに捕捉される local だけを `ContainerRef` 化する escape-aware signal。
  詳細は [docs/container-identity.md](docs/container-identity.md)）。残るは**単一の脱出クロージャ**1 件（別 signal
  が要るので Phase 1 の次スライスへ）。

### 本丸: 重複実装カタログの消化 ＝ ✅ 完了（詳細は [news/2026-06.md](news/2026-06.md)）

列挙された重複実装（測り方 = 残っている重複実装の数）はすべて消化済み。残り `[ ]` ゼロ:

- **Category A — 純粋値 builtin**（#2714 ほか）: `abs`/`lc`/…/`chars`、`sign`/`ords`/`chrs`/`unival`/`univals` の
  Interpreter コピーを削除し native へ委譲。純粋値 builtin の重複ゼロ。
- **Category B — genuine fork**（#2727/#2728/#2730/#2731/#2733/#2734/#2735/#2739）:
  sort/min/max/minmax/first（ブロック系 = orchestration を engine 非依存実装に抽出、ブロック呼びだけ trait 差し替え）、
  elems/flat/join/reverse（lazy-fork = 単一実装へ委譲）。
- **Category C — method/arith/coercion**（#2719/#2723/#2743/#2744/#2745/#2747）:
  Phase 1a/1b（arith・coercion bridge）、Phase 2（reduction operator 本体）、Phase 3（sort ラッパ統合＋`dispatch_sort`
  削除、comb 降ろし、substr 4→1、`.first` pointy 修正）、配置監査（split は模範）。
- **正規表現 validator/matcher §3.1**（#2750）: `src/regex_validate.rs`（1108 行）削除、構造パーサ `parse_regex` に
  一本化、`RegexParseMode { Match, Validate }` 導入。

安全削除の必須手順（今後も適用）: native_function に arm がある（必要条件）だけでは不十分 — **EVAL 経路で同値確認**
（`mutsu -e 'say EVAL(q{f(...)})'` が raku と一致）してから削除し `make roast` で確認。手順・落とし穴・優先マップは
[docs/vm-interpreter-dedup.md](docs/vm-interpreter-dedup.md)。

**教訓（実証）**: ①重複は drift してバグの温床 — `[%] 2**70` 精度・`[~]` NFC・`[minmax]` ネスト・`%h.first` の named
束縛・`elems("hello")`・`flat` over-flatten・`join(sep,Range)`・`reverse()`=Nil・`.first(-> $p)` pointy 未束縛は全て
dedup 作業中に発見・修正。②「1 操作 = 1 実装」に加え**「単一実装が正しいレイヤに在るか」**も問う（comb の純粋分割を
interp から降ろした。WhateverCode/regex 結合な部分は `runtime/` に残すのが正しい）。

### 最終ゴール（現・主作業）: Interpreter 実行パスの撤去 → dual-store 削除

重複カタログ（本丸）完了に伴い、主作業はここへ移った。VM はいまも Interpreter を**共有実行状態 ＋
フォールバック先**として使う（[ANALYSIS.md](ANALYSIS.md) §1.1: `self.interpreter.*` 参照 1300+、内訳上位は
`env()`/`env_mut()` ~480、`type_matches_value`、`var_type_constraint`、`current_package`、`restore_let_saves`、
`readonly_vars_mut`…）。これを VM 所有へ移し切り、真の tree-walk フォールバックを撲滅するのが最終ゴール。
完了すると **env を任意名で書く唯一の存在＝interpreter ブリッジが消え**、`env_dirty`/`ensure_locals_synced`/
`sync_locals_from_env`/`saved_env_dirty` の **dual-store 機構を削除**できる（レバー B 完遂）。

進捗台帳: [docs/vm-interpreter-fallback-ledger.md](docs/vm-interpreter-fallback-ledger.md)。

#### 完了済み（詳細は [news/2026-06.md](news/2026-06.md)）

- [x] **① 真フォールバックの可視化 + 安いルーティング消化**（#2755〜）: 全サイトを `// TODO: compile to bytecode`
      （真フォールバック）/ `// CARRIER:`（反射・MOP・EVAL・メタプロ hook）で注釈し台帳を新設。「生ディスパッチを
      統一エントリへ降ろすだけ」の安いサイトは**枯渇**。
- [x] **② 宣言レジストリの VM 所有化（PR-A/B/C, #2760-2772）**（設計: [docs/vm-registry-ownership.md](docs/vm-registry-ownership.md)）。
      全宣言レジストリを `Arc<RwLock<Registry>>` 足場へ抽出 + read/MRO/型マッチを `impl Registry` メソッド化 +
      write-through 整理 + 再入跨ぎ guard を debug 実行時 guard で強制。plain field への最終畳み込みは Interpreter 撤去後。
- [x] **③ の pure-data カテゴリ消化（PR-1〜10 + ctor #2826-2844）**: Routine/builtin-shadow/multi dispatch 統一、
      array mutators / Buf write / QuantHash coerce / Iterator protocol / splice の native 化、`Package.new`
      コンストラクタを属性 shape 別に native 化（typed/untyped `$`・`@`/`%`・native 型・coercion 型・required）。
      **§1/§2 の「pure-data で降ろせる」カテゴリは枯渇**。

#### 現状認識（2026-06-10）: 残るフォールバックは 3 つの独立な構造ブロッカー前提

台帳に残る §1/§2 の真フォールバックは、すべて以下のいずれかが前提であり、個別ルーティングでは消えない。
そして **この 3 つは別サブシステム（ディスパッチ/状態・データ表現・並行）なので互いに独立 ＝ 並列実行できる**:

| ブロッカー | これが塞いでいる台帳フォールバック |
|---|---|
| **A. ③ state 所有 + 残ディスパッチ** | §1 native-method（IO::Handle/Pipe）・catch-all（native/Buf/Failure 受け手）、§2 catch-all（lexical `&`-var 名呼び / `__mutsu_*` / no-match エラー生成） |
| **B. 第一級コンテナ Phase 2（要素セル）** | §1 array-backed instance mut・shaped push・non-simple push・hyper temp（＋ take-rw / `@a[0]:=` / 深い `>>++`、Q2 の Arc-ptr flaky） |
| **C. 並行 / lever B（共有セル）** | §1 shared push（threaded `@a.push`）・react loop（`run_react_event_loop`）（＋ ANALYSIS §2.3 unsafe aliasing） |

#### Phase I（並列）: 3 トラックで全 §1/§2 tree-walk フォールバックを撲滅

> **依存の核心**: env/型検査などの state フィールドは runtime/ tree-walk に浸透し §1/§2 フォールバック経由で
> 読まれるため、**フィールド再配置（Phase II）は tree-walk 実行パス撲滅後にしかできない**。よって「env を VM 所有へ
> 移す」より先に、まず全フォールバックを native 化する。A/B/C は独立に並列で進め、3 つ揃ったところで Phase II へ。

- [ ] **トラック A — 残ディスパッチの native 化（③ の dispatch 部分）** ＝ 最長・ペース決定トラック
      - [x] PR-1 Routine dispatch / PR-2 builtin-shadow fork / PR-3 catch-all ユーザーメソッド / PR-4 非proto multi fork
      - [~] **§2 catch-all 末端**:
            - [x] **lexical `&`-var の名前経由呼び（`-> &op {...}` 等）= DONE**: pure lexical（#2949）+
                  shadow 経路（#3021）+ slip 呼び `op(|@args)`（#3022）+ shadow×slip 優先順位バグ修正（#3024）。
                  すべて `vm_call_on_value` へ寄せ、interpreter terminal 依存ゼロ。sigilless callable `f(...)` は
                  raku 仕様上不正（`f.()` は既に native）なので対象外。残: shadow 下クロージャ内 sigilless 名前
                  解決の語彙性（dispatch でなく name resolution の別軸課題）。
            - [ ] `__mutsu_*` 内部（並行は C へ）、no-match エラー生成は carrier 確定。
      - [ ] **§1 native-method dispatch**: Buf/Failure/native 受け手のメソッドを native 実装に（または carrier 確定）。
      - [ ] **§1 native IO**: `IO::Handle`/`IO::Pipe` 等。ファイルハンドル状態を VM 所有 or native IO 層へ。
- [ ] **トラック B — 第一級コンテナ Phase 2（要素セル）** ＝ データ表現（A と完全独立）
      設計・段階導入は 🟣第2優先「第一級コンテナ」セクション参照（Phase 1 = landed）。
      - [ ] Phase 0.5 第2段（スタック不変条件 + lvalue opcode）を Phase 2 と同一 PR で。
      - [~] 配列/ハッシュ要素の COW セル化 → array-backed mut・shaped/non-simple push・hyper temp・
            深い `>>++` を解く（take-rw は #2930、`@a[0]:=`/束縛要素セルは #2902-#2925 で landed）。
            - [x] **whole-container `:=` bind 共有（`my @b := @a` / `my %g := %h`）**: 両変数が単一の
                  共有 `ContainerRef` cell を持ち、push/pop/shift/unshift/splice/index-assign/slice-assign/
                  delete の変異が双方向に伝播（旧実装は inner Arc 共有のみで COW push が detach していた）。
                  GetArrayVar/GetHashVar がトップレベル cell を decont（Phase 0.5 第2段の読み側）、bind 経路が
                  cell を生成、変異 op（native array mut/splice・delete）が `env_root_descended_mut` で
                  cell を descend。`t/whole-container-bind.t`(26)。**残**: スカラー `$ref := @a`（`$`-bind は
                  bind_vardecl=false で WrapVarRef 非 emit → コンパイラ変更が前提・別 PR）。
      - [x] **Q2 の型メタ Arc-ptr flaky の構造的除去 = 完全吸収 DONE（2026-06-12〜13）**:
            Hash = HashData (#2952、original_keys = #2954)、Set/Bag/Mix = 既存 *Data 埋め込み (#2957)、
            **Array = ArrayData wrapper（2026-06-13）— 全 5 コンテナ型の型メタがコンテナ値に埋め込まれ、
            型メタ副テーブルは全廃**。経緯・監査済み hazard（unsafe cast / Arc identity / iterator 共有）=
            docs/hashdata-migration-plan.md Stage 3-4 節。
      - [x] **全 ptr-keyed 副テーブル撲滅 = DONE（2026-06-13）**: 残っていた `Arc::as_ptr`-keyed 副テーブルを
            すべてコンテナ値に埋め込み完了。shaped dims = `ArrayData.shape`、array default = `ArrayData.default`
            （既存）、**grep-view = `ArrayData.grep_source`（#2985）**、**hash default = `HashData.default`**。
            最後のユーザーを失った `PtrKeyedMap` / `ptr_keyed.rs`（#2953 の Weak-guard interim 防御機構ごと）を
            **削除**。これで `Arc::as_ptr` ポインタ identity に依存する副テーブルはゼロ＝Q2 の間欠 flaky の構造的
            根を全廃。レバー C 残（単一脱出/汎用捕捉）は要素セル本体（Phase 2）へ合流。
- [ ] **トラック C — 並行 / lever B（共有セル）** ＝ 並行（A と独立。要素セルは B と共有基盤なので B に弱依存）
      - [~] `clone_for_thread` のスナップショットコピー → 共有すべき lexical/global を `ContainerRef`
            （`Arc<Mutex<Value>>`）ライブセルへ（ANALYSIS §8.3/§2.2）。**スライス 1 LANDED**:
            `start` で捕捉・変異される lexical scalar をエスケープ解析でセル化し、スレッド間で同一 Arc を共有。
            ① `start` 引数を escaping position 化（`compile_call_arg_with_escape`）、
            ② エスケープ信号をネストした非エスケープクロージャ越しに親へ伝播（`needs_cell_free_vars` —
            `map { start { $outer++ } }` のような中間 map ブロック越しの捕捉を解く）、
            ③ `++`/`--` を ContainerRef のロック保持下でアトミック RMW 化（`atomic_container_incdec` —
            並列加算が lost-update しない。raku より決定的）、
            ④ box 時に stale な `shared_vars` スナップショットをセルに張り替え
            （mainline `start` 後の `sync_shared_vars_to_env` writeback がセルを clobber する回帰を防止）。
            `await (^N).map: { start { $c++ } }` が正しく N を返す。テスト `t/concurrent-shared-cell.t`。
            **スライス 2 LANDED**: スカラ `state $n` のスレッド間共有。`StateVarInit` が
            `shared_vars_active` の間、user `state` 変数を `shared_vars` 内の共有 `ContainerRef` セル
            （正規化キー `__mutsu_shared_state::{normalize_state_key(scoped_key)}`）に格納。
            キー正規化は mutsu の二重コンパイル（registered body `&f` と OTF `&f/0`）で分岐する
            `@<ip>`／`/<n>` を剥がし、`start f()` と直接 `f()` が同一セルを引くようにする。
            `clone_for_thread` がスレッド前の親 state を共有セルへ移行（`f();f();start{f()}` を解く）。
            増減は ① のアトミック ContainerRef RMW を流用（決定的）。`StateVarInit` は genuine `state`
            宣言のみに出るので `ff`/`fff`/smart-match の内部 state（`set_state_var` 直接・非セル）は不変。
            テスト `t/concurrent-state-var.t`。
            **スライス 3 LANDED（PR #3059）**: スカラ複合代入（`$x OP= rhs`）のアトミック化。
            融合オペコード `OpCode::AtomicCompoundVar { name_idx, op: CompoundBaseOp }` を
            コンパイラのチョークポイント（`compile_expr_assign`／`Stmt::Assign` 末尾）で
            **平の env 名前付きスカラのみ**に発行（local スロット／twigil は除外＝ホットな
            リテラル `$x = $x + y` own-local ループは融合しない）。非セル書き込みは `++` の
            env 書き戻し末尾を抽出した `store_named_scalar_rmw_result` を `++`/`--` と共用するので
            METHOD captured-outer 伝播が `++` と構造的に同一。ContainerRef セル分岐はロック保持下の
            アトミック RMW。`await (^100).map: { start { for ^100 { $c += 1 } } }` が決定的に 10000。
            **要点**: 融合 rhs は `compile_expr` でコンパイル（`compile_call_arg` はスカラ被演算子を
            itemize/escape-box して captured-outer 伝播を壊す）。テスト `t/concurrent-compound-assign.t`。
            **スライス 4 LANDED（PR #3061）**: スレッド間の**ハッシュ要素代入**（`%h{$k} = $v`）。
            `my %h; await (^50).map: -> $i { start { %h{$i} = $i*$i } }` が 0（スナップショットの
            last-writer-wins で書き込み消失）→ 決定的 50（raku 一致）。`assign_hash_elem_to_shared_var`
            （`shared_vars` ロック保持下で get→`Arc::make_mut`→insert＝`.push` と同じ要素単位アトミック）を
            `exec_index_assign_expr_named_op` 先頭の `shared_vars_active` 早期 return ガード
            （`try_shared_hash_element_assign`）から呼ぶ。単一スレッドは早期 return で挙動不変。
            `@a.push` は元から動作（`push_to_shared_var`）、壊れていたのは `@a[i]=`／`%h{k}=`。
            テスト `t/concurrent-hash-assign.t`。
            **スライス 5 LANDED（PR #3063）**: スレッド間の**配列要素 index 代入**（`@a[$i] = $v`）。
            スライス 4 の配列版。`my @a; await (^50).map: -> $i { start { @a[$i] = $i*$i } }` が 0 →
            決定的 50（raku 一致）。`assign_array_elem_to_shared_var`（ロック保持下で
            get→`Arc::make_mut`→Nil で grow→set）を `try_shared_array_element_assign` ガード
            （非負 Int index・型制約/default/shaped/bound なしの単純ケースのみ）から呼ぶ。
            単一スレッドは早期 return で挙動不変。テスト `t/concurrent-array-index-assign.t`。
            **残**: `state @`/`%`（配列/ハッシュ state）のスレッド共有（要素セル＝Track B 依存）、
            unsafe aliasing 撤廃（ANALYSIS §2.3）。
      - [x] **react/supply ランタイムの VM ネイティブ化 — 完了（Stage 1+2+3, #3010〜#3039, 2026-06、
            詳細は [news/2026-06.md](news/2026-06.md)）**。駆動ループの 4 箇所二重化を単一エンジンへ統合し
            （Stage 1）、ループ所有権を `impl Interpreter`→`impl VM`（`vm/vm_react_loop.rs`）へ逆転して
            whenever body を **コンパイル済みバイトコード**でディスパッチ（Stage 2 #3038/#3039）、`// TODO:
            compile to bytecode` を除去（Stage 3）。**Stage 3 follow-up = supply `QUIT` handler も VM
            ネイティブ化**（`VM::call_supply_quit_handler` を `call_react_callback` 経由で実行）。これで VM
            駆動ループのどのコールバックも tree-walk へ戻らない。台帳の react-loop 行参照。
      - [ ] **unsafe aliasing 撤廃**（ANALYSIS §2.3, `Arc::as_ptr as *mut` 11 箇所）— B の要素セル基盤の上で。

#### Phase II（収束・逐次）: state 移管 → carrier 確定 → dual-store 削除 → Interpreter 撤去

> Phase I で 3 トラックが全フォールバックを撲滅したら、ここは比較的機械的に進む。

- [ ] **③ の本丸: 借用 state を VM 所有へ再配置**（設計: [docs/vm-state-ownership.md](docs/vm-state-ownership.md)）:
      env HashMap・型検査（`type_matches_value`/`var_type_constraint`）・readonly 追跡・`let`/`temp` 復元・multi 解決・
      state 変数・`current_package`。終状態は **plain VM field**（env は単一所有者で同時共有が無く `Arc<RwLock>` は
      最ホットで perf 破綻するため不可）。Phase I 完了後＝tree-walk が state を読まなくなって初めて再配置可能。
- [ ] **④ キャリアの扱いを確定**（消すのではなく分離 or 明示）: `EVAL`/`EVALFILE`（compile→サブ VM 実行）・正規表現の
      埋め込み `{}` ブロック・pseudo-package（`CALLER::`/`OUTER::`）・MOP 反射（`.^*`/`.WHAT`/`.VAR`）・call-chain
      （callsame/nextsame）・Test ディスパッチ。③で所有が VM に移れば、これらは「Interpreter 実行パス」ではなく
      単なる**共有レジストリ参照**になる（`// CARRIER:` のまま据え置き or 明示分離）。
- [ ] **⑤ dual-store 機構の削除（レバー B 完遂）**: `env_dirty`/`ensure_locals_synced`/`sync_locals_from_env`/
      `saved_env_dirty`/`VmCallFrame` の dirty/bind フィールド群を撤去。registry `Arc<RwLock>`→plain VM field 畳み込み。
      **Interpreter オブジェクト自体を削除**。

#### Phase I/II と並行で随時消化できる独立タスク（critical path 外）

- [~] **正規表現のコンパイル済みキャッシュ**（ANALYSIS §8.4）— perf。撤去とは独立。
      static パターンの parse 結果は `REGEX_PARSE_CACHE` で memo 済み。#3064 で**キャッシュヒットを
      owned `RegexPattern` の deep clone → `Arc<RegexPattern>` の refcount bump 化**（毎マッチの
      ツリー clone を除去）。ベンチ `~~ /(\w+) \s+ (\w+) \s+ (\d+)/` ×20000 は release ~0.56s
      （raku 0.36s ＝ ~1.6x、ANALYSIS 当時の 8.6x から大幅改善）。**残るギャップはマッチャ本体**
      （`regex_match_ends_from_caps_in_pkg` が全 end 位置の `RegexCaptures` を構築→sort する
      アロケーション）で、これは別の深い最適化。

関連: 🟣第2優先「第一級コンテナ」＝トラック B の本体（レバー C ＝ Phase 1 の一部、Q2 の Arc-pointer flaky を吸収）。

---

## 🟣 第2優先: 第一級コンテナ (container identity) — **戦略フェーズは完了。残りは機会的な正しさ修正**

> ### ⭐ STATUS / 結論（2026-06-08 実証で確定。本セクションの旧「実装順序」を上書きする）
>
> **基盤は landed**: escape 解析（step 1, #2758）でクロージャ捕捉スカラーが escape-aware に共有セル化され、
> 兄弟クロージャ共有・単一脱出クロージャの正しさバグは解決。bareword `f()` 経路も #2759 で修正。
> **これが本移行の戦略的な勝ち筋であり、入った。**
>
> **「4→1 機構削除」目標は撤回する**（旧 step 2 の前提が誤りと実証された）。`owned_captures` /
> `closure_captured_state` / `box_captured_lexicals` の 3 機構は**冗長ではなく、互いに別の正しさケースを担う**:
> | 機構 | なぜセルで置換できないか（実証） |
> |------|--------------------------------|
> | `box_captured_lexicals` | セル生成は**捕捉時**が正位置。宣言時へ移すと `let`/`temp` 復元を壊す（save がセル Arc を保存し復元不可、`S04-blocks-and-statements/let.t` 回帰）|
> | `closure_captured_state` | **事前コンパイル deserialize 経路で load-bearing**。ContainerRef セルはプロセス跨ぎのシリアライズで保存されず、deserialize 後の per-call 状態はこの副テーブルが担う（`S10-packages/precompilation.t` 回帰、GH2897）|
> | `owned_captures` | **read-only ループ捕捉の per-iteration 値凍結**。read-only 変数は mutated でなくセル化されないので、セルでは代替不可 |
>
> → **セルは「ランタイムの共有変異」は包含するが、(a) 宣言時機構との干渉, (b) シリアライズ, (c) read-only 値凍結
> は包含しない。3 機構はセルを*補完*しており、削除すると正しさが減る。** これらの削除は**もう試みない**
> （いずれも release roast でのみ顕在化＝ `make test` を通過する。再試行は浪費）。詳細は
> [docs/container-identity.md](docs/container-identity.md) と メモリ参照。
>
> **残りの container 正しさ項目（`.VAR.^name` 束縛反映 / `is rw` 3-way 持続 / take-rw / 深い `>>++`）は
> すべて深い plumbing**（rw は writeback 方式で真のセルでない・要素セルは Phase 2…）で、quick win は無い。
> よって**戦略プロジェクトとしては扱わず、機会的バックログへ降格**: 特定の roast ファイルが残り 1–2 subtest で
> 通り、その原因がピンポイントで該当バグのときだけ、その都度・単発で直す（下記「機会的バックログ」）。
> **進捗メトリクスは「機構削除数」ではなく、この領域では「個別 roast 正しさ」に戻す**（削除はもう無いため）。
>
> **戦略的な主作業は 🔴 最優先「インタープリタ実行パス撤去（phase ②: registry の VM 所有化, #2760-）」へ戻す。**
> そちらは機構削除（dual-store・interpreter ブリッジ）が実際に進む、現役の clean-architecture 投資先。

**優先順位**: 🔴 最優先（tree-walking Interpreter 廃止 / phase ②）が現役の戦略作業。本セクションは
**基盤完了＋機会的バックログ**。以下は設計知見・バックログとして残すが、旧「実装順序（4→1 削除）」は上の
STATUS で撤回済み。

実装台帳: [docs/container-identity.md](docs/container-identity.md)（現状の地図・段階スライス・進捗ログ・撤回した試行の記録）。

### 機会的バックログ（その都度・単発で。戦略プロジェクト化しない）

各項目は深い。**ROI が明確なとき**（特定 whitelist 候補ファイルが残り 1–2 subtest で、原因が該当バグ）にだけ着手する。

- [x] **`.VAR.^name` 束縛コンテナ反映 (DONE)** — `my $l := (1,2,3); $l.VAR.^name` を `List`（旧 `Scalar`）に。
      scalar `:=`-bound to a container（`@a`/`%h`/`(1,2,3)`/Set/Bag/Mix）は Scalar コンテナを持たず
      コンテナを直接 alias するため `.VAR` は束縛値自体を返す。`__mutsu_bound_decont` marker を Hash/Set/Bag/Mix/
      ContainerRef へ拡張し、VAR ハンドラ（methods_mut.rs）が marker を見て target を返す。scalar rebind
      （`$r := ...`、VarDecl でないので `__scalar_bind` 無し）も `MarkScalarBindContext` を emit して marker 維持。
      `t/var-name-bind.t`(16)。
- [x] **配列/ハッシュ要素のセル化（COW）= Phase 2 残り (DONE)** — 深い `>>++`・`deepmap(++*)`。
      whole-container `:=` bind 共有（`@b:=@a`/`$r:=@a` #2990/#2993）、nested-element hyper writeback
      （`@b[0]>>++` #2996）、hyper lvalue-precise writeback（`@a>>++` が COW コピーを破壊しない #2999）。
      （`is rw` 共有セル #2928・take-rw #2930・束縛要素セル #2902-#2925 も landed）
- [x] **配列/ハッシュ param の別名束縛（parameter aliasing）= 残る最大の container 正しさ項目 (DONE)** —
      plain `@x`/`%h` positional param を caller に alias 束縛。`@x.push`・`@x[0]=v`・`@x>>++`・`splice`・
      whole-container `@x=(...)`/`%h=(...)` が全て伝播。手法=`is rw`/`is raw` の writeback 機構
      （`apply_rw_bindings_to_env`）へ plain `@`/`%` positional param を載せ、`readonly_vars` から `@`/`%` を除外
      （scalar `$` は readonly 維持）。`is copy` はコピー継続。`t/param-array-alias.t`(15)。
      既知の残り: 同一変数を 2 つの param に渡す `f(@z,@z)`（writeback 上書き、main から非回帰）と `return-rw @x` 越し
      の live alias は writeback では非対応（cell 化が必要・極めて稀）。設計メモ=`docs/param-binding-cells-plan.md`。
- [x] **named for-loop 変数の別名束縛 + topic writeback O(n²) 修正 (#3008)** — plain（非 `is copy`）named
      for-loop 変数（`for @m -> @row`/`-> $row`/`-> %r`）が要素を alias 束縛（topic と同じ per-iteration source
      writeback 経由、`is copy`/`<->` 除外）。付随して **pre-existing O(n²)** を修正: topic writeback が毎反復
      配列全体を rebuild（`for @big {...}` 40k で ~46s）→ `loop_var_unchanged`（同一束縛なら O(1) skip）で O(n) 化
      （50k で ~73s→~0.8s）。`t/for-loop-named-param-alias.t`(16)。
- [x] **given/with container topic の別名束縛 + read-only topic 意味論 (#3012)** — `given @a { .push }`・
      `given %h { $_<k>=v }` が伝播（`@`/`%` topic を `write_back_given_topic` で source 書き戻し、#3008 の
      `loop_var_unchanged` guard 流用）。`Given` opcode に `topic_readonly` フラグ追加し `given @a { $_=... }`・
      `given 42 { $_=9 }` を raku 同様 die（`given $x` は rw 維持）。`t/given-topic-alias.t`(12)。
- [x] **whole-container `with` topic + element-source topic（given/with）の別名束縛 (#3014/#3015/#3016)** —
      ① **#3014**: `with @a {.push}`・`with %h {$_<k>=v}` が伝播（`use_given_alias` を `@`/`%` に拡張し #3012 の
      given 機構へルート）。② **#3015**: `given %h<k>{.push}`・`given @a[i]{$_=v}` が伝播（新 `OpCode::TagElementSource`
      で element を lvalue topic 化＋`write_back_element_source` で `assign_into_nested_container` 経由 書き戻し。
      **element は lvalue ＝完全 rw**＝whole-container topic の read-only-for-reassign と非対称）。③ **#3016**: `with %h<k>`
      も `use_given_alias` を simple-var target の `Expr::Index` に拡張して #3015 機構へルート。
      `t/with-topic-alias.t`(13)/`t/given-element-topic.t`(16)/`t/with-element-topic.t`(12)。
- [x] **with/given の pointy-param aliasing (#3020)** — `given @a -> @p { @p.push }` / `with @a -> @p { @p[0]=v }`
      （`-> %p` / `-> $p`・element-source `given %h<k> -> @p`・`when`/`default` body・whole reassign `@p=(...)`）が
      source へ伝播。`is copy` は flatten した fresh copy で非伝播。parser が `@p := $_` に desugar→compiler が
      `Given { pointy_param_idx }` に bound 名を載せ→writeback が param の最終値を source へ。`with` は given にルート。
      道中 3 バグ修正（pre-existing readonly leak / element_source 1-shot leak / stale-alias 越境汚染）。
      `t/with-given-pointy-alias.t`(28)。
- [ ] **Track B 残 niche（深い別 feature・ROI 低。詳細 = メモリ `project_track_b_phase2_element_cells`）**:
      ① **`for %h<k>.values{$_*=2}` の element-source rw writeback**（element-source topic 最後の穴）: for-loop は
      `container_binding`（String var名）駆動の per-iteration writeback で、element source は `%h<k>[idx]=$_` の 2 段
      nested writeback ＋ `for %h<k>` の itemization 修正が必要・**最ホット path で多機構が絡み高リスク**（#3008 教訓:
      for-loop writeback は full roast 必須）。② **`.push(@var)` 参照格納**（Raku `**@` slurpy alias、COW detach するので
      cell 昇格＝#2990 機構が必要・hot push path）。
- [~] **object-hash（`%h{KeyType}`）** — キー保存（original_keys）は HashData 埋め込みで **COW-stable 化済み**
      (#2954)。mixin キー・`.new`/`.clone` 維持 (#2956)、multidim Range slice `:exists` (#2959) も landed。
      **キー表示の全経路修正 (#3007)**: `.sort`/`.gist`/`.Str`/`.raku`/`.list`/`.map`/`for`/`@`-flatten/`.min`/`.max`/
      `:k`/`:kv`/`:p` slice が内部 WHICH 文字列（`Int|1`）を漏らしていたのを `HashData::typed_key`/`typed_pair`
      で実キーへ統一。slice adverb の `value_which_key` lookup も修正（object hash で空を返していた）。
      残る `S09-hashes/objecthash.t`（非whitelist）の失敗は**型強制/構築の別系統課題**（互いに独立）:
      ① test 3 = `%h{Any}` の Mu キー拒否 — 根は object-hash でなく **`Mu.new` が `Any` 型インスタンスを生成**
      （`Mu.new ~~ Any` が誤 True、smartmatch 全体に波及・高リスク）。② test 21/23/24 = `Hash.push` 型チェック。
      ③ test 37-62 を塞ぐ list→hash flatten abort = **itemization 追跡が必要**（bare `%h` は平坦化・itemized
      `$h` は不可、mutsu は build_hash_from_items で区別不可 — メモリ `list-to-hash-flatten-itemization`、
      #2958 revert 済）。
- [ ] **属性セル + 属性束縛 = Phase 3** — `$!x :=` / per-attribute container template（S03-binding/attributes,
      S14-traits/attributes 5-8）。
- [x] **型メタを Arc ポインタ keying からセルへ（Q2 項目）= 完全吸収 DONE（2026-06-13）** — Hash #2952/#2954、
      Set/Bag/Mix #2957、Array = ArrayData wrapper。全 5 コンテナ型で型メタ副テーブル全廃
      （docs/hashdata-migration-plan.md Stage 3-4 節 / 上記トラック B 参照）。**残っていた非型メタ ptr-keyed
      （defaults/shaped/grep-view）も全て埋め込み完了し `ptr_keyed.rs`（Weak-guard 機構）ごと削除（2026-06-13、
      grep-view #2985 + hash default）。`Arc::as_ptr`-keyed 副テーブルはゼロ。**

注: 既に通るようになった項目（観測 2026-06-08）— reduce.t 62 の `:=` 束縛リスト平坦化（`@a.elems`=3）、
`is rw` の**基本** persistent（`f($a);f($a)`）は現状 PASS。バックログから外す。

最終ゴールは **世界最高の Raku インタープリタ ＝ 最速かつ最もメンテしやすい** こと。その物差しで、
インタープリタ廃止の次に最大の構造的負債かつ最大の正しさブロッカーは「mutsu が値 (`Value`) を裸で持ち、
Raku の**コンテナ**（`Scalar` / 配列・ハッシュ要素セル / 属性セル）の identity を持たない」こと。これは
BLOCKERS の複数セクションを横断して塞いでおり、roast を個別に潰す作業では絶対に届かない。
**VM decoupling のレバー C 本丸 ＝ Q2 の Arc-pointer-keying flaky ＝ この一点に収斂する。**

### なぜ最優先の戦略課題か（インパクト）

裸 Value モデルが直接ブロックしているもの（一例。単発では「小さな別バグ」に見えるが根は1つ）:

- **束縛 vs 代入**: `my $x := (1,2,3)` がリストとして平坦化されない / `.VAR` が常に `Scalar`
  （reduce.t 62, S02 variables-and-packages 16件、`:=`-束縛コンテナ識別）
- **`=:=` / `.VAR` / itemization**: コンテナ identity が無く `$(...)` と裸値を区別できない
- **`is rw` / `is raw` / take-rw**: 呼び出し側コンテナをエイリアスできない（gather.t 38, S06 各種,
  S12-methods/accessors, S12-attributes/instance）
- **配列/ハッシュ要素の lvalue**: `@a[0] := …`, `>>++` の深い変異, `deepmap(++*)`/`*--`, object-hash
  （hyper.t 330-333, classify.t Junction キー, S03-binding/nested）
- **兄弟クロージャの捕捉変数共有**: `my ($g,$s)=make(); $s(42); $g()` が共有されない（レバー C 本丸）
- **属性へのバインド**: `$!x := …` / per-attribute container template（S03-binding/attributes,
  S14-traits/attributes 5-8）
- **型メタの flaky**: 副テーブルの Arc-pointer-keying（後述 Q2 項目、S02-names-vars/perl.t 等の間欠 die）は
  「コンテナ自身が安定 identity を持たない」ことの裏返し

→ **一度きちんと入れれば 30+ テストと複数の flaky が同時に解け、以後この種の workaround を書かなくて済む。**

### なぜ過去のプロトタイプは失敗したか — "deref everywhere" 問題

「捕捉スカラーを `ContainerRef` に昇格」する素朴な試みは、ローカルでは tests 18-20 を直したが roast を
広く回帰させた（BLOCKERS S02 節）。原因は、値を消費する**全 op**（算術・比較・ディスパッチ・型チェック・
出力・coercion … 数百サイト）が deref を要し、1つでも漏れると `ContainerRef` が値コンテキストに漏れて
誤動作すること。コンテナを「足す」だけのアプローチは消費面が広すぎて破綻する。

### 設計の鍵: deref を「散在」させず「単一チョークポイント」に集約する

Rakudo/MoarVM が実証する解法 ＝ **decont（脱コンテナ）を 1 箇所に集約**する。コンテナは*格納サイト*
（変数スロット・配列/ハッシュ要素・属性）にのみ存在し、*値読み出し*は VM のオペランド取得経路という
**唯一のチョークポイント**で必ず decont する。op はスタックから「既に decont 済みの値」を pop するので、
算術・比較・ディスパッチ op は**一切変更不要**。コンテナが見えるのは、明示的に lvalue/コンテナを要求する
数少ない経路（`:=` bind, `is rw`, `.VAR`, `=:=`, take-rw, itemization 判定）だけ。
これで消費面が「数百の値 op」→「一握りの lvalue op」へ**反転**する（列挙可能で扱える）。これが素朴版との
決定的な違い。

### 設計知見の記録（旧「実装順序」。step 1 は landed、step 2 の「4→1 削除」目標は上の STATUS で**撤回済み**）

> ⚠️ 以下は当初の段階計画。**step 1（escape 解析）は #2758 で完了**。**step 2 以降の「3 機構をセルで置換・削除」は
> 上の STATUS のとおり撤回**（3 機構は非冗長と実証）。設計の鍵（チョークポイント・escape 解析・perf 担保・
> #2746 の轍）は今も有効な知見なので残すが、これを「機構削除の作戦」として読まないこと。残る正しさ項目は上の
> 「機会的バックログ」を見よ。

**現状の負債（クロージャ捕捉だけで 4 つの重複メカニズムが併存）**:
`owned_captures`（ループ per-iteration 値凍結）/ `closure_captured_state`（per-instance 値凍結 + writeback）/
`box_captured_lexicals` の `loop_local_vars`（ループ boxing）＋ `multi_captured_mutated_locals`（兄弟 boxing,
#2751）/ env snapshot。隙間（単一脱出クロージャ等）を場当たりに残す。**さらに boxing ヒューリスティックを
足すのは誤り**（#2746 で perf 1s→150s+ と正しさ回帰の両面破綻を実証、#2749 に記録）。正しい方向 = 唯一の
escape 解析で統合・削除。

1. **✅ DONE (#2758): escape 解析の基盤（コンパイラ・キーストーン）**: local ごとに「identity がフレームを
   脱出するか」を分類。出力 = `needs_cell_locals` + `closure_escapes`。`multi_captured_mutated_locals`（≥2 兄弟
   proxy）を本物の escape 信号へ置換し、単一脱出クロージャを構造的に解いた。**非脱出 local は裸値のまま（hot path
   不変）**。bareword `f()` の捕捉セル読みも #2759 で修正。

2. **❌ 撤回: スカラーの第一級セル化で 3 機構削除（4→1）**: 実証で「`owned_captures` /
   `closure_captured_state` / `box_captured_lexicals` はセルで置換不可（非冗長）」と判明（上の STATUS 表）。
   宣言時セル化への移設は `let`/`temp` 復元を壊し、副テーブル削除は precompilation deserialize を壊した。
   **この削除はもう試みない。** セルの基盤（step 1）は既に landed しており、それが本移行の正しさ上の本丸だった。

3. **`.VAR` / `=:=` / itemization をセルモデルへ**: セル identity が入ったので `.VAR` はセルのコンテナ型、
   `=:=` はセル identity 比較、itemization はセル wrap。**`__mutsu_bound_decont` マーカー +
   `__mutsu_sigilless_alias` 文字列機構を削除**。reduce.t 62 平坦化・`.VAR.^name` 束縛反映もここで落ちる。

4. **配列/ハッシュ要素のセル化（COW）= Phase 2**: 要素を COW セルに。**`HashSlotRef`/`ArraySlotRef` の場当たり +
   grep-rw-view binding（now embedded `ArrayData.grep_source`, side-table 撤去済 #2985）+ name-based writeback
   reconcile を削除**。take-rw / `@a[0]:=` / 深い `>>++` が落ちる。
   要素にセルが載るので、**値スタック不変条件 + lvalue opcode（旧 Phase 0.5 第2段: `GetArrayVar`/`Index`
   auto-decont）を同梱**。

5. **属性セル + 属性束縛 = Phase 3**: `$!x :=` / per-attribute container template。

6. **型メタを生 Arc ポインタ keying からセル自身へ（Q2 項目を吸収）**: セルが安定 identity を持つので型メタを
   セルに載せ、`Arc::as_ptr as usize` keyed 副テーブル（間欠 flaky の根）を**削除**。

**速度の担保（設計に内蔵、後付けでない）**: (a) escape 解析でセルを省略（捕捉/エイリアス/`.VAR` されない
ローカルは裸値）、(b) 配列は COW で読みクローン無し、(c) decont は単一分岐で分岐予測が効く、(d) 中期の
NaN-boxing で payload 8byte 化。**各ステップで int.t 等の重量級 roast を timed 確認**（#2746 の教訓: perf 回帰は
`make test` で検出不能、CI release roast の timeout で初めて顕在化する）。

**やってはいけないこと**: 単一脱出クロージャや個別ケースを「もう 1 つの boxing ヒューリスティック」で塞ぐこと
（特例を増やし #2746 の轍を踏む）。1（escape 解析）→ 2（セル化）で**統合的に**解く。

### 段階導入（big-bang 回帰を避ける順序）

注: 下の Phase 0/0.5/1 の既存スライス（#2736–#2751）は地ならしと暫定ヒューリスティック。**正準の進め方は
上の「実装順序」**で、Phase 1 第1スライスの兄弟 boxing 等は step 1（escape 解析）が subsume・削除する。

- [~] **Phase 0 — decont チョークポイント整備（挙動不変リファクタ）**: 実装ログは
      [docs/container-identity.md](docs/container-identity.md)。
  - [x] **ヘルパ統合（#2736/#2737/#2738, 2026-06）**: 散在する decont 展開を各軸の正規ヘルパへ集約。
        3 軸（Scalar `$(...)` / ContainerRef `:=` / ArrayKind itemization）は別の型・別の意味論なので
        **融合せず**（融合は `is rw` writeback の Pair 判定・`@a=$l` 平坦化を壊す）。`descalarize`/
        `into_descalarized`（Scalar 再帰）、非 clone の `with_deref`（ContainerRef 読み・`deref_container`
        を再定義）を制定。挙動不変を確認。
  - [~] **Phase 0.5 = スタック不変条件＋lvalue opcode（段階的に分割）**: 値スタックを「常に decont 済み」
        とし `GetArrayVar`/`GetHashVar`/`Index` の auto-deref と push 集約、lvalue 専用 opcode
        （`GetLocalContainer`/`IndexContainer` 等）を追加。
    - [x] **第1段（挙動不変な地ならし, 2026-06-08）**: ContainerRef 読み軸の owned 集約（`Value::into_deref`
          新設 = `deref_container` の owned 版、PR3 後送り分）と、`GetLocal`/`GetGlobal` の手書き inline
          deref を `into_deref` へ集約。値読み出し opcode の棚卸し表を
          [docs/container-identity.md](docs/container-identity.md) §7 に記録（Phase 1 の設計図。`GetLocalRaw`
          が lvalue 読みの前例）。挙動不変を確認。
    - [ ] **第2段（実挙動変化を含む本体）**: `GetArrayVar`/`Index` の auto-decont（= stack 不変条件）と新
          lvalue opcode の本配線。push 内容が変わる**実挙動変化**を含むため、コンテナが要素に載る Phase 1 と
          **同一 PR** で入れる。
- [x] **Phase 1 — スカラーの第一級コンテナ化 ＝ landed（escape-aware セル）**: `$` 変数が escape 解析で
      共有セル（`ContainerRef`）を持つ。第1スライス（非ループ兄弟共有 #2751）→ escape 信号化（#2758）→
      bareword `f()` 修正（#2759）で**クロージャ共有の正しさは解決**。単一脱出クロージャ・兄弟共有とも PASS。
      束縛平坦化（reduce.t 62）・基本 `is rw` persistent は現状 PASS（上の注）。
      残る `.VAR.^name` 反映 / `is rw` 3-way 持続 は上の**機会的バックログ**へ（深い・単発）。
- [ ] **Phase 2 — 配列/ハッシュ要素のコンテナ化**: 要素を COW な `Arc<Vec<Scalar>>` 等のセルに。
      → take-rw（gather.t 38）, `@a[0] :=`, 深い `>>++` / `deepmap(++*)`（hyper.t 330-333）, object-hash,
      S12 accessors/instance。最もホットな表現に触るので Phase 1 の後。
- [ ] **Phase 3 — 属性コンテナ + 属性束縛**: `$!x :=` / per-attribute container template
      （S03-binding/attributes, S14-traits/attributes 5-8）。

各 Phase は `make test` + 関連 roast をローカル検証、全 roast は CI。Phase 0 は挙動不変なので安全に
大きく入れられる（CLAUDE.md「Refactor boldly」）。

### 「最速 × メンテしやすい」をどう両立するか

- **メンテ性（直接の勝ち筋）**: 統一コンテナモデルは散在する workaround を**削除**する — dual-store
  env↔locals、Arc-pointer-keyed 副テーブル（＝ flaky の根。**2026-06-13 に全廃済 — `ptr_keyed.rs` 削除**）、
  ad-hoc itemization フラグ、grep-rw-view binding（embedded 化済、要素セルで最終撤去）、name-based writeback
  reconcile。**1 つの概念が十数個の特例を置換**する。
  これが「世界最高 ＝ 最もメンテしやすい」の核。
- **速度（設計で担保）**: コンテナは間接参照を足すので、(a) **エスケープ解析でコンテナを省略** — 捕捉も
  `.VAR` もエイリアスもされないローカルは裸値のまま（コンパイラが判定。MoarVM の spesh と同型）、
  (b) **配列は COW** で読みはクローン無し、(c) decont は単一分岐で予測が効く、(d) 中期の NaN-boxing で
  payload 8 byte 化すればセルも安価。pervasive container でも spesh/escape で高速化できることは Rakudo が
  実証済み。mutsu の賭けは「コンテナ + エスケープ解析で hot path から消す」。

### 既存項目との関係（重複ではなく収斂）

- レバー C「本丸: 自由変数を indexed upvalue（`ContainerRef`）として捕捉」は **Phase 1 の一部**として完成する。
- Q2「コンテナ型メタを安定 ID へ移す」は **本移行に吸収**される — コンテナが identity を持てば型メタは
  コンテナ自身に載り、Arc-pointer-keying と flaky が構造的に消える。
- レバー B（scoped overlay env）は Phase 1 の前提（変数の所有を env に集約済みであること）であり地ならし。

**着手順**: レバー B 完了 → Phase 0（チョークポイント・挙動不変）→ Phase 1（スカラー）→ Phase 2（要素）
→ Phase 3（属性）。

---

## Q2 (5〜6月): パフォーマンスと Container semantics

目標: **「簡単なスクリプトなら raku の代わりに使える」レベルに到達**

### メソッド呼び出しパフォーマンス (進行中)

- 現状: method-call 2.7x、bench-class 2.3x（目標: 2x 以下）
- 残りのボトルネックは env deep clone (~9μs/call)
- [ ] closure captures as indexed slots (Phase 3b) — env サイズ自体を削減

### Container semantics

- [x] `our $x` クラス属性のバインド (S12-attributes/class.t — tests 11-12 は既に pass、#2541 で 26/28 に改善)
- [x] 多次元構造のエレメントレベルバインド (nested.t — PR #2413 で 42/43 に改善)
- [x] `undefine` の aggregate 参照セマンティクス (undef.t — PR #2414 で 90/91 に改善)

### Exception types (高インパクト — 残り ~16 roast テストをブロック)

- [x] X::TypeCheck::Binding::Parameter, X::Assignment::RO 実装 (#2477)
- [x] X::Adverb 実装 (#2505)
- [x] X::PseudoPackage::InDeclaration 実装 (#2507)
- [x] X::Worry::Precedence::Range 実装 (#2502)
- [x] X::IllegalDimensionInShape, X::Comp::BeginTime 実装 (#2503)
- [ ] 残りの型付き例外 (X::Str::Numeric, X::Method::NotFound, X::Undeclared, X::Cannot::Lazy, X::EXPORTHOW::InvalidDirective 等)
- [ ] 詳細は [TODO_roast/BLOCKERS.md](TODO_roast/BLOCKERS.md) の "throws-like / Exception Types" セクション参照

### アーキテクチャ・正しさの修正 (高インパクト — [ANALYSIS.md](ANALYSIS.md) 由来)

コードベース精読で判明した根本的な正しさ・健全性の問題。Threading/Async (BLOCKERS 31件) の
最大ボトルネックに直結するため最優先。詳細・再現コマンドは ANALYSIS.md 各節を参照。

- [x] **無限 Range の即時展開クラッシュ撲滅 (ANALYSIS §8.2) + 遅延リスト pull モデル統一 (§8.1)**
      — 実質完了（#2791/#2793/#2796/#2799/#2804、詳細は [news/2026-06.md](news/2026-06.md)）。
      `materialize_capped` で無限 Range crash 撲滅、eager ops は `X::Cannot::Lazy` throw、
      無限 gather コルーチンの bounded pull、**method 形 `.map`/`.grep` を真に遅延化**（`LazyList::lazy_pipe`）。
      残（低優先・実害確認なし）: §8.2 の残り生 `(a..=b).collect()` サイト掃討（多くは end cap 済/`.take`/`0..n`）。
- [ ] **並行 state 共有の修正** (ANALYSIS §8.3, §2.2) — `clone_for_thread` のスナップショットコピーを
      やめ、共有すべきレキシカル/state/global を `Arc<Mutex>` のライブセルとして真に共有する。
      `start` ブロック間で `$counter`/`state $n` が共有されない (mutsu 1/0、raku 4/3)。
- [ ] **`unsafe` の "single-threaded 前提" を是正** (ANALYSIS §2.3) — `Arc::as_ptr as *mut` での
      エイリアス書き換え 11 箇所がスレッド生成と矛盾し UB の余地。配列/ハッシュを共有セル化して撤廃。
- [ ] **正規表現のコンパイル済みキャッシュ導入** (ANALYSIS §8.4) — `Value::Regex(Arc<String>)` が
      毎マッチ再パース。実測 raku 比 8.6x 遅 (変数束縛でも改善せず)。パターン→構造のキャッシュを追加。
- [ ] **コンテナ型メタデータを生 Arc ポインタ keying から安定コンテナ ID へ移す**（間欠 flaky の根本原因。
      ANALYSIS §2.3 の `Arc::as_ptr as *mut` エイリアスと同根の Arc-ポインタ-identity 不健全性）—
      **※ 上の「🟣 第2優先: 第一級コンテナ」に吸収される（コンテナが identity を持てば型メタはコンテナ
      自身に載り、ポインタ keying と flaky が構造的に消える）。単独で着手せず本移行の一部として扱う。**
      `array_type_metadata`/`hash_type_metadata`/`set`/`bag`/`mix` の 5 マップが `Arc::as_ptr as usize`
      をキーにしており、コンテナ drop 後にそのポインタが無関係の後続アロケーションに再利用されると、
      stale な型情報がそちらに aliasing する。typed 配列 `@.items` の `Item` 要素型が `EVAL` の生成
      リストに乗り移って `Int` を `Item` と型チェックし die（`roast/S02-names-vars/perl.t` ~10%、exit 255 =
      plan 未完了）、object hash のキー制約が `.clone`/再構築コンテナに乗り移って読みが `Nil` になる
      （`roast/S02-types/hash.t` の `%a.clone` ブロック ~0.2%）。CLAUDE.md で「CI-load timeout」と誤分類
      されていたが、実体は alloc/hash 順依存の**正しさバグ**（テストは ~0.07s 実行・起動 ~0ms、perf 無関係）。
    - **済**: ハッシュ要素 READ 経路を #2635 で部分対処（name-based reconcile + stale 上書き復元、
      S09-typed-arrays/hashes.t を 0/500 に）。
    - **済（untyped 代入時の stale クリア、`%`→`@` 対称化）**: `vm_var_assign_ops.rs` は untyped `%h = ...`
      代入時に「`var_type_constraint` が `None`＝確実に untyped なのに値に型メタが付いている＝ポインタ再利用の
      stale alias」を `unregister_container_type_metadata` で除去していた。これを `@a = ...` にも拡張
      （native/typed 配列は `var_type_constraint` Some でスキップ＝不変）。`t/native-array-mut.t` subtest 26
      flaky の根（drop 済み typed 配列の slot を untyped `@a` の構築が再利用し `Array[Int]` を継承）を断つ。
      **部分対処**（untyped *変数代入*サイト限定。EVAL 生成リスト等の他サイトや根の Arc-ptr keying 自体は残＝
      下記「本筋」）。
    - **試して revert（2回・いずれも不成立）**: (1) メタに `Weak` を併存させ lookup 時 `Arc::ptr_eq` 検証
      する案は family 全体を 0/300 にしたが、native typed 配列（`my int @a`/`my str @a`）で
      `native-int.t` 240 件回帰 → revert。(2) ハッシュで効いた name-based reconcile（`var_type_constraints`
      から mut メソッド入口 + 要素代入で再登録）を配列にも一般化して Weak と併用したが、native 配列の
      240 件は**全く減らず**（reconcile が native 配列のメタライフサイクルに届かない）→ これも破棄。
      **結論: Weak + name-reconcile の安価なパッチは native 配列で行き止まり。** 部分対処の積み増しでは
      family を根治できない。
    - **本筋（埋め込みで構造的に解消 — ✅ 完了 2026-06-13）**: 生ポインタ keying を廃止し型メタを**コンテナ
      Value 自体に載せる**方針を完遂。**Hash = HashData 埋め込み（型メタ #2952 ＋ original_keys #2954）、
      Set/Bag/Mix = *Data struct 埋め込み (#2957)、Array = ArrayData wrapper（型メタ + default + shape +
      grep-view #2985）**。これで `S02-types/hash.t` の `.clone` flaky・object-hash の Nil 読み・
      `S02-names-vars/perl.t` の EVAL 生成リスト aliasing flaky の構造的根を全廃。**最後に残っていた非型メタの
      ptr-keyed 副テーブル（hash default）も `HashData.default` へ埋め込み、`PtrKeyedMap`/`ptr_keyed.rs`
      （#2953 の Weak-guard interim 防御機構）ごと削除。`Arc::as_ptr`-keyed 副テーブルはコードベースから消滅。**
      再現手順・失敗した2手法はメモリ `project_known_failing_tests_reclassified`。

---

## Q3 (7〜9月): ウェブアプリに必要なモジュール互換性

目標: **mutsu でウェブブログシステムが構築できる**

### ウェブブログに必要なスタック

| レイヤー | モジュール | 状態 | 備考 |
|----------|-----------|------|------|
| JSON | JSON::Tiny | ✅ テスト全 pass | #2329 |
| テンプレート | Template::Mustache | ⚠️ grammar action dispatch がブロッカー | proto regex in alternation の action 呼び出し |
| HTTP サーバー | HTTP::Server::Tiny | ❌ 依存未解決 | HTTP::Parser, IO::Blob, HTTP::Status |
| DB | (検討中) | ❌ | NativeCall 不可。JSON file / SQLite CLI wrapper |

### モジュール対応の進め方

1. **Template::Mustache** — `.meta` メソッド等を修正してテスト通過
2. **HTTP::Server::Tiny** の依存モジュール群（HTTP::Parser, IO::Blob, HTTP::Status）
3. **HTTP::Server::Tiny** 本体
4. DB アクセス — pure Raku の簡易実装 or qqx ベースの SQLite wrapper

### その他モジュール

- [ ] File::Temp
- [ ] MIME::Base64 (pure Raku)
- [ ] File::Directory::Tree

### バイナリ配布

- [ ] mise GitHub バックエンドでのインストール検証
- [ ] GitHub Releases の自動化

### Roast 90% 突破

- [x] Whitelist → 1190+ (roast 90%) — 達成: 1218

---

## Q4 (10〜12月): 安定性とコミュニティ

目標: **他の人が試して「ちゃんと動く」と思えるレベル**

### 安定性

- [ ] エッジケースでの panic/crash を 0 にする（[ANALYSIS.md](ANALYSIS.md) §8.2 の Range 展開クラッシュ・
      §2.1 の panic→`X::` 変換境界。Q2 で着手済みなら継続）
- [ ] エラーメッセージの品質向上
- [ ] 制御フロー (`return`/`last`/`next`/`take`/`emit`) を `RuntimeError` god-struct から
      `enum Control` へ分離（[ANALYSIS.md](ANALYSIS.md) §2.4 — `result_large_err` 負債の解消）

### パフォーマンス Phase 2

- [ ] method-call を 1.5x 以下にする（closure captures indexed slots → NaN-boxing）
- [ ] bench-class を 1.5x 以下にする
- [ ] bench-fib (型制約付き) を 2x 以下にする
- [ ] NaN-boxing: Value を 72 bytes → 8 bytes に（Int/Num/Bool/Nil）
- [ ] JIT compilation (Cranelift) の検討
- [ ] Cycle collector (circular object references)

### ドキュメントとコミュニティ

- [ ] 「mutsu でウェブブログを作る」チュートリアル
- [ ] raku との互換性マトリクス公開
- [ ] WASM playground の公開

### Roast

- [x] Whitelist 1200+ 目標 — 達成: 1218

---

## Backlog: 未実装の言語機能

BLOCKERS.md の分析に基づき、インパクト順に並べたもの。

### Phasers

- [ ] Phaser rvalue caching (INIT/CHECK/BEGIN as rvalues in closures)
- [ ] PRE/POST phasers (contract programming)

### Type constraints / Signatures

- [ ] Signature type-checking enforcement (reject wrong-type args with X::TypeCheck)
- [ ] Native int/uint overflow and bounds checking
- [ ] Multiple signatures on a single sub

### OOP

- [ ] Namespaced class construction (`A::B.new`)
- [ ] `augment class` improvements (augmenting with new attributes)
- [ ] Parameterized role mixin

### Supply/Concurrency

- [ ] Supply backpressure
- [ ] `supply`/`react` block scoping issues
- [ ] Tap management (close, drain)

### IO / Process

- [ ] IO::Handle read modes (binary, encodings)
- [ ] Proc and Proc::Async completeness
- [ ] File test operators (`-e`, `-f`, `-d` etc.)

### Regex / Grammar

- [ ] **Match キャプチャ番号付け / コンテナ kind**（`.caps`/`.chunks` の値と `Match.gist` 位置
      キャプチャ表示・ネスト Match の corner-quote は #2644 で実装済み。残は別根の2件）:
      (1) `$<x>=(...)` 名前付きキャプチャが positional スロットにも重複格納され `(\d)` の番号がずれる
      （`/$<x>=(\w)(\d)/` で raku は `x`+`0`、mutsu は `0`+`x`+`1`）。
      (2) `m:g//` 結果を `my @m` に代入後 `@m.gist` が `(…)` を返す（`say @m` は正しく `[…]`）—
      method receiver が結果を List-kind で見る dual-store ナンス。
- [ ] Lookbehind assertions (`<!after>`)
- [ ] `:Perl5` modifier edge cases

### Parser: 未実装演算子

- [ ] `ff` / `fff` — flipflop operators (8 variants)
- [ ] `==>` / `<==` — feed operators の **precedence の残**（基本動作・インライン `my @o`/`my $x`
      sink 代入・スカラー sink の Array 化は #2643 で実装済み）。`==>` は `=` より緩い結合のはずだが
      mutsu は強く結合する: `my @out = (1,2,3) ==> map {...}` は raku では `@out = (1,2,3)`（feed は
      map に流して捨てる）だが mutsu は `[2 4 6]`。`say [1,2,3] ==> grep {...}` も同様の結合差。
- [ ] `~<` / `~>` — string bitwise shift（raku 本体も "not yet implemented" のため優先度低。
      `~&` / `~|` / `~^` は実装済み）。
  - 実装済み（Backlog から削除）: `minmax`（メソッド `.minmax` + 中置）、`unicmp` / `coll`。

### Parser: メタ演算子

- [ ] Generalized negation meta (`!op`) — beyond `!~~` and `!%%`
- [ ] Hyper assignment (`@a >>+=>> 1`)
  - 実装済み（Backlog から削除）: Triangular reduction (`[\+]`, `[\*]` など)。

### アーキテクチャ・リファクタ (中長期 — [ANALYSIS.md](ANALYSIS.md) §3, §4, §6)

VM↔Interpreter の切り離し本体は冒頭の「🔴 最優先」セクション参照。以下はそれ以外の構造的負債。

- [x] 正規表現の validator/matcher 二重実装を単一パーサに統合 (§3.1) — 完了（news/2026-06.md）
- [ ] `.^methods`/`.can` の型別メソッド一覧を実ディスパッチ表から導出 (§4)
- [ ] roast fudge ロジックを核から分離 / テストの一時ファイルを `tmp/` へ / 500行超ファイルの分割 (§6)

### Practicality (将来)

- [ ] REPL
- [ ] Debugger
- [ ] `zef` package manager compatibility
- [ ] Native binary output

---

## メトリクス

| 指標 | 現在 (5月末) | Q2 目標 | Q3 目標 | Q4 目標 |
|------|-----------|---------|---------|---------|
| Whitelist | **1218** ✅ | 1190+ ✅ | 1200+ ✅ | 1220+ |
| fib(25) vs raku | **1.0x** ✅ | <10x ✅ | <10x | <10x |
| method-call vs raku | **2.7x** | <2.5x | <2x | <1.5x |
| bench-class vs raku | **2.3x** | <2x | <1.5x | <1.5x |
| 起動時間 vs raku | 0.04x | 0.04x | 0.04x | 0.04x |
| JSON::Tiny | ✅ テスト全pass | ✅ | ✅ | ✅ |
| Template::Mustache | ⚠️ grammar action | - | ✅ | ✅ |
| HTTP::Server::Tiny | ❌ | - | ✅ | ✅ |
| 動作モジュール数 | 1 | 2 | 5+ | 5+ |
| mise install | ❌ | ❌ | ✅ | ✅ |
