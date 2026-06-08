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

着手順（依存順。各ステップは strangler-fig で段階的に、CI を安全網に）:

- [ ] **① 残存する真の tree-walk フォールバックの撲滅**（ANALYSIS §1.1。これが「フォールバック率 0%」の本体）:
      `call_method_with_values`（`vm/vm_call_method_compiled.rs`/`vm_call_method_mut_ops.rs`/`vm_data_ops.rs` の各サイト
      ＝生きた Instance/Buf/Failure メソッド fork）、`run_instance_method`（`class.rs` ＝ユーザー定義クラスメソッド）、
      `call_function`/`call_function_fallback`（`vm_call_func_ops.rs`/`vm_call_dispatch.rs`）、`run_react_event_loop` を
      VM ネイティブ実行に置換。**残すフォールバックには規約どおり `// TODO: compile to bytecode` を付与**（現状 VM ツリー
      に 0 件 ＝ 負債が不可視。まず可視化する）。**可視化済み（2026-06-08）**: 全サイトを `// TODO: compile to bytecode`
      （真フォールバック）/ `// CARRIER:`（反射・MOP・EVAL・メタプロ hook）で注釈し、進捗台帳
      [docs/vm-interpreter-fallback-ledger.md](docs/vm-interpreter-fallback-ledger.md) を新設。同 PR で `succ`/`pred` を
      統一 compiled-first ディスパッチへ降ろし §1 から 2 サイト消化。以後は台帳の行を消す形で進める。
- [ ] **② 宣言の実行を VM 所有レジストリへ**: `class`/`role`/`enum`/`subset`/`token`/`sub`/`method` は現在
      `Register*` opcode が `interpreter.register_*_decl()` を呼び、クラスシステム・MRO・role 合成が Interpreter 側。
      これらのレジストリを VM 所有データへ移す（②は③の前提）。
- [ ] **③ VM が借用している Interpreter 状態を VM 所有へ移管**: env HashMap（変数ストア本体）・classes/roles/enums
      レジストリ・型検査（`type_matches_value`/`var_type_constraint`）・readonly 追跡・`let`/`temp` 復元・multi 解決・
      state 変数・`current_package`。これが移れば **interpreter ブリッジ自体が不要**になる。最大の山。
- [ ] **④ 本質的キャリアの扱いを確定**（消すのではなく分離 or 明示）: `EVAL`/`EVALFILE`（既に compile→サブ VM 実行で
      tree-walk ではない＝env/レジストリ所有のため Interpreter を借りる**キャリア**）、正規表現の埋め込み `{}` ブロック
      （interpreter regex エンジン経由で caller local を名前書き込み）、pseudo-package（`CALLER::`/`OUTER::` の reflective
      lookup）。③で所有が VM に移れば、これらは「Interpreter 実行パス」ではなく単なる共有レジストリ参照になる。
- [ ] **⑤ dual-store 機構の削除（レバー B 完遂）**: ①〜③でブリッジが消えた後、`env_dirty`/`ensure_locals_synced`/
      `sync_locals_from_env`/`saved_env_dirty`/`VmCallFrame` の dirty/bind フィールド群を撤去。カウンタ自体は既に
      ほぼ最適化済み（method-call 1 / bench-class 2 / fib 0、docs/vm-dual-store.md の resume map）で、残る `bench-array`
      の closure コストは🟣第2優先「第一級コンテナ」Phase 1（#2751 で着手済み）に収斂する。

関連: 🟣第2優先「第一級コンテナ」はこの最終ゴールと地続き（レバー C ＝ Phase 1 の一部、Q2 の Arc-pointer flaky を
吸収）。本セクション①〜③（Interpreter 実行パス撤去）と並行 or 接続して進める。

---

## 🟣 第2優先（インタープリタ廃止の次）: 第一級コンテナ (container identity) への移行

**優先順位**: 上の「🔴 最優先 = 重複実装を消す（tree-walking Interpreter 廃止）」を**第1優先**、
本セクションを**第2優先**とする（ユーザー方針 2026-06-06）。両者は独立ではなく地続き（レバー B/C の本丸が
本移行の前提・一部）なので、インタープリタ廃止の完了を待ってから本格着手しつつ、その尾部
（レバー C upvalue 等）と自然に接続する。なお第一級コンテナ移行は dual-store・Arc-pointer 副テーブル・
ad-hoc itemization フラグ等の**重複/特例を 1 概念で置換する**点で、上の鉄則「重複を許さない」と同じ方向。

実装台帳: [docs/container-identity.md](docs/container-identity.md)（現状の地図・段階スライス・進捗ログ）。

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

### 実装順序（アーキテクチャ第一・北極星: 最もクリーン × 最速）

**roast 通過は結果であって目標ではない**（ユーザー方針 2026-06-08）。よって本移行の**進捗メトリクスは
「削除した重複/特例メカニズムの数」**とする。各ステップは特例を**純減**させ、独立に出荷可で、perf を
回帰させない（escape 解析でセルを省略するのが速度の肝）。

**現状の負債（クロージャ捕捉だけで 4 つの重複メカニズムが併存）**:
`owned_captures`（ループ per-iteration 値凍結）/ `closure_captured_state`（per-instance 値凍結 + writeback）/
`box_captured_lexicals` の `loop_local_vars`（ループ boxing）＋ `multi_captured_mutated_locals`（兄弟 boxing,
#2751）/ env snapshot。隙間（単一脱出クロージャ等）を場当たりに残す。**さらに boxing ヒューリスティックを
足すのは誤り**（#2746 で perf 1s→150s+ と正しさ回帰の両面破綻を実証、#2749 に記録）。正しい方向 = 唯一の
escape 解析で統合・削除。

1. **escape 解析の基盤（コンパイラ・キーストーン）**: local ごとに「identity がフレームを脱出するか」を
   単一原理で分類（脱出クロージャ捕捉 / `:=` エイリアス / `is rw` / `.VAR`・`=:=` 参照 / take-rw）。出力 =
   local 単位の **needs-cell ビット**。これが `loop_local_vars` / `multi_captured_mutated_locals` /
   `owned_captures` / `closure_captured_state` を**置換する唯一の解析**。#2751 の兄弟 boxing とループ boxing は
   本解析が subsume して削除する**暫定ヒューリスティック**と位置づける。**非脱出 local は裸値のまま（hot path
   不変）** — これが速度の肝（escape されないローカルはコンテナを持たない）。
   - 単発の「単一脱出クロージャ」バグはここで構造的に解く。**個別 boxing パッチで塞がない。**

2. **スカラーの第一級セル化（escape 解析でゲート）**: needs-cell な `$` local を**宣言時にセル**（`ContainerRef`）
   として格納、非脱出は裸値。`=` はセル格納、`:=` は rebind、読みはチョークポイントで decont。捕捉セルは Arc
   共有で全クロージャが共有 → **`owned_captures` / `closure_captured_state` / `box_captured_lexicals` の
   ヒューリスティックを削除**（4 メカニズム → 1）。レバー C 完遂。

3. **`.VAR` / `=:=` / itemization をセルモデルへ**: セル identity が入ったので `.VAR` はセルのコンテナ型、
   `=:=` はセル identity 比較、itemization はセル wrap。**`__mutsu_bound_decont` マーカー +
   `__mutsu_sigilless_alias` 文字列機構を削除**。reduce.t 62 平坦化・`.VAR.^name` 束縛反映もここで落ちる。

4. **配列/ハッシュ要素のセル化（COW）= Phase 2**: 要素を COW セルに。**`HashSlotRef`/`ArraySlotRef` の場当たり +
   grep-rw-view binding + name-based writeback reconcile を削除**。take-rw / `@a[0]:=` / 深い `>>++` が落ちる。
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
- [~] **Phase 1 — スカラーの第一級コンテナ化**: `$` 変数が共有セル（`ContainerRef`）を持つ。
  - [x] **第1スライス（非ループ兄弟クロージャ共有 = escape-aware）**: ≥2 個の兄弟クロージャに捕捉される
        captured-mutated `$` スカラーだけを box する精密 signal（`multi_captured_mutated_locals`）+
        mutation-writeback の cell 対応化。`make(); $s(42); $g()` が共有される。前回の broad boxing 回帰
        （perf/correctness）を構造的に回避。詳細は [docs/container-identity.md](docs/container-identity.md)。
  - [ ] **残（個別に塞がず統合解決）**: 単一脱出クロージャ / `.VAR.^name` 反映 / `is rw` 3-way persistent /
        束縛平坦化（reduce.t 62）/ S02 変数捕捉 は、上の「実装順序」の **step 1（escape 解析）→ step 2
        （セル化）→ step 3（.VAR/=:=/itemization）** で統合的に解く。個別 boxing パッチを足さない（#2746 の轍）。
        `GetArrayVar`-`Index` の auto-decont は step 4（要素セル化）に同梱。
- [ ] **Phase 2 — 配列/ハッシュ要素のコンテナ化**: 要素を COW な `Arc<Vec<Scalar>>` 等のセルに。
      → take-rw（gather.t 38）, `@a[0] :=`, 深い `>>++` / `deepmap(++*)`（hyper.t 330-333）, object-hash,
      S12 accessors/instance。最もホットな表現に触るので Phase 1 の後。
- [ ] **Phase 3 — 属性コンテナ + 属性束縛**: `$!x :=` / per-attribute container template
      （S03-binding/attributes, S14-traits/attributes 5-8）。

各 Phase は `make test` + 関連 roast をローカル検証、全 roast は CI。Phase 0 は挙動不変なので安全に
大きく入れられる（CLAUDE.md「Refactor boldly」）。

### 「最速 × メンテしやすい」をどう両立するか

- **メンテ性（直接の勝ち筋）**: 統一コンテナモデルは散在する workaround を**削除**する — dual-store
  env↔locals、Arc-pointer-keyed 副テーブル（＝ flaky の根。下記 Q2 項目を吸収）、ad-hoc itemization
  フラグ、grep-rw-view binding、name-based writeback reconcile。**1 つの概念が十数個の特例を置換**する。
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

- [ ] **無限 Range の即時展開クラッシュを撲滅** (ANALYSIS §8.2) — `(a..=b).map(Value::Int).collect()`
      が src 全体 43 箇所で無ガード、`MAX_ARRAY_EXPAND` ガードは 9 箇所のみ。無限 Range で
      `capacity overflow` パニックしプロセスごと落ちる。展開サイトを単一ヘルパに集約しガードを一元化。
      `(1..Inf).grep(* %% 2)[^3]` がクラッシュ (raku は `(2 4 6)`)。
- [ ] **遅延リストを pull/Iterator モデルに統一** (ANALYSIS §8.1) — `grep` 等の eager 経路を
      `map`/`first`/`head`/`[]` と同じ遅延扱いに揃える。Seq/Range を真の遅延イテレータに。
- [ ] **並行 state 共有の修正** (ANALYSIS §8.3, §2.2) — `clone_for_thread` のスナップショットコピーを
      やめ、共有すべきレキシカル/state/global を `Arc<Mutex>` のライブセルとして真に共有する。
      `start` ブロック間で `$counter`/`state $n` が共有されない (mutsu 1/0、raku 4/3)。
- [ ] **`unsafe` の "single-threaded 前提" を是正** (ANALYSIS §2.3) — `Arc::as_ptr as *mut` での
      エイリアス書き換え 11 箇所がスレッド生成と矛盾し UB の余地。配列/ハッシュを共有セル化して撤廃。
- [ ] **VM の panic→`X::` 変換境界を `run()` に設置** (ANALYSIS §2.1, §5) — ユーザコード起因の
      Rust パニックを Raku 例外に変換し、プロセスクラッシュを防ぐ (Q4 「panic/crash を 0 に」の前倒し)。
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
    - **試して revert（2回・いずれも不成立）**: (1) メタに `Weak` を併存させ lookup 時 `Arc::ptr_eq` 検証
      する案は family 全体を 0/300 にしたが、native typed 配列（`my int @a`/`my str @a`）で
      `native-int.t` 240 件回帰 → revert。(2) ハッシュで効いた name-based reconcile（`var_type_constraints`
      から mut メソッド入口 + 要素代入で再登録）を配列にも一般化して Weak と併用したが、native 配列の
      240 件は**全く減らず**（reconcile が native 配列のメタライフサイクルに届かない）→ これも破棄。
      **結論: Weak + name-reconcile の安価なパッチは native 配列で行き止まり。** 部分対処の積み増しでは
      family を根治できない。
    - **本筋（次セッションはここから直接着手）**: 生ポインタ keying を**完全に廃止**し、型メタを
      **コンテナ Value 自体に載せる**（例: `Value::Array(Arc<Vec<Value>>, ArrayKind, Option<Arc<ContainerTypeInfo>>)`、
      Hash/Set/Bag/Mix も同様）か、**真の安定コンテナ ID**（生成時に採番し COW・再構築・`clone_for_thread`
      を跨いで保持）。前者は Value variant 署名変更で全構築/match サイトに波及する大改修だが、副テーブルの
      ポインタ再利用 aliasing を構造的に消せる唯一の道。hot path 全体に関わるため段階的に。再現手順・失敗
      した2手法の詳細はメモリ `project_known_failing_tests_reclassified` 参照。

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
