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
- **レバー C（クロージャ upvalue 化）**: 境界の明確なスライス 1/2/2b/3/3b 完了。残るは非ループ一般コンテナ
  捕捉 1 件で、これは🟣第2優先「第一級コンテナ」Phase 1 に吸収（下記）。

### 本丸: 重複実装を消す（進行中・主作業）

**測り方 = 残っている重複実装の数**（フォールバック率ではない）。手順・落とし穴・優先マップは
[docs/vm-interpreter-dedup.md](docs/vm-interpreter-dedup.md)。安全削除の必須手順:
native_function に arm がある（必要条件）だけでは不十分 — **EVAL 経路で同値確認**
（`mutsu -e 'say EVAL(q{f(...)})'` が raku と一致）してから削除し、`make roast` で確認する。VM の
`try_native_function` と interpreter fallback の `native_function` は**カバレッジが違う**（例: `chrs`/`ords`
は native arm があるのに fallback 未到達 → 残す）。

- [x] **Category A — 純粋値 builtin（delete & fallthrough）完了**: native が authoritative。Interpreter の
      `builtin_*` arm + 本体を削除し、catch-all → fallback → native_function に委譲。
  - [x] **第1バッチ (#2714)**: `abs`/`lc`/`uc`/`tc`/`trim`/`flip`/`chr`/`ord`/`chars` の Interpreter
        コピー削除。
  - [x] **第2バッチ（Category A 完了）**: 残りの純粋値重複 `sign`/`ords`/`chrs`/`unival`/`univals` を削除。
        `sign`/`ords` は native 1-arg が既にカバー（純粋削除）。第1バッチで「fallback 未到達」として
        残していた `chrs`/`unival`/`univals` は、重複維持ではなく **native を到達可能化** して削除:
        `chrs` を `native_function_variadic` へ全 arity ルーティング（+ `value_to_list` で Range/Seq 平坦化）、
        `unival`/`univals` を `native_function_1arg` から `.unival`/`.univals` メソッド実装へ委譲。
        VM/EVAL 両経路で raku 一致を確認。**純粋値 builtin の重複はゼロ**。`words`（IO 版で別物）は非対象。
- [~] **Category B — genuine fork（native に難ケースを足してから削除）**: `min`/`max`/`minmax`/`sort`/`join`/
      `first`/`flat`/`elems`/`index`/`rindex` 等。native/pure 層は comparator ブロック・lazy・junction で bail し
      Interpreter に落ちる。**比較子ブロックは VM 層で `vm_call_on_value`（`.map`/`.grep` と同じクロージャ
      dispatch）で呼べる**ので、VM 層に完全実装を置いて Interpreter コピーを削除する。
  - **ブロック系 fork（comparator/mapper/matcher ブロック）= 完了**。共通パターン: オーケストレーション
    （fold/sort/scan 本体）を engine 非依存の単一実装に抽出し、**ブロック呼び出しだけ**を VM
    (`vm_call_on_value`) / interpreter (`call_sub_value` / `eval_call_on_value`) のクロージャ（trait）で差し替え。
    VM がインタープリタにフォールバックしなくなり（`interpreter_fallbacks=0` 実測）、重複オーケストレーションが消える。
    - [x] **sort (#2727)**: `sort_items_generic`/`sort_indices_generic` + `SortCaller` 抽出。VM が comparator/
          mapper/`:k`/Hash sort を完全 native 化。`dispatch_sort` は interpreter carrier 用の薄いアダプタに縮小。
    - [x] **min/max (#2728)**: `extrema_from_values_generic` 抽出。VM が `.min`/`.max`（`:by` ブロック・
          `:k`/`:v`/`:kv`/`:p` adverb 込み）を native 化。
    - [x] **minmax (#2730)**: `minmax_from_values_generic` 抽出。VM が `.minmax`（`:by` 込み）を native 化。
    - [x] **first (#2731)**: `find_first_match_generic` + `FirstMatcher` trait 抽出。VM が `.first`（block/regex/
          smartmatch matcher、Hash 要素は `pair_as_positional`）を native 化。adverb/Bool-matcher は fallback。
  - **残: lazy-fork（`join`/`flat`/`elems`/`reverse`）**。これらは comparator ブロックではなく **lazy 強制が
    native にできない**のが fork 原因。native 版と interpreter 版が itemized array / Stash / variadic で
    **微妙に drift**（dedup doc 警告の実例）しているため、単純な「lazy 強制 → native 委譲」では挙動が変わる。
    drift を 1 件ずつ reconcile（どちらが raku 正かを確認）してから委譲する慎重な per-item 作業が必要。
  - **対象外**: `index`/`rindex` は interpreter コピーが存在せず既に native のみ。
  - ✅ **sort 移行の落とし穴 — 根本原因が判明（2026-06-07, #2725）**: 「`%h.sort({block})` → expected 2 got 0」
    の正体は **`Value::Pair` vs `Value::ValuePair` の束縛差**。mutsu は呼び出し側の名前付き引数を `Value::Pair`
    （dispatch 全体で positional arity から除外）、positional な pair 値を `Value::ValuePair` として **Value
    variant で区別**している。**Hash の反復（`value_to_list`）は `Value::Pair` を生成**するため、要素をそのまま
    comparator/matcher ブロックに渡すと**名前付き引数として束縛され positional が 0 個**になる（→ "got 0"）。
    リストリテラル `(a=>3),(b=>1)` は `ValuePair` を生成するので `@x.list.sort({block})` は動いていた。これは
    sort 固有ではなく、同根の live バグ `%h.first({block})`（→ "expected 1 got 0" / `.value on Any`）でも再現。
    - 対処: 汎用ヘルパ `runtime::utils::pair_as_positional`（`Pair`→`ValuePair`）を追加し、要素をブロックに
      positional 束縛させる。`find_first_match_over_items`（`.first` 駆動）に適用済み（#2725）。
    - **sort 移行の解禁**: native/VM sort を書く際、comparator にペア要素を渡す箇所で `pair_as_positional` を
      使えば旧 `dispatch_sort` の暗黙処理を吸収できる → `dispatch_sort` 削除が可能になる（make roast 必須）。
    - 別根・未対処: pointy `-> $p {...}` / `*.value` matcher は `find_first_match_over_items` の `Value::Sub`
      呼び出し分岐ではなく `smart_match` 経路に行き topic 未束縛（リテラル配列でも失敗）。別 PR。
- [ ] **Category C — メソッド / 演算子・arith・coercion の重複**: native fast path（`src/builtins/methods_*`,
      `vm_arith_ops`）と Interpreter slow path（`src/runtime/methods*.rs`）の重複。段階導入・手動監査
      （手順・対象マップ・進捗は [docs/vm-interpreter-dedup.md](docs/vm-interpreter-dedup.md)）。
  - [x] **Phase 1a (#2719)**: arith `%`/`mod` の `runtime/ops.rs::apply_reduction_op` ローカル再実装を削除し
        native `arith_mod` へ委譲（重複2 arm 削除、`[%] 2**70,3` / `[%] 5,0` の正しさも改善）。
  - [x] **Phase 1b (#2719)**: Instance→numeric bridge の重複統合。VM `coerce_numeric_bridge_value` を
        interpreter `coerce_infix_operand_numeric`（単一 authoritative 実装）へ委譲（重複1削除）。
        `utils::coerce_numeric`/`coerce_to_numeric` は既に単一実装の共有なので非対象。
  - [x] **Phase 2 (#2719, #2723)**: 手動監査の結果、**単純値メソッドの interpreter コピーは既に削除済み**と
        判明（残るのは生きた Instance/Buf/Failure/comparator fork。`dispatch_method_by_name_*` に harvest なし）。
        真の残重複は `apply_reduction_op` の演算子本体の再実装で、VM がそこへ委譲しているので authoritative:
        - `~`（concat）を VM `concat_values`（state-free 化）へ委譲（重複1削除＋非ASCII NFC/Buf の潜在バグ修正）。
        - `[minmax]` を共有 `vm_misc_ops::minmax_bounds_of_value`（再帰版）へ委譲（重複1削除＋ネストの bug 修正）。
        - 論理短絡 `&&`/`||`/`//` は reduction 専用実装（VM は infix をジャンプにコンパイル）で重複ではない。
          比較は共有 `runtime::compare_values` 使用済み。→ `apply_reduction_op` の operator-body dedup は完了。
  - [~] **Phase 3 groundwork (#2725)**: genuine-fork メソッドの native 折込。`%`-chain ブロッカーの根本原因を
        特定（上の Category B 参照 = `Value::Pair` の named 束縛）し `pair_as_positional` を追加・`.first` を修正。
        **残（大作業・未着手）**: ① native/VM sort 移行 + `dispatch_sort` 削除（`pair_as_positional` で解禁済み・
        高 blast-radius）、② `comb`(正規表現 matcher)/`substr`/`split`(named args) の native 折込、
        ③ pointy/`*.code` matcher の `.first`（smart_match 経路 topic 未束縛、別根）。
- [ ] **正規表現の validator/matcher 二重実装の統合**（[ANALYSIS.md](ANALYSIS.md) §3.1。重複の一種）。

### 最終ゴール
- [ ] **Interpreter のメソッド/関数実行パス（`call_function`/`call_method_with_values`/`dispatch_*` の
      重複実装）を削除**し、フォールバック率 0%（Test/EVAL 等の本質的例外を除く）にする。これが完了すると、
      env を任意の名前で書く唯一の存在（interpreter ブリッジ）が消えるので `env_dirty`/`ensure_locals_synced`/
      `sync_locals_from_env`/`saved_env_dirty` の dual-store 機構も削除できる（レバー B 完遂）。
- **残フォールバック**には `// TODO: compile to bytecode` を付け負債を可視化。
- **次の着手候補（優先順）**: Category A 完了（純粋値 builtin の重複ゼロ）。Category B の**ブロック系 fork
  （sort/min/max/minmax/first）は完了**（#2727/#2728/#2730/#2731、上記参照）。次は Category B の残り
  **lazy-fork（join/flat/elems/reverse）の drift reconcile → 委譲** → Category C Phase 3 の残（comb/substr/split
  折込）→ 🟣第2優先「第一級コンテナ」（レバー C 本丸 + Q2 の Arc-pointer flaky を吸収）。
  - 教訓: 「重複削除」を始めると**潜在バグが芋づる式に出る**（`[%] 2**70` 精度・`[~]` NFC・`[minmax]` ネスト・
    `%h.first` の named 束縛は全て dedup 作業中に発見・修正）。重複は drift してバグの温床になる実例。

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

### 段階導入（big-bang 回帰を避ける順序）

- [~] **Phase 0 — decont チョークポイント整備（挙動不変リファクタ）**: 実装ログは
      [docs/container-identity.md](docs/container-identity.md)。
  - [x] **ヘルパ統合（#2736/#2737/#2738, 2026-06）**: 散在する decont 展開を各軸の正規ヘルパへ集約。
        3 軸（Scalar `$(...)` / ContainerRef `:=` / ArrayKind itemization）は別の型・別の意味論なので
        **融合せず**（融合は `is rw` writeback の Pair 判定・`@a=$l` 平坦化を壊す）。`descalarize`/
        `into_descalarized`（Scalar 再帰）、非 clone の `with_deref`（ContainerRef 読み・`deref_container`
        を再定義）を制定。挙動不変を確認。
  - [ ] **残: スタック不変条件＋lvalue opcode（= Phase 0.5）**: 値スタックを「常に decont 済み」とし
        `GetArrayVar`/`GetHashVar`/`Index` の auto-deref と push 集約、lvalue 専用 opcode
        （`GetLocalContainer`/`IndexContainer` 等）を追加。`GetArrayVar`/`Index` の push 内容が変わる
        **実挙動変化**を含むため、コンテナが要素に載る Phase 1 と一緒に入れる。
- [ ] **Phase 1 — スカラーの第一級コンテナ化**: `$` 変数が `Arc<Scalar>` セルを持つ。`=` はセルへ格納、
      `:=` は束縛差し替え、itemization はセル wrap。→ 束縛/平坦化（reduce.t 62）, `=:=`/`.VAR`,
      兄弟クロージャ共有（レバー C 完了）, S02 変数捕捉, S03-binding を解消。
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

- [ ] 正規表現の validator/matcher 二重実装を単一パーサに統合 (§3.1)
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
