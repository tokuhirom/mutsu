# PLAN.md — mutsu 2026年ロードマップ

## 方針

**実用的な Raku インタープリタ**として使ってもらえる品質を目指す。
起動 25 倍速い強みを活かし、CLI ツール・スクリプティング用途をメインターゲットとする。
最終的には **mutsu でウェブブログが作れる** レベルのライブラリ互換性を実現する。

過去の実装状況は [news/](news/) を参照。
パフォーマンス詳細は [PERFORMANCE.md](PERFORMANCE.md) を参照。
roast ブロッカー分析は [TODO_roast/BLOCKERS.md](TODO_roast/BLOCKERS.md) を参照。

---

## 🔴 最優先: バイトコード VM をちゃんと治す

**roast テストを1件ずつ潰すより、VM アーキテクチャの根本改修を最優先する** (ユーザー方針 2026-06-03)。

mutsu の「バイトコード VM」は実態として tree-walking Interpreter の薄いフロントエンドであり、
VM は Interpreter を共有実行状態コンテナ + フォールバック先として使っている
([ANALYSIS.md](ANALYSIS.md) §1)。これを **strangler-fig 方式**で段階的に切り離す:
古いフォールバックを残したまま計測し、毎 PR で「フォールバック率 X%→Y%」を可視化しながら縮める。
進捗台帳は [docs/vm-decoupling.md](docs/vm-decoupling.md)（dispatch）と
[docs/vm-dual-store.md](docs/vm-dual-store.md)（locals↔env）。

**これはアーキテクチャ改善 = 結合削減が目的であり、パフォーマンス改善ではない**（perf は副次的、
fib が速くなるかでは判断しない。ユーザー方針 2026-06-04）。CI（`make test` + 包括的 `make roast`）が
全マージをゲートするので、本質的リファクタは小さく刻みすぎず**大胆に**やり、CI を安全網にする
（CLAUDE.md「Refactor boldly」）。

### 計測（done）
- [x] メソッド/関数ディスパッチのフォールバック率計測 + **関数名・メソッド名別 histogram**
      (`MUTSU_VM_STATS=1`、PR #2571/#2601/#2604)。どの builtin/method がフォールバックしているかを
      推測でなくデータで特定できる。

### レバー A: ディスパッチのフォールバックを native 化（フォールバック率を下げる）
**実質完了**（native 化: sprintf/accessor/.new/.map/.grep/.subst/.sort/Test ディスパッチ層、
TAP 状態の `TapState` カプセル化 #2659、EVAL/pseudo-package を carrier 分類）。詳細は news/2026-06.md。残:
- [ ] **（低優先・deferred 負債）コアに焼き込まれた Test 専用ディスパッチの撤去**。Test *だけ*がコアに名前を
      知られている（`is_test_function_name()` の ~50 関数名ハードコード + VM ディスパッチ4ファイルの Test 固有分岐）。
      消す道は2つ（最終ゴールは Test を本質的例外と許容）: ①generic native-module table 化（`use` 時に関数名を
      汎用テーブル登録、コアから Test 知識撤去・安い）/②Test-as-Raku-module（`Test.rakumod` を mutsu 上で eval・
      純粋だが大事業）。当面着手しない。
- 既知 pre-existing バグ（別途）: `@a.grep(...)[i] = v` の lvalue index 代入 / `my @g=@a.grep(...); @g>>++` の
  `=` 後 hyper writeback が元を更新（Arc-pointer keyed binding が `=` を跨いで残存）。完全な rw aggregate
  binding（配列要素の共有コンテナ化）が要る。

### レバー B: `locals`↔`env` 二重ストアの解消（共有状態結合の本丸, §1.2）
完了分（Slice 1〜6.2 = scoped/overlay env への全パス変換 + dirty フラグ機構撤廃、multi-tier overlay #2684、
Slice 6.3 stage 1/1b = メソッド dispatch 毎回の pull 撲滅 #2689/#2692）は news/2026-06.md 参照。残タスク:

- [ ] **Slice 6.3 残り（= `env_dirty` フラグ削除 = レバー B 完遂）**。フラグはまだ存在し、稀な
      interpreter-bridge 用の安全網として残る。**詳細な再開マップは docs/vm-dual-store.md「NEXT SESSION —
      resume map」**（debug ビルドで `MUTSU_VM_STATS=1` + benchmarks/*.raku 実測）。順序:
      - (A) **regex/smartmatch の pull — 完了**（bench-string `locals_pulls` 5003→3、repro 5001→1）。
        `exec_smart_match_expr_op`（vm_comparison_ops.rs）の blanket `env_dirty=true` を撤去。caller local を
        別名化しうる by-name 書き込みは `_`（match 後に元値へ復元＝slot と一致）と `var_name`（`update_local_if_exists`
        で slot へ reverse-write-through）のみ。`$/`/`$0`/`$<>` は `sync_regex_interpolation_env_from_locals` の
        skip リスト通り caller local を別名化しない。pin `t/smartmatch-env-dirty.t`（14）。
      - (B) **0-arg 関数 fast path の pull — 完了**（`for ^5000 { $c += f() }` locals_pulls 5001→2、
        deep_copies 不変、fib 不変）。`CallFunc` 0-arg fast path（vm_call_func_ops.rs）の blanket `env_dirty=true` を撤去し、
        `call_compiled_function_fast`（vm_call_dispatch.rs）が精密化: locals 有りは既存の overlay/clone merge、**0-local は
        コンパイル時 `cf.code.has_env_writes` ゲート**（env 書き込みなし＝caller slot を汚せない＝pull 不要）。
        ※ 0-local に overlay を被せる案は routine-`_` 挿入が make_mut を誘発し deep_copies 1→5000 で逆効果（不採用）。
        pin `t/zeroarg-env-dirty.t`（16）。
      - (C) **①heavy named dispatch path — 完了**（`for ^5000 { $c += g(2) }` で default param/return type/where/typed/multi
        callee の locals_pulls 5001→1）。`call_compiled_function_named`（vm_call_dispatch.rs）が return merge から env_dirty を
        精密に設定（caller-slot を別名化する plain-lexical/`is rw` writeback の実変化、または `is raw` return 時のみ true。
        動的変数 writeback は slot を持たないので除外）。hot call 元（vm_call_func_ops heavy/OTF/cached named、vm_var_get_ops
        bareword-term named）の blanket マークを撤去。EVAL/carrier 元（vm_call_exec_ops）は step3 なので保守的に残す。
        pin `t/named-call-env-dirty.t`（16）。
      - (C') **残り step2（次の着手）**: ②genuine by-name write の clean drop / reverse-write-through（vm_data_ops:418 は
        push が既に slot 復元済み＝冗長、vm_register_ops:824/908 は `locals_set_by_name` 済み＝冗長、vm_var_assign_ops:4018/4042 は
        `:=` 要素 SlotRef）。安全だが cold。③ループ/control の env 往復（vm_control_ops 7 / vm_misc_ops let-temp）— 要個別分析、リスク高、後回し。
      - (D) step3: I/O(Say/Put/Print/Note round-trip) + EVAL/atomic carrier。interpreter bridge 撤去（lever A / Q2）に gated。
      - 全 setter 消滅後に `env_dirty`/`ensure_locals_synced`/`sync_locals_from_env`/`saved_env_dirty` を削除。
        metric: `MUTSU_VM_STATS=1` の `locals_pulls`→0（bench-string 含む、出力不変）。
      - ※ bench-array/array-ops の `env_deep_copies` は `.map`/`.grep`/`.sort` クロージャの per-element overlay
        deep-copy＝**レバー C 領域**（env_dirty ではない）、ここでは扱わない。

### レバー C: クロージャ upvalue 化（§1.3）
完了分（Slice 4a / scoped-overlay によるサブゴール#2 / for ループ変数 per-iteration 捕捉 / upvalue Slice 1・2・2b
= loop-body-local の per-iteration 捕捉と box-on-capture / Slice 3 = loop body-local `my` clobber 修正）は
news/2026-06.md 参照。残タスク:
- [ ] **残（延期・大）: 非ループの一般コンテナ捕捉**（`my ($g,$s)=make(); $s(42); $g()` → raku `42`、
      `my $x=1; my $c={$x}; $x=2; $c()` top-level → `2`）。コンパイラ解析（`captured_mutated_locals`）は非ループも
      検出できるが、**非ループ被捕捉スカラを box すると ContainerRef 未対応の多数経路を踏む**（型付き `my A $a`
      の `.=`、isa-ok、subset 型オブジェクト代入、submethod state、Mix 不変性 — 「値検査サイトに ContainerRef が
      漏れる」共通根）。全 lvalue/型/dispatch 経路の deref 監査が要る＝専用の複数 PR。`t/closure-container-capture.t`
      の非ループ兄弟 2 ケースは `todo`。
- [ ] **Slice 3b（残）: `if`/`else` ボディローカル `my` の clobber**（`my $x=99; if c { my $x=5 }; say $x` が
      raku `99`、mutsu は `5`）。`if`/`else` を BlockScope（全 env 復元）でラップする案は**ブランチ内からの `:=`
      束縛を巻き戻す**回帰（`roast/S03-sequence/exhaustive.t` 27 subtest 失敗）を生む。正解＝loop と同じ
      shadow-only 復元を `if` ブランチにも効かせる軽量機構（`Push/PopBlockLocalScope` opcode で
      `loop_local_saved_env` を流用、全 env 復元はしない）。`t/block-local-my-scope.t` の if/else は todo。別 PR。

### 最終ゴール
- [ ] メソッド/関数フォールバック率を 0%（Test/EVAL 等の本質的例外を除く）にし、Interpreter のメソッド/関数
      実行パスを削除。残フォールバックには `// TODO: compile to bytecode` を付け負債を可視化。
- **次の着手候補（優先順）**: B の Slice 6.3 残り（regex pull → 残 by-name op → carrier、最後に `env_dirty` 削除）
  → C の非ループ一般コンテナ捕捉 ／ if-else body-local `my`（Slice 3b）→ 🟣第2優先「第一級コンテナ」Phase 0。

---

## 🟣 第2優先（インタープリタ廃止の次）: 第一級コンテナ (container identity) への移行

**優先順位**: 上の「🔴 最優先 = バイトコード VM をちゃんと治す（tree-walking Interpreter 廃止）」を
**第1優先**、本セクションを**第2優先**とする（ユーザー方針 2026-06-06）。両者は独立ではなく地続き
（レバー B/C の本丸が本移行の前提・一部）なので、インタープリタ廃止の完了を待ってから本格着手しつつ、
その尾部（レバー C upvalue 等）と自然に接続する。

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

- [ ] **Phase 0 — decont チョークポイント整備（挙動不変リファクタ）**: 値スタックの不変条件を「常に
      decont 済み」とし、全読み出しをそこへ集約。lvalue 専用 opcode（`GetLocalContainer`/`IndexContainer`
      等）を追加。コンテナはまだ殆ど存在しないので**挙動は不変** ＝ roast 完全一致で検証。単一サイトを
      先に確立する（以後の Phase の安全網）。
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
