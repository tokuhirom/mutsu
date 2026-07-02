# roast ブロッカー再評価メモ

この文書は、roast の失敗を「テストファイル単位」ではなく
**根本原因単位**で追うための索引。
個々の失敗を片端から潰すためではなく、
**今どこを直せば何がまとめて動くか**を判断するために使う。

**最終更新 2026-07-01**（2026-06-29〜07-01 の大量の完了項目を `news/2026-06.md` /
`news/2026-07.md` に移し、残件のみを再集計した版）

## この文書の読み方

- ファイル名の羅列ではなく、**根本原因単位**でブロッカーを追う。
- 各ブロッカーは次の粒度で書く:
  - **根本原因**: 何の抽象が欠けているか
  - **変更レイヤ**: parser / compiler / VM / runtime / value 表現 / scheduler のどこを触るか
  - **Next slice**: 次に何を切り出して着手するか
  - **完了条件**: どこまで行けば、その類の roast がまとめて動くか
- **whitelist 済みになった項目はこの文書から削除し、詳細は `news/` に移す。**
  このファイルは常に「現在まだ開いている残件」だけを持つ。
- 個別の per-file 詳細ログは `TODO_roast/S*.md` を参照。このファイルはそちらの要約と
  優先順位づけに徹する。
- **このファイルは頻繁に複数セッションから並行更新される。** 着手前に必ず
  `MUTSU_FUDGE=1 prove -e target/debug/mutsu roast/<path>.t` で実際の pass/fail 数を
  取り直すこと。ここに書かれた数字は執筆時点のスナップショットであり、数時間で
  古くなることがある。

## 現在の前提

- whitelist は **1338**（2026-07-01 時点、`4c311c71`、`wc -l roast-whitelist.txt`）。
- 安い 1 ファイル勝ちはほぼ枯渇している。残件の大半は少数の根本原因に集約される:
  1. **第一級コンテナ / container identity** — 配列・ハッシュ要素・属性 slot の書き戻し、
     BEGIN/EVAL 時の lexical 永続化。
  2. **真の lazy 配列 / 無限列** — 残りは `S09-subscript/slice.t` のみ。
  3. **dispatch / 演算子 sugar の desugar surface** — 残りは `hyper.t` のみ
     （`assign.t` は完了、詳細は `news/2026-07.md`）。
  4. **並行実行基盤（S17）** — 大半は片づいたが、`Lock.protect` に本物の
     race condition が残っている。

---

## 1. Hard — 単体では動かせない、大きな下位基盤待ち

### 1.1 Unicode / RakuAST / Collation（低 ROI・据え置き）

- `S32-str/CollationTest_NON_IGNORABLE-3.t`（1367/1369、test 1161, 1171 が失敗）
  - **根本原因**: ICU4X が BMP センチネル noncharacter（U+FFFE/U+FFFF）を特別扱いする一方、
    UCA-17/MoarVM は全 noncharacter に符号位置由来の implicit weight を付与するため。
    mutsu は符号位置を正しく保持しており、差異は ICU4X collator 内部のみ。
  - **評価**: 正しい修正には DUCET ベースの collation 実装差し替え（pure Rust の `feruca` 移行
    または自前実装）が必要。2 ケースのために大仕事かつ per-strength API の互換性リスクもあり、
    低 ROI のため据え置き。着手するなら `feruca` を throwaway で実測してから判断する。
- `S32-str/format.t`（reachable 26/49 は全 pass、test 27-29 で中断）
  - **根本原因**: `Formatter::Syntax.parse`→Match、`Formatter.CODE`→Callable、
    `Formatter.AST`→`RakuAST::Node` を要求し、ここで runtime error 中断するため以降が到達不能。
    本質は **RakuAST サブシステム不在**（参照 raku 本体すら `Format` 6.e 未対応）。
  - **評価**: RakuAST 本実装なしには whitelist 不可。stub 化は禁止のため据え置き。

---

## 2. 局所修正で進む残り

### 2.2 `S02-types/generics.t`

- **現状**: 0/1（変化なし）。
- **論点**: 6.e の coercion type 項 + `Array[T]` サブクラス化が必要。ローカル raku
  (v2022.12=6.d) でもこのテスト自体がコンパイルに失敗するため、参照実装での検証すらできない。
- **評価**: 深い機能待ちで、局所修正では進まない。優先度は低い。

### 2.3 multislice lvalue（array/hash）

- **対象**: `S32-array/multislice-6e.t`（784/812、28 失敗）、
  `S32-hash/multislice-6e.t`（318 実行時点で plan mismatch、90 失敗——ファイルが途中で中断）。
- **論点**: `@a[0;0;0] = 999`、`%h<a;b;c> = 999` の lvalue 書き戻し経路。
- **根本原因**: single subscript と multislice が同じ「書き戻し可能 target 集合」抽象を
  返していない。array 側はかなり前進した（605→157→28 まで縮小）が、hash 側はまだ
  Track B（要素 cell 化・ADR-0001 層3a・GC と統合キャンペーン）待ちで大きく残っている。
- **変更レイヤ**: `vm_var_index_ops` / `vm_var_assign_ops` / lvalue binding surface
- **Next slice**: array 側の残り 28 件から着手し、hash 側の plan mismatch（中断の原因）を
  先に切り分ける。
- **Primary files**: `src/vm/vm_var_index_ops.rs`, `src/vm/vm_var_assign_ops.rs`
- **調査結果（2026-07-02）**: array 側の 28 失敗（例: test 245, line 120）を再現したところ、
  直接代入 `@array[0;0;3] = 999`（境界外インデックスへの自動延伸）自体は正しく動く：
  ```
  my @array = [[[42,666,[314]],],];
  @array[0;0;3] = 999;  # OK: [[[42, 666, [314], 999],],]
  ```
  失敗するのは、この**境界外・自動延伸が必要な多次元インデックス式を sigilless bind
  （`\target`）でサブルーチン引数として渡し、後から `target = 999` で代入する**ケース
  （テストの `assignable-ok(\target, \values, @result)` ヘルパーが使うパターン）：
  ```
  sub f(\target, \values) { target = values }
  f(@array[0;0;3], 999);  # mutsu: 何もしない（Nil を返す）; raku: 正しく延伸して代入
  ```
  つまり根本原因は「lvalue 書き戻し経路」一般ではなく、**§3 のコンテナ id キャンペーン
  （配列/ハッシュ要素の cell 化）そのもの**——多次元インデックス式が返す「書き込み可能な
  参照」が、値としてキャプチャされた後もオリジナルの配列への自動延伸込みの書き込みを
  実現できる第一級コンテナ（cell）になっていない。§2 の「局所修正」の範疇を超え、§3.2 の
  「配列/ハッシュ要素 cell 化」（`docs/container-identity.md`、対象に `splice.t` と並記）と
  同一の基盤工事が前提条件。単体の parser/dispatch 修正では閉じない。
  次に着手するなら §3 のキャンペーンとして本腰を入れる想定で計画すること。

### 2.4 `S06-operator-overloading/infix.t`

- **現状（2026-07-02 更新）**: 48/50（残り 2 失敗: test 31-32 のみ）。test 11, 33 は
  rakudo 自身が divergent としている既知の TODO で数に入らない。
- **経緯**: `import ClassName` 経由の演算子メソッド export（#3982）→ 35/42 → LTM 拡張・
  `is assoc('non')`・user-declared `infix:<+>`/`infix:<+=>` fast-path override・
  `infix:OP:[...]`/`infix:OP:«...»` 呼び出し構文で 42/44 → `&infix:<<$var>>`/`«$var»`/
  `[$var]`（interpolating/expression な演算子名参照。詳細は `TODO_roast/S06.md`）の
  ランタイム解決を追加して 48/50 まで前進。
- **残り (test 31-32)**: `sub infix:<,>($a,$b){...}` によるカンマ演算子そのものの
  オーバーロード（EVAL 内限定）。カンマは list/引数区切りとしてパーサ全体に
  ハードコードされた特殊トークンであり、他の user 演算子のような汎用 dispatch
  経路に乗っていない。real raku でも EVAL の外（同一ファイルのトップレベル）では
  効かない——EVAL は新しいコンパイル単位として、宣言済みの演算子テーブルを
  引き継いで再パースするため機能する、という compile-time な仕組み。
- **評価**: 深い・リスクの高いアーキテクチャ変更（カンマを他の user 演算子と同じ
  拡張可能テーブル経由にする必要があり、あらゆる箇所の区切り用途を壊さずに行う
  必要がある）。安易に着手しない。whitelist するにはこの 2 件を解決する必要がある。
- **Canary**: `roast/S06-operator-overloading/infix.t`

---

## 3. 第一級コンテナ / container identity

`S32-array/splice.t` の self-splice、`S02-types/whatever.t` の container preservation、
`S14-traits/attributes.t` の per-attribute Scalar container など、
element/attribute slot の書き戻しが未整備な項目が集まっている。
すでに whitelist 済みの項目（`temp.t`、`adverbs.t` の typed-hash default、`capture.t`、
`is_default.t`、`walk.t` など）は `news/2026-06.md` を参照。

### 3.1 残っている対象

- `S02-names-vars/variables-and-packages.t`（30/39、9 失敗: test 29-34, 37-39）
  — 全 9 失敗が独立した深い lexical/scoping/BEGIN 問題。2026-07-02 に各グループを root-cause 精査した結果:
  - **test 29-31（BEGIN block init）**: `{ baz(); ...; my $a; BEGIN { $a = 3 }; sub baz { $a++ } }` が
    3,4,5 でなく **0,1,2**。`BEGIN { $a = 3 }` が compile-time に走らないため `$a` が 3 に初期化されない。
    → BEGIN compile-time execution（declaration model 再設計・medium-large）。
  - **test 32-34（escaped `our sub`）**: `{ my $a = 3; our sub grtz { $a++ } }; &OUR::grtz()` が
    3,4,5 でなく **0,1** / **0,0,1**。★**block 内で呼べば 3,4 で正しい**が、block 退出後は 0。
    根因＝`escaped_our_lexical_cells["a"]` が **0-seed** の cell を保持（0→1 と increment するので cell 自体は
    persist されている）。`my $a=3` を実行する code frame の `needs_cell_escaping_our_sub` に "a" が入って
    おらず（nested block/closure の **code レベルで cell 分類が別 frame に落ちている**＝`nceos` vs `nceos_free` の
    own/free 判定、opcode.rs:2129-2147）、assignment 時 boxing の persist branch（vm_var_assign_set_local.rs:54）
    が発火しない。hoisted RegisterSub が `my $a=3` 前に空 cell を persist し、in-place persist が cell(3) で
    上書きできていない。★assignment 時に persist を足す fix（試行済）は branch 非発火で**無効**＝
    `collect_escaping_our_lexical_names`（accessors_state.rs:137・`_free` を読まない）と box_decl 両方の
    code-level 分類を直す必要。
  - **test 37-38（X::Redeclaration::Outer）**: `sub s($i is copy){ for ...{ @a.push($i); my $i=1 } }` が
    silent（raku=compile-time「Lexical symbol '$i' is already bound to an outer symbol」）。
    → block 内で outer 名を **使用後に** `my` 再宣言する use-before-decl の compile-time scope 解析が必要
    （既存の same-scope 重複検出とは別軸）。
  - **test 39（`$OUTER::_`）**: `for "a" { $t = sub { $OUTER::_ } }; $t()` が 'a' でなく **Nil**。
    **2 バグ**: ①`$OUTER::x` は `GetOuterVar`→`get_outer_var`（vm_var_get_ops.rs:357）が closure の captured
    env でなく `outer_scope_locals`（lexical block nesting 用）を見るため closure 呼び出し時に空→Nil
    （skip-own-scope セマンティクスも要）②`for "a" { sub {$_} }` 自体が topic `$_` を capture できず `(Any)`。
  - ★全て load-bearing な closure-capture / declaration-model に触る高リスク変更。盲目的な部分 fix は避け、
    1 グループずつ専用セッションで攻めること。最も infra が揃うのは 32-34（`escaped_our_lexical_cells` 機構）。
- `S14-traits/attributes.t`（4/8、bad plan：8 planned に対し 4 で中断）
  — `Attribute.container.VAR does Role` に必要な「scalar 属性が Scalar コンテナを VAR として
  持ち、合成時に role を mixin し、それがインスタンスの per-attribute コンテナに伝播する」
  という深いコンテナ表現。mutsu の scalar 属性は値を直接保持し VAR は Any を返すため、
  属性 slot の cell 化（本項の Primary files 参照）待ち。`$my_ref := $obj.attr`
  （scalar accessor への `:=` 束縛）も同根の残課題。
- `S12-subset/subtypes.t`（83/90、7 失敗: test 25, 68, 77, 83, 87-89）
  — where-block placeholder、expr-position constraint persistence、capture where の一部。
- `S02-types/whatever.t`（122/130、8 失敗: test 111, 119-124, 126）
  — WhateverCode の over-currying（CallOn-arg branch で握りすぎる）。他ケースを壊さない
  narrow fix が必要。
- `S32-array/splice.t`（357/392、35 失敗）
  — self-splice / push-replace-self が true first-class element container を要求する
  container identity の canary。
- `S26-documentation/12-non-breaking-space.t`（1/2、test 2 が失敗）
  — 2026-07-02 に root-cause 確定＝**BEGIN が compile-time でなく source 順 runtime 実行**される問題。
  test は末尾 `BEGIN { @nbchars = [...]; @bchars = [...] }` で配列を設定し、冒頭 line 8
  `my $nbchar-count = @nbchars.elems` で読む。raku は BEGIN を compile-time 実行するので line 8 が読む時
  @nbchars は既に 4 要素だが、mutsu は BEGIN を line 108 の位置で実行→line 8 の read が pre-BEGIN(空)を見て
  `$nbchar-count = 0`→`plan 0+1`＝`plan 1`＋`for 0..^0`＝0 反復で subtest が 0 test 実行→fail。
  最小再現: `my @a; my $c = @a.elems; BEGIN { @a = 1,2,3 }; say $c` が mutsu=0/raku=4。
  ★注意: `my @a; BEGIN { @a = 1,2,3 }; say @a.elems`（read が BEGIN の**後**）は mutsu も 3 で正しい
  ＝問題は read が BEGIN より**前**の時のみ。
  **試行と blocker**: 冒頭 `compile()` に「top-level BEGIN と bare `my` decl を front に hoist」する
  `hoist_begin_phasers` を実装したが、**identity reorder（順序保持の to_vec）は 3 で正しいのに、
  bucket-reorder（bare_decls+begins+rest に分割再結合）は interspersed した `SetLine` 等も動かすため
  以前 pass していた `my @a; BEGIN` すら 0 に壊す**（compile-order / local-slot / SetLine 依存が判明）→revert。
  正攻法は AST 再配置でなく **BEGIN body を compile-time に sub-interpreter で eval して初期状態に反映**
  （raku 準拠）か bytecode-level の BEGIN prelude。declaration-model と同じ深い層で、専用セッション要。
  [[project_declaration_model_hoisted_cell_clobber]] / [[project_begin_compile_time_timing]] と同族。

### 3.2 サブキャンペーンと choke point

1. **配列/ハッシュ要素 cell 化**
   - 依存文書: `docs/container-identity.md`
   - 変更レイヤ: `value/mod.rs`、`vm_var_assign_ops.rs`、`vm_var_index_ops.rs`
   - 対象: `splice.t`, multislice（§2.3）
   - 完了条件: element bind / take-rw / deep nested write が post-call writeback なしで成立
2. **属性 accessor を value copy ではなく slot 経由にする**
   - 変更レイヤ: attribute read/write path、instance attr storage
   - 対象: `S14-traits/attributes.t` の残り（`Attribute.container.VAR does Role`）
3. **BEGIN/EVAL/lexical 配列変更の永続化**
   - 変更レイヤ: env/local coherence、block escape、BEGIN 実行時 lexical carrier
   - 対象: `S26-documentation/12-non-breaking-space.t`、
     `S02-names-vars/variables-and-packages.t`

コード choke point:

- 要素 write 集約: `src/value/mod.rs::assign_element_slot`, `src/value/mod.rs::hash_insert_through`
- 要素 read decont: `resolve_array_entry`, `resolve_hash_entry`
- whole-container / env-local coherence: `docs/env-locals-coherence.md`,
  `src/vm/vm_env_helpers.rs`, `SetLocal` / `flush_local_to_env` / bound-container metadata

- **Next slice**: `splice.t` の self-splice ケースから、配列 write を helper 経由へさらに
  寄せて post-call writeback 依存を 1 段減らす。
- **Canary tests**: `roast/S32-array/splice.t`, `roast/S14-traits/attributes.t`,
  `roast/S02-names-vars/variables-and-packages.t`

---

## 4. 真の lazy 配列 / 無限列

`eqv` の both-lazy ガードと `+a`/`+@a` の Seq single-pass consumption は完了済み
（詳細は `news/2026-06.md`）。残るのは 1 ファイルのみ。

### 4.1 `S09-subscript/slice.t`

- **現状**: 32/56（24 失敗）。以前は途中で中断していたが、周辺の一般修正で最後まで
  実行が通るようになった。
- **論点**: sequence-literal-in-index、slice adverbs、lazy semantics が絡む。
  `docs/lazy-arrays.md` の L2b-L4 と同じ話 — Array が lazy を保ったまま
  mutation / slice / slurpy に耐えられない。
- **変更レイヤ**: `LazyList` 表現、`@`-assign preserve、mutation 時 reify、slice/index
- **Primary files**: `src/runtime/resolution_lazy.rs`, `src/runtime/methods_call_dispatch.rs`,
  `src/runtime/methods_type_coerce.rs`
- **Next slice**: 24 件の失敗を種類ごとに分類し、`docs/lazy-arrays.md` に沿って
  reify/mutation を進める。

---

## 5. dispatch cluster

`wrap.t` / `dispatching.t` / `qualified.t` / `assign.t` は完了済み（詳細は
`news/2026-06.md` / `news/2026-07.md`）。残るのは演算子 sugar が dispatch へ落ちるまでの
surface のうち 1 ファイル。

### 5.1 `S03-metaops/hyper.t`

- **現状**: 420 planned 全実行、3 失敗（test 362, 407-408）。plan mismatch は解消。
- **今回の進捗**: plan mismatch の原因は `<<op>>` 系ハイパーメタ演算子の閉じ `>>` 探索が
  演算子文字列自体と重なるケース（`<<=>>>` = `<<`+`=>`+`>>`）で誤って最短一致を採用し、
  以降のファイル全体を静かに打ち切っていたパーサバグだった（`hyper_concat.rs`）。
  副次的に `set_shared_var`（`@`/`%` への env 書き込み全経路を通る、hyper 専用ではない
  一般関数）が非スレッド文脈でも List→Array を無条件正規化しており、`:=` で束縛した
  List の kind を最初の env sync で破壊していた一般バグも発見・修正。さらに `is_listy()`
  （hyper 分配の判定）に `Value::Slip` が抜けており `|<1 2> >>xx>> 2` が per-element
  分配されない不具合、および `.i`（postfix imaginary）が dotted 呼び出し
  （`4.i`/`@a».i`、本来 X::Method::NotFound）と bare 呼び出し
  （`4\i`/`(expr)i`/`@a»i`、`&postfix:<i>` 演算子）を区別できていなかった不具合を修正。
  詳細は `TODO_roast/S03.md` の hyper.t エントリと news 参照。
- **残り 3 件**: (1) test 362 ユーザ定義 custom infix 演算子とビルトイン演算子文字の
  字句衝突（`infix:<+-*/>` の `*` が Whatever と誤認）、(2)(3) test 407-408 `».+`/`».*` の
  all-candidates MRO dispatch 未実装（非 hyper の `.+`/`.*` でも同じ欠落を確認済み —
  mutsu のビルトイン型は List/Any/Cool のような多段クラス階層に渡る重複メソッド定義を
  モデル化していない、根本的なアーキテクチャギャップ。個別メソッドの候補数を
  ハードコードするのは禁止されているテスト特化ハックそのものなので不可）。
  いずれも局所修正では閉じない、それぞれ独立した深掘りが必要な項目。
- **変更レイヤ**: `src/parser/expr/precedence_meta_ops/hyper_concat.rs`,
  `src/vm/vm_hyper_method_ops.rs`, `src/vm/vm_hyper_ops.rs`,
  `src/runtime/runtime_shared_vars.rs`, `src/parser/expr/postfix/loop_.rs`,
  `src/parser/primary/number.rs`, `src/runtime/builtins_operators_fallback.rs`
- **Next slice**: 残り 3 件はいずれも大きめの独立作業（custom operator の longest-match
  字句解析、MRO 多重候補 dispatch の新規実装）。次に着手するなら (2)(3) の
  all-candidates dispatch が最も汎用的（他の `».+`/`».*` 系テストにも波及しうる）が、
  スコープはこのファイル単体では収まらない見込み。

---

## 6. 並行・非同期（S17）

大半は完了した（semaphore.t、then.t、scheduler/basic.t、supply/migrate.t、supply/stable.t、
S17-promise/start.t、S07-hyperrace/basics.t、S17-lowlevel/cas-int.t —
詳細は `news/2026-06.md` / `news/2026-07.md`）。残るのは次の 2 ファイル。

### 6.1 `S17-lowlevel/lock.t` — 本物の race condition

- **現状**: 3 planned に対し TAP 順序異常＋2 失敗。`Thread .join` を実スレッド join に
  ルーティングする修正（#4026）が landed した後も再現する。
- **症状**: `Lock.protect` 配下で複数スレッドが共有カウンタを increment するテストで、
  期待列（10, 20, 30, ..., 1000）に対し実際の出力から特定のインクリメントが欠落したり
  重複したりする（例: `420` が `0` になる、`10` が丸ごと消える、`710` が二重に出る）。
  これは shared-scalar coherence の未解決部分であり、`S17-lowlevel/semaphore.t` で行った
  `call_protect_block` 型の reconcile/writeback への寄せが `Lock` 自体にはまだ
  適用されていない可能性が高い。
- **変更レイヤ**: `shared_vars` / `shared_vars_dirty`、`Lock.protect` の critical section
  entry/exit
- **Primary files**: `src/runtime/native_methods/state_lock.rs`,
  `src/runtime/runtime_shared_vars.rs`
- **Next slice**: `Lock.protect` を `call_protect_block`（既存の resync/writeback
  chokepoint）へ実際に寄せられているか確認し、寄せられていなければ semaphore.t の修正
  （#3992）と同じパターンを適用する。TAP 順序異常は別に fire-and-forget スレッド出力の
  drain タイミングの問題の可能性がある。

### 6.2 `S17-supply/syntax.t`

- **現状**: 88/90（test 75, 90 が失敗）。根本原因分析は `TODO_roast/S17.md` に記録済み:
  - test 75: nested `whenever`（on-demand supply が `whenever Supply.interval {}` の
    内側にある場合）は react loop でなく非-react tap 経路（`native_supply_mut`
    tap/act）を通るため、その async body 完了時の `on_close_callbacks` 発火が
    react-loop 側の仕組みに乗らない。
  - test 90: 2 つの `whenever` がそれぞれ `last` する cross-whenever ordering — hard case、
    据え置き。
- **Canary**: `roast/S17-supply/syntax.t`

### 6.3 async IO / Proc::Async / socket supplies（変化なし）

- **対象**: `procasync/*`、`IO-Socket-Async*`
- **根本原因**: encoding、stdin/stdout/stderr supply bridge、socket/listener 駆動が未成熟。
- **変更レイヤ**: `methods_object_native_ctors_io.rs`、`native_methods/socket_*`、encoding layer
- **Next slice**: encoding 無しの `Proc::Async` happy-path を supply bridge と done/quit
  delivery まで揃える。
- **Primary files**: `src/runtime/methods_object_native_ctors_io.rs`,
  `src/runtime/native_proc_async.rs`, `src/runtime/native_methods/socket_async_conn.rs`

---

## 7. whitelist を目標にしない項目

### 7.1 rakudo 側も失敗する、または roast 側の問題が強いもの

ここは mutsu 側で一般改善が入るのはよいが、
**そのファイルを whitelist すること自体を目標にしない**。

- `S05-nonstrings/basic.t`
- `S05-metasyntax/angle-brackets.t`
- `S05-mass/rx.t`
- `S06-advanced/caller.t` — stale test・over-stated plan（22 planned vs 実質 19 assertion）。
  rakudo 自身の期待値と乖離しており unpassable と確定済み（#3975）。
- `S06-advanced/return_function.t`
- `S10-packages/require-and-use--dead-file.t`
- `S12-traits/parameterized.t`
- `S12-meta/exporthow.t`
- `S12-class/open_closed.t`
- `S32-str/sprintf.t`

### 7.2 低 ROI で後回しにすべきもの

- `S32-str/CollationTest_NON_IGNORABLE-3.t` — 2 ケースのために実装が重すぎる（§1.1 参照）。
- `S32-str/format.t` — `RakuAST` が無い限り最後まで通しにくい（§1.1 参照）。

---

## 8. 今のおすすめ着手順

「次に何をやるか」を 1 本だけ選ぶなら、順番はこう見るのが妥当。

1. **`S17-lowlevel/lock.t` の race condition**（§6.1）— semaphore.t で確立した
   reconcile/writeback パターンを `Lock` にも適用できれば早い。
2. **第一級コンテナ campaign**（§3）— `docs/container-identity.md` に沿って
   splice.t / attributes.t / multislice hash 側の slot identity を前に進める。
   これは腰を据えた基盤工事で、個々のテストを直接潰すより効果が大きい。
3. **`S03-metaops/hyper.t`**（§5）— plan mismatch の原因を先に特定してから、
   残りの失敗を分類して潰す。
4. **`S09-subscript/slice.t`**（§4）— 24 件を種類ごとに分類し、lazy array 基盤工事の
   一環として進める。

`S06-operator-overloading/infix.t`（§2.4）は残り 2 件（カンマ演算子オーバーロード）が
深いアーキテクチャ変更を要するため、上記優先順から外した。

whitelist を目標にしない §7 の項目は、mutsu 側の一般改善のついでに触れるのはよいが、
そのファイル単体を通すことを目的にしない。
