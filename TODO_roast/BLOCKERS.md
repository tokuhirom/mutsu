# roast ブロッカー再評価メモ

この文書は、roast の失敗を「テストファイル単位」ではなく
**根本原因単位**で追うための索引。
個々の失敗を片端から潰すためではなく、
**今どこを直せば何がまとめて動くか**を判断するために使う。

**最終更新 2026-07-03**（§4 `S09-subscript/slice.t` の Buf スライス代入
[test 31] を解消・再集計）

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
  3. **dispatch / 演算子 sugar の desugar surface** — 完了（`hyper.t`・`assign.t`
     とも whitelist 済み、詳細は `news/2026-07.md`）。
  4. **並行実行基盤（S17）** — `Lock.protect` の race condition は解決済み
     （§6 参照）。残るのは `S17-supply/syntax.t` のみ。

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

---

## 3. 第一級コンテナ / container identity

`S32-array/splice.t` の self-splice、`S02-types/whatever.t` の container preservation、
`S14-traits/attributes.t` の per-attribute Scalar container など、
element/attribute slot の書き戻しが未整備な項目が集まっている。
すでに whitelist 済みの項目（`temp.t`、`adverbs.t` の typed-hash default、`capture.t`、
`is_default.t`、`walk.t` など）は `news/2026-06.md` を参照。

### 3.1 残っている対象

- ~~`S02-names-vars/variables-and-packages.t`~~ — **DONE・whitelist 済み（2026-07-07）**。
  最後まで残っていた nested-block BEGIN（test 29-31: `{ is baz(),3; ...; my $a; BEGIN { $a = 3 }; sub baz { $a++ } }`
  が 3,4,5 でなく 0,1,2）を解消。root-cause＝ネストした bare block 内の `BEGIN` が compile-time に走らず
  source 順（reads の後）で実行されるため、BEGIN より前に textual に出る read（forward-declared sub 経由）が
  初期化を観測できない。修正（`src/runtime/phasers.rs::reorder_at_level`）は AST 再配置で BEGIN をブロック内の
  reads より前へ hoist するが、宣言/代入/初期化を跨がないよう厳しく gate する:
  - `reorder_recursive`/`reorder_at_level` に `is_top` を追加。top-level は従来どおり
    `run_toplevel_begin_phasers` に任せ、この pass では hoist しない。
  - ネスト BEGIN は「その BEGIN より前に read（Say/Call/…）があり、かつ barrier（宣言・代入・初期化子）が
    無い」場合のみ hoist（`first_read_idx < idx < first_barrier_idx`）。これで `class C; BEGIN {…C…}`
    （begin.t 7）・`has $.a; BEGIN {…set_build…}`（S12 defaults）・`my $x=42; BEGIN {…}; constant c=$x`
    （constant.t 27）の in-source 順序を壊さず、read-after BEGIN（begin-compile-time.t 11）も従来どおり
    in-place で動く。CHECK/INIT を持つブロックは従来挙動（全 BEGIN を先頭 bucket へ）を維持。
  回帰テスト＝`t/begin-nested-block.t`（`t/begin-compile-time.t` も回帰なし）。test 32-34（escaped `our sub`）・
  37-38（X::Redeclaration::Outer）・39（`$OUTER::_`）は #4235/#4305 等で既に解消済みだった。
- `S14-traits/attributes.t`（4/8、bad plan：8 planned に対し 4 で中断）
  — `Attribute.container.VAR does Role` に必要な「scalar 属性が Scalar コンテナを VAR として
  持ち、合成時に role を mixin し、それがインスタンスの per-attribute コンテナに伝播する」
  という深いコンテナ表現。mutsu の scalar 属性は値を直接保持し VAR は Any を返すため、
  属性 slot の cell 化（本項の Primary files 参照）待ち。`$my_ref := $obj.attr`
  （scalar accessor への `:=` 束縛）も同根の残課題。
- ~~`S12-subset/subtypes.t`（83/90、7 失敗）~~ — **DONE・whitelist 済み（2026-07-03 再検証）**。
  現状 92/92、唯一の `not ok 92` は `# TODO custom type checking on hashes NYI`（期待失敗）。
- ~~`S02-types/whatever.t`（122/130、8 失敗: test 111, 119-124, 126）~~ — **DONE（#4067、131/131）**。
  — WhateverCode の over-currying（CallOn-arg branch で握りすぎる）。他ケースを壊さない
  narrow fix が必要。
- ~~`S32-array/splice.t`（357/392、35 失敗）~~ — **DONE・whitelist 済み（2026-07-05）**。
  全 35 失敗は 1 原因＝whole-container 代入（`@a = ...`）がコンテナを**再束縛**していたこと。
  raku は `@a = ...` を**既存コンテナの in-place 更新**として扱うため、`(5,0,@a)` の
  ように list へ by-value で捕捉された `@a` 参照が後の再代入を観測できる。mutsu は
  `Value::Array` の backing `Gc` を置換していたので、捕捉側は旧 `Gc` を見続け self-splice
  で余分な要素が入った。修正（`vm_var_assign_set_local.rs` / `vm_exec_dispatch.rs` の
  SetLocal・SetGlobal 両経路）:
  1. plain な whole-container 代入（`=`、`my`/`:=` 以外）は既存 `Gc` の中身を
     `gc_contents_mut` で in-place 更新し、pointer identity を保つ
     （`array_inplace_reassign` / `hash_inplace_reassign`、自己参照は `new_gc`→`old_gc`
     へ張り替え）。
  2. raku の `=` コピー意味論を保つため、`my @b = @a` 等の**新規宣言/非in-place経路**で
     backing `Gc` が共有（strong_count>1）なら fresh `Gc` の shallow-copy へ detach
     （`detach_shared_container`）。これで `@b !=:= @a` になり、in-place 化しても
     コピー独立性が壊れない（従来は COW で偶然独立に見えていた既存の非互換も是正）。
  3. 匿名コンテナ（`my %`/`my @` は単一スロット名 `%__ANON_HASH__` を再利用）は
     in-place 対象から除外（各宣言は別 logical 変数）。pointy-block（`given @c -> @p`）
     退出時に `@p` のスロット値もクリア（alias 残留コンテナの in-place 破壊を防止）。
  回帰テスト＝`t/container-identity-whole-assign.t`。
  **残る深いサブケース（本項の対象外・別スライス）**: `@a` が compilation-unit 内で
  closure に capture されると shared-cell（`ContainerRef`）表現に切り替わり、その状態で
  list へ by-value 捕捉した参照は cell 更新を追えない（＝ sub の外で捕捉参照を読む
  `ident4` 系）。これは return-writeback / cell スナップショット問題で、[[project_declaration_model_hoisted_cell_clobber]]
  と同族。splice.t は sub 内で捕捉参照を読むため影響を受けず PASS。
- ~~`S26-documentation/12-non-breaking-space.t`（1/2、test 2 が失敗）~~ — **DONE・whitelist 済み
  （2026-07-06）**。root-cause＝**top-level BEGIN が compile-time でなく source 順 runtime 実行**される
  問題（`my @a; my $c = @a.elems; BEGIN { @a = 1,2,3 }; say $c` が mutsu=0/raku=3）。
  修正（`run_toplevel_begin_phasers`・`src/runtime/run_prelude.rs`）は AST 再配置ではなく BLOCKERS が推す
  「**BEGIN body を compile-time に sub-interpreter（`eval_block_value`）で eval → 共有 env に反映**」方式:
  1. `run()` 内で `reorder_phasers` の**前**（`preregister_top_level_subs` の後）に top-level の
     hoistable な `Stmt::Phaser{Begin}` を `eval_block_value` で実行し `body_main` から除去。
     `eval_block_value` は plain lexical 書き込みを共有 `env` に残す（`&`-callable キーのみ隔離）ので
     seed が永続する。reorder より前に消すことで「`my $c = @a.elems` init が BEGIN より前に
     bucket される」旧 revert の破綻を回避。
  2. hoist 後、seed された変数の no-init `my` 宣言（`Literal(Array/Hash 空)`・`Literal(Nil)`）を
     `body_main` から drop（`drop_seeded_noinit_decls`、`my (@a,@b)` の SyntheticBlock も 1 段降下）—
     raku 同様「container は compile-time に 1 度だけ作られ、runtime の宣言は再初期化しない」を再現。
  3. **hoistable gate は保守的**（`begin_body_is_hoistable`）: body に宣言・BareWord・Call のいずれも
     無いこと。理由＝`eval_block_value` は name 解決と sink-context のエラー forcing で mainline VM と
     意味論が乖離するため、シンボル解決や例外を起こしうる BEGIN（class 参照 / forward sub 呼び / `1/0.Int`）は
     hoist せず in-place 経路（`X::Comp::BeginTime` ラップ込み）に任せる。純粋な値代入 BEGIN のみ hoist。
  回帰テスト＝`t/begin-compile-time.t`（`t/begin-phaser-begintime.t` も回帰なし）。
  **nested-block BEGIN（`variables-and-packages.t` test 29-31）も DONE・whitelist 済み（2026-07-07）**：
  bare block **内**の BEGIN を、read の後・barrier（宣言/代入/初期化）の前という条件でのみ hoist する
  AST 再配置で解消（`reorder_at_level`、§3.1 参照）。

### 3.2 サブキャンペーンと choke point

1. **配列/ハッシュ要素 cell 化**
   - 依存文書: `docs/container-identity.md`
   - 変更レイヤ: `value/mod.rs`、`vm_var_assign_ops.rs`、`vm_var_index_ops.rs`
   - 対象: `splice.t`, multislice（§2.3）
   - 完了条件: element bind / take-rw / deep nested write が post-call writeback なしで成立
2. **属性 accessor を value copy ではなく slot 経由にする**
   - 変更レイヤ: attribute read/write path、instance attr storage
   - 対象: `S14-traits/attributes.t` の残り（`Attribute.container.VAR does Role`）
3. **BEGIN/EVAL/lexical 配列変更の永続化** — **DONE**。
   - top-level BEGIN 版（`S26-documentation/12-non-breaking-space.t`、`run_toplevel_begin_phasers`）・
     nested-block BEGIN 版（`S02-names-vars/variables-and-packages.t`、`reorder_at_level`）とも
     whitelist 済み（§3.1 参照）。

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
（詳細は `news/2026-06.md`）。lazy 配列そのものの基盤（L1-L2b、`docs/lazy-arrays.md`
参照）も完了済み。**`S09-subscript/slice.t` は 2026-07-04 に 56/56 で whitelist 済み**
（残っていた test 35 = nested slice adverbs を解決。詳細は `news/2026-07.md` /
`TODO_roast/S09.md`）。このセクションに開いている残件はない。

### 4.1 `S09-subscript/slice.t` — **DONE・whitelist 済み（2026-07-04）**

- **現状**: 56/56 PASS。
- **今回解決した根本原因**（詳細は `TODO_roast/S09.md` の該当エントリ）:
  1. `:=` bind が `sequence_spec`/`closure_seq`/`lazy_pipe` 系 `LazyList` を
     `coroutine` 以外は強制マテリアライズしていた（`vm_var_assign_set_local.rs`）。
  2. `LazyList::is_genuinely_lazy()` が「有限リストに `.lazy` を掛けた」ケース
     （cache のみで coroutine/body なし）を見逃し `.is-lazy` が `False` になっていた
     （`value_lazy.rs`）。
  3. `lazy`/`eager`/`hyper` 文prefix のパーサが後続のカンマリスト全体でなく先頭の
     1 項だけを対象にしていた深いバグ（`lazy 3,4,5` が `(3.lazy, 4, 5)` に、
     `lazy 1...*` が `(1.lazy)...*` にパースされていた）。`return` の
     `parse_comma_or_expr` と同様に全体を消費するよう修正
     （`parser/expr/postfix/loop_.rs`）。
  4. read 側でネストした `LazyList` サブインデックス（`@a[1,(lazy 3,4,5)]`）が
     再帰されていなかった（`vm_var_index_ops.rs`）。
  5. フラットなスライス代入（`@a[1,3] = ...`）を式コンテキスト（`say (...)` 等）で
     使うと env にしか書き戻らず locals とズレる dual-store bug
     （`vm_var_assign_index_named.rs`、early return が共有の locals-resync tail を
     skip していた）。
  6. ネスト lazy サブリストへの代入・bind（`@a[1,(lazy 3,4,5)] = "a"...*` や
     トップレベル `@a[lazy 1,2,(4,5)] = ...`）が未実装で panic していた——新設の
     `SliceKeyTree`（write pass `assign_slice_key_tree` ＋ 代入完了後に再読込して
     nested な返り値を組み立てる `read_slice_key_tree`。重複インデックスは
     「書き込み時点の値」でなく「最終状態」を返すため別パスが必要）で解消。
- **2026-07-03 解決**: test 31（Buf スライス代入 `$b[0,1] = 2,3`）。原因は
  Buf/Blob（`Value::Instance`）に対する index-assign 専用パスが存在せず、
  汎用フォールバックが「未初期化コンテナ」と誤認して `$b` をハッシュ
  `{0=>2, 1=>3}` で丸ごと上書きしていたこと。`InstanceAttrs::with_attr_mut`
  経由で共有 "bytes" セルへ直接書き込む専用分岐を追加（変数の再束縛不要
  ＝ Buf の identity は共有 attribute cell 側にあるため）。単一 index・
  スライス双方に対応、値は uint8 ネイティブ配列と同じ mod 256 マスク、
  範囲外は 0 埋めで自動延伸、immutable な Blob への書き込みは拒否。
  詳細・テストは `TODO_roast/S09.md` / `t/buf-index-slice-assign.t`。
- **2026-07-03 解決**: test 15（bound slice の固定 arity
  `@slice := @array[1,2]; @slice = <A B C D>` → "A B"）。当初は §3
  container-identity 相当の新 `Value` variant が要ると見積もったが、
  **既存の per-element `ContainerRef` cell 機構を再利用するだけで済んだ**
  （新 variant 不要）:
  1. コンパイラ（`stmt.rs`）: `@`-sigil の VarDecl bind の RHS が
     `Expr::Index{is_positional:true,..}`（単一/スライス問わず）のときも
     `scalar_bind_autovivify`+`bind_terminal` を立てて autovivify-lazy 経路
     へ通す（従来は `$`-sigil の要素 bind のみ対象だった）。
  2. VM（`vm_var_index_ops.rs`）: `exec_index_autovivify_lazy_op` の
     `terminal` 分岐に、index が単一 int でなく Array/Seq/Slip（スライス）
     の場合の枝を追加。各インデックスを既存の `array_slot_ref(idx, true)`
     で個別に `ContainerRef` セルへ昇格し、それらセルを要素に持つだけの
     **プレーンな** `Value::Array` を bind 結果として返す（セル自体は
     1 要素用のままで、スライスは「セルを持つ配列」という組み合わせで表現）。
  3. VM（`vm_var_assign_local.rs` / `vm_var_assign_set_local.rs`）:
     `@`-sigil ローカルへの**丸ごと**代入（式/文の両方）で、現在値が
     「要素に `ContainerRef` を含む配列」なら、コンテナを置き換えるのでなく
     **自身の要素数に固定されたまま**各セルへ write-through する新分岐を追加
     （余った RHS は破棄、不足分は `Any` 埋め）。`@`-sigil ローカルは
     `code.simple_locals[idx]` が常に `false`（`$`-sigil 専用フラグ）なので、
     この分岐は各関数の「fast path」でなく「slow path」側に置く必要がある
     （最初 fast path に書いて到達しないバグを踏んだ — 教訓として記録）。
  4. 副次修正（`vm_var_assign_set_local.rs`）: 上記 2 で単一 index の
     container-valued leaf（例 `@x := @array[2]`、要素が Array/Hash 自体）
     を bind すると `ContainerRef` セルが返るようになった結果、VarDecl bind
     の既存 Positional 型チェックが `raw_popped` を decont せずに
     `Value::ContainerRef` を弾いて `X::TypeCheck::Binding` を誤発生
     （既存の別バグを誘発）。`raw_popped.deref_container()` してから
     判定するよう修正——ついでに `@x := @array[2]; @x = 5,6;` が `@array`
     へ書き戻らない**既存の**バグも解消（`docs/container-identity.md` の
     Phase 2 系とは独立した bind-time 経路の穴だった）。
  新規テスト: `t/bound-array-slice-arity.t`。
- **2026-07-03 解決（test 54-56）**:
  - test 56（`@a[**]` HyperWhatever hammer index）: index に `Value::HyperWhatever`
    を受けたとき、ネスト配列を再帰的に降下して全 leaf を flat list として返す分岐を
    追加（`vm_var_index_ops.rs` の `hyperwhatever_hammer_collect`）。ローカル raku は
    未実装だが roast の期待値（`[^10].List`）は決定的なので実装可能。
  - test 54-55（重複インデックスを含むネスト lazy リストへの **bind**
    `(@a[lazy 1,2,(4,5),4,5] := ...)`）: 式コンテキストの indexed `:=` bind が
    `__mutsu_bind_index_value` マーカー無しで parse され、VM が `=` 代入扱い
    （境界打ち切り＋最終状態値）していた。真因は 2 つ:
    ① `try_parse_assign_expr`（式/括弧経路）の index-target `:=` がマーカーを付けて
    いなかった → statement 版と同じマーカー付与に修正（`try_assign.rs`）。
    ② VM 側に bind 専用の分配関数が無く assign と同じ経路 → `bind_slice_key_tree`
    （write-time 値・境界打ち切りなし）＋ `slice_key_tree_max_index_all` を新設し、
    LazyList arm で `bind_mode` 分岐（`vm_var_assign_index_named.rs`）。
    回帰テスト＝`t/hyperwhatever-and-lazy-slice-bind.t`。
- **test 35（nested slice adverbs）解決済み（2026-07-04）**: ネストインデックスへの
  slice adverb（`:p`/`:k`/`:v`/`:kv`/`:exists`/`:delete` の組み合わせ、plan 62）が
  nested index list に再帰するよう、4 経路（`builtin_subscript_adverb` /
  `ExistsIndexAdv` / `DeleteIndexNamed` / `DELETE-KEY`+adverb compiler routing）を
  additive に再帰化。Range/Seq/List target の coerce（`("a".."z")[3,5]:k`）も追加。
  container-identity §3 とは独立した read/format 再帰＋leaf delete で完結した。

---

## 5. dispatch cluster

`wrap.t` / `dispatching.t` / `qualified.t` / `assign.t` / `hyper.t` は完了済み（詳細は
`news/2026-06.md` / `news/2026-07.md`）。演算子 sugar が dispatch へ落ちるまでの surface は
すべて whitelist 済みで、このクラスタに残件はない。

### 5.1 `S03-metaops/hyper.t` — **DONE・whitelist 済み（2026-07-03）**

420/420 PASS。最後まで残っていた 2 件を解消:
- **test 407**（`».+&`/`».*&` の builtin-MRO all-candidates dispatch）: `.+`/`.*` が
  List/Any/Cool のような多段クラス階層をまたぐ全候補を呼べるよう VM 側で対応
  （`vm_call_helpers.rs` / `vm_hyper_method_ops.rs` / `methods_call_helpers.rs`）。
- **test 408**（atomicint のクロージャ内リセット書き戻し）: `subtest {...}` のような
  ネストしたクロージャフレーム内での `$r = 0`（`atomicint` へのプレーン代入）が共有
  atomic セルをリセットせず、次の `⚛++` が古い値を読んでいた。真因は
  `reset_atomic_var_key` が現フレームの `env` でしか name→value_key マッピングを探さず、
  かつ SetGlobal 経路（自由変数書き込み）にリセット呼び出しが無かったこと。
  SetGlobal store に SetLocal と同じ atomic リセットを追加し、`reset_atomic_var_key` に
  shared_vars フォールバックを追加（`vm_exec_dispatch.rs` / `runtime_shared_vars.rs`）。
  回帰テスト＝`t/atomicint-closure-reset-writeback.t`。

---

## 6. 並行・非同期（S17）

大半は完了した（semaphore.t、then.t、scheduler/basic.t、supply/migrate.t、supply/stable.t、
S17-promise/start.t、S07-hyperrace/basics.t、S17-lowlevel/cas-int.t、S17-lowlevel/lock.t —
詳細は `news/2026-06.md` / `news/2026-07.md`）。残るのは次の 1 ファイル。

### 6.2 `S17-supply/syntax.t`

- **現状**: 88/90（test 75, 53 が失敗。**test 90 は 2026-07-08 に修正済み**）。
  根本原因分析は `TODO_roast/S17.md` に記録済み:
  - **test 90 (FIXED)**: live `Supply.grep`/`.map` 転送タップ + react loop の
    supplier pre-drain（emit-before-done 順序保証）+ グローバル emit 順マージ
    （`emit_seqs` で sibling `whenever $s.grep(...)` を emit 順に interleave）で
    決定的に通過。pin=`t/supply-live-grep-map-react-order.t`。
  - test 75: nested `whenever`（on-demand supply が `whenever Supply.interval {}` の
    内側にある場合）の `closing => { $closed++ }` が cross-thread plain-scalar
    writeback（worker→main lexical）で伝播しない別軸（§4.1/§4.2 thread-capture gap、
    lock.t 10-24 と同じ）。cross-thread-lexical campaign 待ち。
  - test 53: `supply {}` の whenever 相互排他順序 race（full-file parallel load 下のみ
    flaky、単体では 6/6 pass）。
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

1. **第一級コンテナ campaign**（§3）— `docs/container-identity.md` に沿って
   attributes.t / multislice hash 側の slot identity を前に進める。
   これは腰を据えた基盤工事で、個々のテストを直接潰すより効果が大きい。
   **進捗（2026-07-05）**: whole-container 代入の container-identity（`@a = ...` の
   in-place 化＋`=` コピーの fresh-`Gc` detach）は完了し `splice.t` は whitelist 済み
   （§3.1 参照）。残るは closure-captured shared-cell を list へ by-value 捕捉したときの
   スナップショット追従（return-writeback 系）と、attribute slot の cell 化。
2. **`S17-supply/syntax.t`**（§6.2）— test 75/90 は個別に深掘りが必要（hard case）。

whitelist を目標にしない §7 の項目は、mutsu 側の一般改善のついでに触れるのはよいが、
そのファイル単体を通すことを目的にしない。
