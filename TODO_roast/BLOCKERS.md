# roast ブロッカー再評価メモ

この文書は、roast の失敗を「テストファイル単位」ではなく
**根本原因単位**で追うための索引。
個々の失敗を片端から潰すためではなく、
**今どこを直せば何がまとめて動くか**を判断するために使う。

**最終更新 2026-06-28**

## 2026-06-28 near-pass sweep の結論

`tmp/near-pass.tsv`（残 1 失敗のファイル群）を起点に「浅いターゲット」を探したが、
1 失敗で残っている §2 の Medium 級は**いずれも深いアーキテクチャ機能がブロッカー**だと
確認した。今セッションで isolation 再現まで取って verdict を確定したもの:

- `S04-declarations/my-6e.t` → dual-store の **block-local scope 漏れ**
  （`if (1) { my $b = 1 }` 後に `$b` が見える；EVAL 固有ではない）。
- `S02-types/generics.t` → 6.e **coercion type 項 + `Array[T]` サブクラス化**
  （ローカル raku v2022.12=6.d でも実行不可で全体検証もできない）。
- `S04-declarations/constant.t` → 演算子 `constant &op := &other` の
  **共有 alias**（現状コピー実装）。68/72、唯一の file-stopper。
- `S02-literals/allomorphic.t`（§2.6） → **lexical class identity**
  （同名 `my class` が global registry で後勝ち）。
- `S06-signature/slurpy-params.t`（§2.1） → **Seq single-pass consumption**
  （Seq を materialize し `X::Seq::Consumed` を投げない）。

→ **curated §2 Medium 群について「1 失敗の浅いターゲットは枯渇」が結論。** 残りは
container identity / lazy-iterator / lexical-scope (dual-store) / 並行実行 という
§3〜§4 の基盤工事に帰着する。次の前進はこれら基盤のいずれかに腰を据えて着手すること。
各ファイルの最小再現と詳細は §2 / `TODO_roast/S02.md` / `S04.md` に記録済み。

未 probe バケット（保留）: `tmp/near-pass.tsv` の `integration/advent*` 系（1 失敗が
~10 ファイル）は「実プログラムが特定 1 機能で落ちる」別カテゴリで、今回は未調査。
個別に浅い可能性はあるが、まとまった共通根があるとは限らないため、基盤工事より
優先度は低い。掘るなら 1 ファイルずつ failure を特定してから判断する。

## この版での再評価結果

今回の見直しで、旧版には次の問題があることを確認した。

- すでに whitelist 済みのファイルが未解決項目として残っていた。
  例: `S05-capture/array-alias.t`、`S05-match/capturing-contexts.t`、
  `S03-binding/attributes.t`、`S03-binding/nested.t`、
  `S12-methods/accessors.t`、`S32-io/io-path.t`、`S32-list/skip.t`、
  `S09-hashes/objecthash.t`。
- 「main track」はもう `Interpreter-removal` ではない。
  そこはほぼ終わっており、今の主戦場は
  **第一級コンテナ / 真の lazy 配列 / dispatch / 並行実行基盤**。
- 一見すると孤立した不具合に見えるものでも、
  実際には main track にぶら下がっている項目がある。
  例: `S26-documentation/12-non-breaking-space.t`、
  `S12-introspection/walk.t`。

このため、旧版の「ファイル別の細かい失敗数」よりも、
**依存関係と投資対効果**を優先して整理し直した。

## この文書の読み方

- **§1 優先度A**:
  main track と衝突しにくく、直せば横展開しやすいもの。
- **§2 優先度B**:
  局所修正で進むが、複数の小さな論点を含むもの。
- **§3 main track 待ち**:
  第一級コンテナ、lazy 配列、dispatch 基盤などの着地待ち。
- **§4 並行・非同期**:
  S17 を中心とした別軸の重い課題。
- **§5 whitelist を目標にしない項目**:
  rakudo 側も失敗する、または roast 側の問題が強いもの。

個別の詳細ログは `TODO_roast/S*.md` を参照。
このファイルは、そちらの要約と進め方の整理に徹する。

## 現在の前提

- whitelist は **1285**。
- 安い 1 ファイル勝ちはかなり減っている。
- 残件の大半は、少数の大きな根本原因に集約される。
- 今後は「テスト名の羅列」ではなく、**どのレイヤをどう崩すか**まで書く。

## この版で固定する分析粒度

各 blocker は、今後この粒度で扱う。

- **根本原因**: 何の抽象が欠けているか
- **変更レイヤ**: parser / compiler / VM / runtime / value 表現 / scheduler のどこを触るか
- **最初の 1 PR**: まず何を切り出して landed させるか
- **完了条件**: どこまで行けば、その類の roast がまとめて動くか

特に混ぜないこと:

- **lazy 配列**と**Seq single-pass consumption**は別問題
- **第一級コンテナ**と**attribute/container capture**は同じレバー
- **S17** は 1 本ではなく、shared scalar coherence / scheduler / supply timer / async IO の 4 軸
- すでに whitelist 済みの項目は、進行中 blocker の主列には置かない

今いちばん効く大分類は次の 4 つ。

1. **第一級コンテナ / container identity**
   `:=`、`is rw`、`.VAR`、属性・要素の共有、Capture の書き戻し、
   typed-hash default、wrapper/closure の container capture など。
2. **真の lazy 配列 / 無限列**
   `@a[0..*]`、lazy slurpy、same-type lazy iterable の
   `X::Cannot::Lazy`、配列操作の reify-on-demand。
3. **dispatch / MOP / call surface**
   wrapper/closure lexical visibility、call cache、lexical `&` shadowing、
   qualified dispatch、compound assign / method-call desugar など。
4. **並行実行基盤**
   Semaphore、lock contention、nonblocking await、Supply combinator、
   detached `start/react` の駆動保証など。

---

## 1. 優先度A — main track と衝突しにくく、効果が大きい

### 1.1 `S32-exceptions/misc.t`

- **優先度**: 最優先
- **難度**: Hard
- **現状**: 171/181 pass（181 run / plan 182、bad plan）。
- **理由**:
  1 ファイルだけを見ると重いが、ここで実装する型付き例外は他ファイルへ再利用できる。
  例外の数は多いが、main track のコア構造を大きく壊さず進められる。
- **評価**:
  内容は「丸ごと未着手」ではなく **終盤の one-off 群** に入っている。
  旧版が挙げていた `X::Comp::BeginTime`（tests 46/47/61）と
  `gather { return }` / 各種 `X::ControlFlow::Return`（tests 70-73）は
  **既に pass している**（旧ガイダンスは stale だったので削除）。
- **完了済みスライス**:
  slice 1 (compile-sorrow accumulator, tests 20+26) / slice 2 (source-format
  preserving sink warning — `Expr::LiteralSrc(Value, source)` で数値リテラルの
  元書式を保持。tests 139-142 と `X::Syntax::Confused` の pre/post=test 17。
  PR #3931 で確定、`(1) { }` 等の括弧付きリテラルを誤ラップしない回帰修正含む) /
  slice 3 (PR #3932: 未終端スマート単一引用符 `‘…`/`‚…` → `X::Comp::FailGoal`、
  および FailGoal の `.line` を停止位置=EOF 行に設定。test 178) /
  slice 4 (test 155: parameter sub-signature 内の非 slurpy `@`/`%` param が
  Positional/Associative を要求。`bind_sub_signature_from_value` に sigil 型検査を
  追加し `X::TypeCheck::Binding::Parameter`（`.got` に実値を載せる新
  `typecheck_binding_parameter_value`）を投げる。併せて EVAL の undeclared 前検査が
  sub-signature 内名 (`$first`/`@rest`) を宣言済みとして扱うよう
  `add_sub_signature_locals` を追加) /
  slice 5 (test 166: role 型オブジェクト（built-in role `Callable`/`Iterable`/…
  または user role）への `but` を `X::Method::NotFound`（`method => 'mixin'`,
  typename `Perl6::Metamodel::ParametricRoleGroupHOW`、非空 message）にする。
  新 `Interpreter::but_on_type_object_error`（`is_role_type_name` で built-in
  role を認識）。`does` は全型オブジェクトで `X::Does::TypeObject` 維持、`but` on
  class 型オブジェクト（`Int but role`）も `X::Does::TypeObject` 維持 = 匿名
  サブタイプ生成機能は別途未実装) /
  slice 6 (test 157: genuine bare block `{ ... }` を Raku callframe として
  扱い、その中で捕捉された backtrace の `.list` に匿名フレーム（`.subname` 空 /
  `.is-routine` False）を含める。`BlockScope`/`TryCatch` opcode に
  `is_bare_block` フラグを追加（非 tail block 経路、自己クリーンアップ）、
  inline 経路（tail block は inline されて opcode 境界を持たない）には
  `PushBlockFrame`/`PopBlockFrame` marker opcode を導入。if/while 等の合成
  ブロックは frame にしない（`synthetic_block_body` で gate）。leak は
  sub/block/try 境界が routine_stack を entry 深度に truncate して回収。
  concise text（`.Str`/gist）には匿名 block 行を出さず raku の `.nice` に
  一致させた）。
- **残件インベントリ（2026-06-29 実測、TAP 番号）**:
  - test 3: `X::Undeclared::Symbols` の `post_types`（grammar 前方宣言）
  - test 16, 179: `X::Comp::Group`（panic/sorrows/worries の構造化グルーピング）
  - test 21, 22: `X::Comp::FailGoal` — カスタム `postcircumfix/circumfix:«⟨ ⟩»`
    演算子（custom bracket slang。非常に困難）
  - test 27: `&[doesntexist]` は未知 infix なので compile error にすべき（X::Comp）。
    parse 時の infix レジストリ照合が必要で、語幹 infix の誤検出で valid な
    `&[wordop]` を壊すリスクがある（要注意）。
  - test 84: `sub foo(C of Int)`（body 無し）が parse error。raku は signature
    parse 時に `X::NotParametric`（body ありの 80-83 は pass 済み）。
  - test 121: sink warns on colonpair `:foo(42)` — AST が `Binary FatArrow` で
    `foo => 42` と区別不能、~29 の pair 検出箇所に影響するため別スライス。
  - test 161: backtrace 4 frames — DONE（`X::AdHoc.new.throw` の setting
    `throw` frame を `.backtrace.list` 先頭に積む。structured-only でレンダリング
    テキストからは隠す。`build_backtrace_value_with_leading`）。
  - test 177: 二重引用符内 `{` を必ず closure 補間として扱う（unbalanced `{` を
    リテラル扱いせず `X::Comp::FailGoal`）。`{` 補間厳格化は broad-blast-radius
    で高リスク（`"literal { brace"` を現状 mutsu は許容、raku は Two terms）。
- **Next slice 候補**（いずれも単発・要設計）:
  test 84（body 無し parametric-param sub）が比較的 contained だが parse 時の
  型パラメータ可能性判定が要る（parser に型情報なし）。test 16/179 の
  `X::Comp::Group` 構造化は再利用性が高いが重い。
- **Canary tests**:
  `roast/S32-exceptions/misc.t`
- **Primary files**:
  `src/runtime/regex_parse_core.rs`, `src/compiler/helpers_phasers.rs`,
  `src/runtime/builtins_control_flow.rs`, `src/ast.rs`

### 1.2 IO の残三兄弟

- **対象**:
  `S32-io/io-handle.t`、`S32-io/io-cathandle.t`、`S32-io/lock.t`
- **難度**: Hard
- **根本原因**:
  「少し足りない IO」ではなく、mutable handle object / handle 合成 /
  OS lock primitive が未完成。
- **変更レイヤ**:
  `runtime/native_methods/*`、`methods_object_native_ctors_io.rs`、IO 系 Instance の attr 更新。
- **最初の 1 PR**:
  `io-handle.t` を先に詰める。user-subclassable `IO::Handle` と polymorphic
  `READ` / `WRITE` / `EOF` が無い限り、その上の `CatHandle` も lock 周辺も閉じない。
- **完了条件**:
  `IO::Handle` の object identity と multi-dispatch される read/write API が
  一貫し、その上に `CatHandle` と file lock を積めること。
- **Next slice**:
  user-subclassable `IO::Handle` 上で `.READ` / `.WRITE` / `.EOF` を method dispatch に通す
- **Canary tests**:
  `roast/S32-io/io-handle.t`, `roast/S32-io/io-cathandle.t`, `roast/S32-io/lock.t`
- **Primary files**:
  `src/runtime/methods_object_native_ctors_io.rs`,
  `src/runtime/native_methods/mod.rs`,
  `src/runtime/native_proc_async.rs`

内訳:

- `io-handle.t`:
  user-subclassable `IO::Handle` と `READ` / `WRITE` / `EOF` の多相 dispatch が必要。
- `io-cathandle.t`:
  `IO::CatHandle` 自体が未実装。
  さらに一部は rakudo 側の挙動確認を要する。
- `lock.t`: **DONE（whitelist 済）**。
  `fcntl` record lock（`F_SETLK`/`F_SETLKW` × `F_RDLCK`/`F_WRLCK`）で
  `IO::Handle.lock`/`.unlock` を実装。モード不一致（read handle に排他 /
  write handle に共有）は kernel が `EBADF` を返すので `X::IO::Lock` Failure に整形。
  cross-process 観測は `is_run` が lock コードを in-process（同 PID で fcntl が
  競合しない）で走らせていたのを subprocess 強制に変更して解決。
  併せて 3 つの一般バグも修正:
  (1) `.method: 'a' and .method: 'b'` の colon-arg が loose `and`/`or` を取り込む
  parser バグ（listop 引数として parse し直し）、
  (2) fire-and-forget `start` スレッド出力が program exit で drain されず消える、
  (3) `IO::CatHandle.lock`/`.unlock` を active handle へ委譲（無ければ Nil）。

実装順は次で見るのがよい。

1. `IO::Handle` object が「native fd を持つ可変レシーバ」であることを固定する
2. `READ` / `WRITE` / `EOF` を method dispatch に通す
3. `IO::CatHandle` を「複数 handle の逐次読み source」として実装する
4. file lock は最後に OS primitive を足す

### 1.3 Unicode / RakuAST / Collation

**前提（2026-06-27 調査）**: mutsu の Unicode 対応は実質完成。
S15 全 81 ファイル、および tractable な S32-str Unicode 機能
（fc/flip/comb/tc/tclc/uc/capitalize/samecase/samemark/uniparse/utf8-c8 …）は
すべて whitelist 済み。未 whitelist の Unicode テストは下記 2 件のみで、
どちらも大規模サブシステム待ち。新規に着手して通せる tractable な Unicode
テストは残っていない。

- `S32-str/CollationTest_NON_IGNORABLE-3.t`
  - **難度**: Hard
  - **根本原因**:
    1369 中 2 失敗（test 1161, 1171）。ICU4X が BMP センチネル noncharacter を
    特別扱いする（U+FFFE = primary-ignorable、U+FFFF = max-sentinel）一方、
    UCA-17/MoarVM は全 noncharacter に codepoint 由来 implicit weight
    （AAAA=0xFBC0+(cp>>15)）を付与するため。mutsu は符号位置を正しく保持して
    おり、差異は ICU4X collator 内部のみ。
  - **依存方針の調査結果**:
    - `icu_collator`（icu4x）は **完全 pure Rust・C 依存なし**。ICU4C（C ライブラリ）
      とは別物で、データも Rust crate に同梱（`cargo tree` で確認）。
      よって「外部 C 依存を減らす」観点では現状すでに問題なし。
    - 代替（すべて pure Rust）: **feruca**（from-scratch UCA、Unicode16/CLDR46、
      icu4x より小依存・2-4倍速、spec 準拠なので noncharacter バグは直る見込み／
      要実測）、collate（成熟度低）、rust_icu（ICU4C bindings = **C 依存が増える**、却下）。
    - 自前実装の規模 ≒ feruca を作り直す（DUCET データ + contraction/expansion +
      implicit weight 派生 + sort key 構築、数千行 + 生成データ）。pure Rust の
      feruca が既にある以上、自前実装の妥当性は低い。
    - feruca 採用時の **要検証リスク**: 現状の `coll` は icu4x で 3 強度比較して
      `$*COLLATION` の 4 レベル個別 reverse/disable を合成している。feruca の公開
      API は `collate()` 一本で per-strength / sort key を露出しないため、
      `coll` + 非デフォルト `$*COLLATION` の再現が難しい可能性がある。
  - **評価**:
    実バグだが正しい修正には DUCET ベースの collation 入れ替え（feruca 移行 or
    自前実装）が必要で、2 ケースのために大仕事。noncharacter は交換用途では
    非妥当文字で実害も薄い。低 ROI のため defer。着手するなら feruca を throwaway
    で実測（noncharacter 修正可否 + coll/$*COLLATION 維持可否）してから判断する。
- `S32-str/format.t`
  - **難度**: Hard
  - **現状**: 49 中 26 到達・全 pass。`Format`/`.fmt` は完全実装済みで、
    test 30-49 相当（List/Seq/Set/Bag/Mix/Map への `.fmt`）も単体では動く。
  - **評価**:
    test 27-29 が `Formatter::Syntax.parse`→Match、`Formatter.CODE`→Callable、
    `Formatter.AST`→`RakuAST::Node` を要求し、ここで runtime error 中断するため
    以降が到達不能。本質は **RakuAST サブシステム不在**。参照 raku 本体すら
    `Format`（6.e）未対応。RakuAST::Node を偽装するのは stub（禁止）なので、
    RakuAST 本実装なしには whitelist 不可。

---

## 2. 優先度B — 局所修正で前進するが、複数論点を含む

### 2.1 `S06-signature/slurpy-params.t`

- **難度**: Medium ではなく、実質 Medium-Hard
- **再評価ポイント**:
  旧版は「残り 6 件」と書けていたが、根は 2 本ある。

残件は分離して考えるべき:

1. **真の lazy slurpy**
   `oneargraw(1..*)` 系。
   これは lazy 配列キャンペーンに接続する。
2. **Seq の single-pass consumption**
   `+foo` / `+@foo` の identity と、2 回目反復時の `X::Seq::Consumed`。
   これは lazy 配列とは別軸で、Seq の消費モデルの問題。

つまり、このファイルは「slurpy だけの局所修正」では終わらない。
一部は §3 に送るべき。

- **2026-06-28 確認**: 残り失敗は 70/71/74/75/76 で、すべて #2 (Seq single-pass) 軸。
  最小再現:
  ```raku
  sub f(+a) { a };
  say f((1,2,3).grep({$_})).WHAT.^name;   # mutsu: Array / raku: Seq
  my \seq = f((1,2,3).grep({$_}));
  my @r; push @r, $_ for seq;             # 1回目: [1 2 3]（両者OK）
  push @r, $_ for seq;                    # 2回目: mutsu 黙って再反復 / raku throws X::Seq::Consumed
  ```
  mutsu は slurpy 受け取り時に Seq を Array へ materialize するため、(a) `.WHAT === Seq`
  が崩れ、(b) single-pass 消費状態を持たないので `X::Seq::Consumed` を投げられない。
  対して `+@a`（配列 slurpy）は List 化が正しい。
  **verdict: deep（lazy iterator / Seq 消費モデル）。** Seq に消費フラグを持たせ、
  sigilless slurpy が Seq identity を保持する必要があり、ADR-0001 の lazy/iterator track
  と地続き。局所修正では落ちない → §3.2 寄り。

追加で固定しておく:

- **lazy slurpy 側の変更レイヤ**:
  `docs/lazy-arrays.md` の L2/L3、`binding_signature.rs`、`@`-assign preserve、`LazyList` の array-context
- **Seq single-pass 側の変更レイヤ**:
  `Value::Seq` / `LazyList` の consumed state、`+foo` と `+@foo` の文脈差、再反復時の失敗
- **最初の 1 PR**:
  `X::Seq::Consumed` を発火させる最小 pin test を作り、2 回目反復だけをまず正しく落とす
- **完了条件**:
  `+foo` は Seq のまま、`+@foo` は List 化、再反復は `X::Seq::Consumed`
- **Next slice**:
  `pd.onearg && pd.sigilless` で single `Value::Seq` を preserve する最小分岐
- **Canary tests**:
  `roast/S06-signature/slurpy-params.t`
- **Primary files**:
  `src/runtime/types/binding_signature.rs`,
  `src/parser/stmt/sub_param/param_inner.rs`,
  `src/runtime/methods_call_dispatch.rs`

### 2.2 `S04-statements/for.t`

- **難度**: Medium
- **主な論点**:
  bad loop-var binding と topic aliasing edge case
- **評価**:
  旧版どおり局所修正で進む。
  parser / control-flow 周辺に閉じていて扱いやすい。

### 2.3 `S04-declarations/my-6e.t`

- **難度**: Medium
- **論点**:
  EVAL が外側 lexical scope を見られるか
- **評価**:
  broad な main track ではなく、EVAL と lexical visibility の局所課題として扱える。

### 2.4 `S03-operators/inplace.t`

- **難度**: Medium
- **論点**:
  readonly constant や class instantiation に対する `.=`
- **評価**:
  小さく見えて operator surface と readonly 判定が絡むが、
  main track 待ちではない。

### 2.5 `S02-types/generics.t`

- **難度**: Medium
- **論点**:
  nominalizable generic type
- **評価**:
  parameterized role/MOP ほど深くはないが、型オブジェクト側の整理が必要。

### 2.6 `S02-literals/allomorphic.t`

- **難度**: Medium-Hard
- **論点**:
  gather ブロックごとに同名 lexical class が別 identity を持つべきなのに、
  いまは global map で潰れている。
- **評価**:
  「parser の小ネタ」ではなく **型宣言 identity** の問題。
  ただし container identity ほど基盤工事ではない。

- **2026-06-28 確認**: 残り失敗は 16/32/48（IntStr/RatStr/NumStr の `.ACCEPTS`）。
  同名 `my class IntFoo` が @true gather（`method Numeric { 3 }`）と @false gather
  （`method Numeric { 42 }`）で二重宣言され、@true の IntFoo.new は Numeric=3 を
  保つべきだが mutsu は最後の定義（42）で潰す。最小再現:
  ```raku
  my @a = gather { my class Foo { method val { 1  } }; take Foo.new };
  my @b = gather { my class Foo { method val { 99 } }; take Foo.new };
  say @a[0].val, @b[0].val;   # mutsu: 99 99 / raku: 1 99
  ```
  gather 内で作った instance は値としてキャプチャされるが、メソッド dispatch 時に
  クラス名 "Foo" を global registry で引くため、後勝ちの定義に解決される。
  **verdict: deep（lexical class identity / dual-store debt）。** `my class` を
  宣言ごとに別 identity として lexical scope に閉じ込め、instance がその identity を
  保持する必要がある。同じ家系の課題: [[my-6e EVAL block-local scope leak]] と
  同じ「block-local 宣言が global store に漏れる」debt。

### 2.7 multislice lvalue

- **対象**:
  `S32-array/multislice-6e.t`、`S32-hash/multislice-6e.t`
- **難度**: Medium
- **論点**:
  `@a[0;0;0] = 999`、`%h<a;b;c> = 999` の lvalue 書き戻し経路
- **評価**:
  旧版の「main track blocked」よりは軽い。
  subscript-as-lvalue 経路の未実装として独立管理でよい。

- **変更レイヤ**:
  parser ではなく `vm_var_index_ops` / `vm_var_assign_ops` / lvalue binding surface
- **最初の 1 PR**:
  単一 subscript と multislice が同じ「書き戻し可能 target 集合」抽象を返すようにする
- **完了条件**:
  `assignable-ok(\target, ...)` の内部からの read/write が、呼び出し中に元配列/元ハッシュへ見える

### 2.8 `S04-declarations/constant.t`

- **再評価**:
  旧版では lazy 側に寄せていたが、`TODO_roast/S04` と `PLAN.md` の文脈では
  主要 lazy ケースはかなり前進済み。
  いま残る大きい論点は `G::c` qualified name など dispatch 側。
- **評価**:
  現時点では lazy 主因としては扱わない。

---

## 3. main track 待ち — いま個別に触っても前進しにくい

この節は「直せない」ではなく、
**先に container identity / lazy array / dispatch 基盤が進んだ方が速い**
という意味。

### 3.1 第一級コンテナ / container identity

この群は今も最大のレバー。

- `S02-types/capture.t`
- `S02-names-vars/variables-and-packages.t`
- `S04-blocks-and-statements/temp.t`
- `S14-traits/attributes.t`
- `S12-subset/subtypes.t` の一部
- `S02-types/whatever.t` のうち container preservation 系
- `S32-hash/adverbs.t` の typed-hash default survival
- `S32-array/splice.t`
- `S02-names/is_default.t`

**再評価で移動した項目**:

- `S26-documentation/12-non-breaking-space.t`
  - 旧版は Pod / BEGIN hoisting 側に寄せていたが、
    `TODO_roast/S26.md` の分析では
    **BEGIN 中の配列 lexical 変更が外へ永続化しない**ことが主因。
  - つまり isolated Pod 問題ではなく、container identity 側の課題。
- `S12-introspection/walk.t`
  - 旧版は self-contained MOP としていたが、
    実際の残りは
    **public rw accessor mutation の永続化**と
    **lazy one-at-a-time invocation**。
  - したがって、これも main track 待ちに移すのが正しい。

ここは「container identity が必要」というだけでは粗い。実際には
次の 4 サブキャンペーンに分けて追うべき。

1. **配列/ハッシュ要素 cell 化**
   - 依存文書: `docs/container-identity.md`
   - 変更レイヤ: `value/mod.rs`、`vm_var_assign_ops.rs`、`vm_var_index_ops.rs`
   - 最初の 1 PR: 既存 helper への write-through 集約を増やす
   - 完了条件: element bind / take-rw / deep nested write が post-call writeback なしで成立
2. **属性 accessor を value copy ではなく slot 経由にする**
   - 変更レイヤ: attribute read/write path、instance attr storage、methods instance ops
   - 対象: `S12-attributes/clone.t`, `S14-traits/attributes.t`, `S12-introspection/walk.t`
3. **typed-hash default / Capture 書き戻し / wrapper capture**
   - 変更レイヤ: hash missing-key default、Capture bind/writeback、wrap closure env
   - 対象: `S32-hash/adverbs.t`, `S02-types/capture.t`, `S02-types/whatever.t`
4. **BEGIN/EVAL/lexical 配列変更の永続化**
   - 変更レイヤ: env/local coherence、block escape、BEGIN 実行時 lexical carrier
   - 対象: `S26-documentation/12-non-breaking-space.t`, `temp.t`, package/var tests

言い換えると、「第一級コンテナ」は単独 blocker 名ではなく、
**slot identity をどこまで VM の正規表現にするか**という設計課題である。

コード choke point は既にかなり見えている。

- 要素 write 集約:
  - `src/value/mod.rs::assign_element_slot`
  - `src/value/mod.rs::hash_insert_through`
- 要素 read decont:
  - `resolve_array_entry`
  - `resolve_hash_entry`
- whole-container / env-local coherence:
  - `docs/env-locals-coherence.md`
  - `src/vm/vm_env_helpers.rs`
  - `SetLocal` / `flush_local_to_env` / bound-container metadata

したがって、container 系で先にやるべきなのは新しい workaround を足すことではなく、
**read/write surface をこの choke point 群へさらに寄せること**である。

- **Next slice**:
  配列/ハッシュ write を helper 経由へさらに寄せ、post-call writeback 依存を 1 段減らす
- **Canary tests**:
  `roast/S32-array/splice.t`, `roast/S32-hash/adverbs.t`,
  `roast/S02-types/capture.t`, `roast/S02-names-vars/variables-and-packages.t`
- **Primary files**:
  `src/value/mod.rs`, `src/vm/vm_var_assign_ops.rs`,
  `src/vm/vm_var_index_ops.rs`, `src/vm/vm_env_helpers.rs`

### 3.2 真の lazy 配列 / 無限列

- `S09-subscript/slice.t`
- `S03-operators/eqv.t` の lazy case
- `S06-signature/slurpy-params.t` の lazy slurpy half

ここは `docs/lazy-arrays.md` の L2b-L4 と同じ話。
closure-based infinite sequence はかなり進んだが、
**Array が lazy を保ったまま mutation / slice / slurpy に耐えられない**。

ただし、ここにも 2 本ある。

1. **Array-held lazy source**
   - 依存文書: `docs/lazy-arrays.md`
   - 変更レイヤ: `LazyList` 表現、`@`-assign preserve、mutation 時 reify、slice/index
   - 対象: `S09-subscript/slice.t`, lazy slurpy half
2. **Seq single-pass consumption**
   - 変更レイヤ: `Value::Seq` / `LazyList` の consumed state と iterator API
   - 対象: `S06-signature/slurpy-params.t`, lazy `eqv`, 一部 list ops

`docs/lazy-arrays.md` は前者の設計文書としては十分細かい。
後者には [docs/seq-single-pass-consumption.md](../docs/seq-single-pass-consumption.md) を新設した。

コード choke point:

- Array-held lazy source:
  - `src/runtime/resolution_lazy.rs`
  - `src/runtime/methods_call_dispatch.rs`
  - `src/runtime/methods_type_coerce.rs`
  - `src/runtime/builtins_operators_infix.rs`
- Seq consumed guard:
  - `src/runtime/methods_call_dispatch.rs`
  - `src/runtime/methods_dispatch_match2.rs`
  - `src/runtime/test_functions/comparison.rs`

つまり、lazy array campaign は既存文書どおり `LazyList` の reify/mutation を進めればよいが、
`X::Seq::Consumed` は **別に guard 点の棚卸し**が必要。

- **Next slice**:
  `(1...*)` / closure-seq の `@`-assign preserve と mutation-side reify の差分整理
- **Canary tests**:
  `roast/S09-subscript/slice.t`, `roast/S03-operators/eqv.t`,
  `roast/S06-signature/slurpy-params.t`
- **Primary files**:
  `src/runtime/resolution_lazy.rs`,
  `src/runtime/methods_call_dispatch.rs`,
  `src/runtime/methods_type_coerce.rs`

### 3.3 object-hash / junction key — **DONE (classify.t whitelisted, 2026-06-29)**

- ~~`S32-list/classify.t`~~ **PASS / whitelisted**

`classify`/`categorize` keyed by a non-`Str` classifier result (e.g. a Junction
from `*.contains: any ...`) now builds an OBJECT hash. Implemented in:
- `builtins_collection_classify.rs`: `classify_finish_hash` marks the result an
  object hash (`key_type=Some("Any")` + `original_keys`) when any first-level key
  is non-Str.
- `builtins/methods_0arg/collection.rs` `.keys`: honor `has_typed_keys()` (was
  Set/Bag/Mix `__setty_origin`-only) so object-hash keys come back as real objects.
- `vm/vm_var_index_ops.rs`: BOTH junction-subscript autothreading sites now skip
  threading on an object hash (`has_typed_keys()`, peeking through Scalar/
  ContainerRef) → falls to the `.WHICH`/encoding key lookup; and the object-hash
  key type-check is skipped for `Any`/`Mu` key types (which accept a Junction key
  that `type_matches_value` would otherwise autothread+reject).
- `parser/primary/ident/predicates.rs`: added `classify`/`categorize` to
  `is_listop` so the bare-sub list-op form `classify *.meth, @list` parses (was
  read as `classify * (.meth ...)` infix-multiply → "Cannot convert string to
  number 'classify'").

Pin: `t/classify-object-hash.t`. NOTE: a *declared* object hash `my %h{Any}` with
an Array/object KEY in a STORE (`%h{[1,2]} = ...`) is still treated as a slice on
the write path — broader object-hash subscript STORE remains future work (the
classify result is read-only, so this PR's read path suffices).

### 3.4 dispatch-sensitive cluster

main track ほどではないが、局所修正を積み上げるより
dispatch 基盤を意識してまとめて触った方がよい群。

- `S06-advanced/wrap.t`
- `S06-advanced/dispatching.t`
- `S03-operators/assign.t`
- `S03-metaops/hyper.t`

これらは parser、multi dispatch、call cache、wrapper lexical visibility が絡む。

ここも少なくとも 3 本に分ける。

1. **wrapper / closure lexical visibility**
   - 対象: `wrap.t`, 一部 `assign.t`
   - 変更レイヤ: wrap 時の env capture、callsite lexical overlay
2. **multi dispatch / cache / candidate ordering**
   - 対象: `dispatching.t`, `hyper.t` の callable side
   - 変更レイヤ: candidate selection、cache key、wrapper 後 invalidation
3. **operator sugar が dispatch へ落ちるまでの surface**
   - 対象: `assign.t`
   - 変更レイヤ: parser lowering、compound assign / method-call desugar

`S12-methods/qualified.t` はここから外す。parameterized role の残件は完了済み。

コード choke point:

- wrapper / lexical overlay:
  - `src/runtime/resolution_call_sub.rs`
- candidate selection / cache:
  - `src/runtime/methods_dispatch_match2.rs`
- assign / operator desugar:
  - parser lowering と `vm_var_assign_ops`

この cluster は「まず parser か runtime か」が案件ごとに違うので、
ファイル名を見ずに generic に片付けようとすると迷子になる。

- **Next slice**:
  `wrap.t` を入口に closure lexical overlay の authoritative path を 1 本決める
- **Canary tests**:
  `roast/S06-advanced/wrap.t`, `roast/S06-advanced/dispatching.t`,
  `roast/S03-operators/assign.t`
- **Primary files**:
  `src/runtime/resolution_call_sub.rs`,
  `src/runtime/methods_dispatch_match2.rs`,
  `src/vm/vm_var_assign_ops.rs`

---

## 4. 並行・非同期 — S17 を中心とした別軸の重課題

この軸は roast 全体の中でも性質が違う。
container identity や lazy 配列とは別に、
**スケジューラと同期原語の正しさ**が問われる。

主な未解決:

- `S17-lowlevel/semaphore.t`
- `S17-lowlevel/lock.t`
- `S17-promise/start.t`
- `S17-promise/then.t`
- `S17-scheduler/basic.t`
- `S17-supply/migrate.t`
- `S17-supply/stable.t`
- `S17-supply/syntax.t`
- `S07-hyperrace/basics.t`

補足:

- `S17-promise/nonblocking-await.t` はもう whitelisted。
- `S17-supply/categorize.t` も whitelisted。
- したがって旧版の「S17 全体が広く停滞」は、今は言い過ぎ。
  残っているのは **本当に重い並行 primitives** に寄っている。

この節は今後、次の 4 軸で見る。

### 4.1 shared scalar coherence

- **対象**:
  `S17-lowlevel/semaphore.t`、一部 `Promise.start` / `lock-async`
- **根本原因**:
  thread clone された lexical `$x` が bare read では stale のまま。
  `Lock.protect` には専用 resync があるが、Semaphore には無い。
- **変更レイヤ**:
  `shared_vars` / `shared_vars_dirty`、thread env clone、critical section entry/exit
- **最初の 1 PR**:
  `Semaphore.acquire/release` を `call_protect_block` と同型の resync/writeback chokepoint に寄せる
- **完了条件**:
  bare semaphore critical section でも captured scalar の read-modify-write が安定する

- **コード choke point**:
  - `src/runtime/native_methods/concurrency.rs`
  - `src/runtime/resolution_eval.rs::call_protect_block`
  - `src/runtime/runtime_shared_vars.rs::sync_shared_vars_to_env`
  - `src/runtime/native_methods/state_lock.rs`
- **Next slice**:
  semaphore 保護ブロックを `call_protect_block` 型の reconcile/writeback helper に寄せる
- **Canary tests**:
  `roast/S17-lowlevel/semaphore.t`, `roast/S17-promise/lock-async.t`
- **Primary files**:
  `src/runtime/native_methods/concurrency.rs`,
  `src/runtime/runtime_shared_vars.rs`,
  `src/runtime/native_methods/state_lock.rs`

### 4.2 scheduler / detached task lifecycle

- **対象**:
  `S17-promise/start.t`、`then.t`、`S17-scheduler/basic.t`
- **根本原因**:
  detached `start` / cue / uncaught handler / TAP sync が別々に欠けている
- **変更レイヤ**:
  scheduler queue、thread spawn/join、uncaught exception reporting、test runtime bridge
- **最初の 1 PR**:
  `$*SCHEDULER.uncaught_handler` と start-thread exception propagation を先に固定する
- **完了条件**:
  task 起動、失敗、完了、reporting の lifecycle が 1 本の実装で閉じる

- **コード choke point**:
  - `src/runtime/native_methods/scheduler.rs`
  - `src/runtime/native_methods/state_scheduler.rs`
  - `src/runtime/methods_promise.rs`
  - `src/runtime/builtins_system_async.rs`
- **Next slice**:
  detached `start` failure を `$*SCHEDULER.uncaught_handler` へ通す経路を 1 本化する
- **Canary tests**:
  `roast/S17-promise/start.t`, `roast/S17-promise/then.t`,
  `roast/S17-scheduler/basic.t`
- **Primary files**:
  `src/runtime/native_methods/scheduler.rs`,
  `src/runtime/native_methods/state_scheduler.rs`,
  `src/runtime/methods_promise.rs`

### 4.3 Supply combinator / timer-driven flush

- **対象**:
  `S17-supply/stable.t`、`migrate.t`、`syntax.t`、flaky `batch.t`
- **根本原因**:
  emit-driven 実装が多く、time-based operator に background clock / cue が無い
- **変更レイヤ**:
  supply registry、scheduler cue、native supply mut methods
- **最初の 1 PR**:
  `batch(:seconds)` 系の flush を emit 依存から scheduler cue 依存へ分離する
- **完了条件**:
  Supply operator が「次の emit が来たら動く」のではなく、時間と demand の両方で駆動される

- **コード choke point**:
  - `src/runtime/native_supply_mut_methods.rs`
  - supply registry 群
  - scheduler cue の接続点
- **Next slice**:
  `batch(:seconds)` を emit-driven flush から scheduler cue-driven flush へ分離する
- **Canary tests**:
  `roast/S17-supply/stable.t`, `roast/S17-supply/migrate.t`,
  `roast/S17-supply/syntax.t`
- **Primary files**:
  `src/runtime/native_supply_mut_methods.rs`,
  `src/runtime/native_methods/scheduler.rs`,
  supply state registry modules

### 4.4 async IO / Proc::Async / socket supplies

- **対象**:
  `procasync/*`、`IO-Socket-Async*`、supply syntax の一部
- **根本原因**:
  encoding、stdin/stdout/stderr supply bridge、socket/listener 駆動が未成熟
- **変更レイヤ**:
  `methods_object_native_ctors_io.rs`、`native_methods/socket_*`、encoding layer
- **最初の 1 PR**:
  `Proc::Async` の encoding 無し happy-path を 1 本通す
- **完了条件**:
  child process / socket / Supply bridge が同じ scheduler と event-source model に載る

- **コード choke point**:
  - `src/runtime/methods_object_native_ctors_io.rs`
  - `src/runtime/native_proc_async.rs`
  - `src/runtime/native_methods/socket_async_conn.rs`
  - `src/runtime/native_methods/encoding.rs`

`Proc::Async` と socket supply は最後に見た目だけ合わせるのではなく、
**event source -> channel/supply bridge -> scheduler** の 1 本化を先に考えるべき。

- **Next slice**:
  encoding 無しの `Proc::Async` happy-path を supply bridge と done/quit delivery まで揃える
- **Canary tests**:
  `roast/S17-procasync/basic.t`, `roast/S17-promise/basic.t`,
  `roast/S32-io/IO-Socket-Async.t`
- **Primary files**:
  `src/runtime/methods_object_native_ctors_io.rs`,
  `src/runtime/native_proc_async.rs`,
  `src/runtime/native_methods/socket_async_conn.rs`

---

## 5. whitelist を目標にしない項目

### 5.1 rakudo 側も失敗する、または roast 側の問題が強いもの

ここは mutsu 側で一般改善が入るのはよいが、
**そのファイルを whitelist すること自体を目標にしない**。

- `S05-nonstrings/basic.t`
- `S05-metasyntax/angle-brackets.t`
- `S05-mass/rx.t`
- `S06-advanced/caller.t`
- `S06-advanced/return_function.t`
- `S10-packages/require-and-use--dead-file.t`
- `S12-traits/parameterized.t`
- `S12-meta/exporthow.t`
- `S12-class/open_closed.t`
- `S32-str/sprintf.t`

### 5.2 低 ROI で後回しにすべきもの

- `S32-str/CollationTest_NON_IGNORABLE-3.t`
  - 2 ケースのために実装が重すぎる。
- `S32-str/format.t`
  - `RakuAST` が無い限り最後まで通しにくい。

---

## 6. 今のおすすめ着手順

「次に何をやるか」を 1 本だけ選ぶなら、順番はこう見るのが妥当。

1. `S32-exceptions/misc.t`
   reusable な型付き例外を増やす。
2. IO 三兄弟
   `io-handle` / `io-cathandle` / `lock` を別軸で片づける。
3. 第一級コンテナ campaign
   `docs/container-identity.md` に沿って slot identity を前に進める。
4. Seq single-pass 設計メモの新設
   lazy 配列と混ぜずに `X::Seq::Consumed` 軸を切り出す。
5. S17 を 4 軸へ分解した上で、まず semaphore / scheduler を触る。

その一方で、次は**個別テストを直接殴るより、先に基盤 campaign を系統だって進めた方が速い**ので、原則として後回し。

ここでいう「後回し」は、

- 難しいから放置する
- 価値が低いから無視する

という意味ではない。

意味は逆で、これらは **main track の canary** であり、
個別に special case を足すより

- 第一級コンテナ
- lazy / Seq 消費モデル
- dispatch 基盤

のどれかを先に前進させた方が、結果としてまとめて解ける、という判断である。

- `S26-documentation/12-non-breaking-space.t`
- `S12-introspection/walk.t`
- `S32-array/splice.t`
- `S32-hash/adverbs.t` の typed-hash default 群
- `S09-subscript/slice.t`

補足:

- `S32-array/splice.t` は「splice 固有のロジック不足」が主因ではなく、
  self-splice / push-replace-self が **true first-class element containers** を要求する
  [container identity canary](/home/tokuhirom/work/mutsu-codex/TODO_roast/S32.md:83)。
- `S32-hash/adverbs.t` の typed-hash default 群も、missing-key default の one-off というより
  container/default survival の canary。
- `S09-subscript/slice.t` も parser 小修正だけでは閉じず、lazy source 保持と nested slice semantics の両方に触れる。

## 6.1 `S32-exceptions/misc.t` をやるなら、何を順にやるか

このファイルは「例外を追加する」だけでは終わらない。残件は 4 束に分けて着手する。

1. **compile-sorrow grouping**
   - 変更レイヤ: parser/compiler の error accumulator
   - 対象: `X::Comp::Group`
   - 最初の 1 PR: 複数 compile failure を即 throw せず収集する土台
2. **source-format preserving warning**
   - 変更レイヤ: literal AST/source span
   - 対象: sink warning 群
   - 最初の 1 PR: `Expr::Literal` に source text か span を持たせ、warning builder へ通す
3. **BEGIN textual-time execution**
   - 変更レイヤ: compiler/unit init ordering
   - 対象: `X::Comp::BeginTime`
   - 最初の 1 PR: BEGIN を unit 末尾の後処理ではなく、その位置で実行する最小モデル
4. **純 parser / control-flow one-off**
   - 対象: `my $x :a`, `my Int (Str $x)`, `gather{return}`, `&[unknownop]`
   - これは最後にまとめて取る

この順にしないと、細かな `X::` を足しても not-run が減らない。

実コードで最初に見る場所も固定しておく。

- **compile-sorrow grouping**
  - `src/runtime/regex_parse_core.rs`
  - compile-time error を `X::Comp::Group` へ畳む既存断片があるので、まずここを一般化する
- **source-format warning**
  - `src/ast.rs` の `Expr::Literal`
  - warning emission 側は `src/runtime/builtins_control_flow.rs`
  - ここに source text / span を通さない限り `:foo(42)` が `foo => 42` に崩れる
- **BEGIN textual-time execution**
  - `src/compiler/helpers_phasers.rs`
  - `src/runtime/phasers.rs`
  - `src/runtime/registration_sub.rs`
  - すでに `X::Comp::BeginTime` の箱はあるので、問題は「いつ走らせるか」
- **param type / suppressed name 系**
  - `src/runtime/registration_sub.rs::validate_param_type_constraints`
  - `src/runtime/runtime_encoding.rs` の `suppressed_names`
- **`gather { return }`**
  - parser ではなく runtime の control-flow signal 境界を探す仕事

## 6.2 次に新設すべき設計メモ

まだ `BLOCKERS.md` だけでは足りず、専用メモを起こした方がよいものが 2 つある。

1. **Seq single-pass consumption**
   - なぜ必要か:
     lazy array と混ざるが、本質は「消費済み状態をどこに保持し、どの API がそれを見るか」
   - 最初に棚卸しするコード:
     `methods_call_dispatch.rs`, `methods_dispatch_match2.rs`,
     `comparison.rs`, `builtins_operators_infix.rs`
2. **S17 scheduler / supply drive model**
   - なぜ必要か:
     semaphore, `start`, `batch(:seconds)`, Proc::Async, socket supply が
     全部「誰がいつ event loop を回すか」で再合流する
   - 最初に棚卸しするコード:
     `native_methods/scheduler.rs`, `methods_promise.rs`,
     `native_supply_mut_methods.rs`, `native_proc_async.rs`

この 2 本は次の文書として起こした:

- [docs/seq-single-pass-consumption.md](../docs/seq-single-pass-consumption.md)
- [docs/s17-scheduler-supply-drive-model.md](../docs/s17-scheduler-supply-drive-model.md)

## 6.3 完了済みで main list から外す項目

この版では、次を blocker の主列から外す。

- `S32-array/adverbs.t`
- `S12-methods/qualified.t`
- `S09-typed-arrays/native-shape1-int.t`
- `S09-typed-arrays/native-shape1-num.t`
- `S09-typed-arrays/native-shape1-str.t`

いずれも既に whitelist 済みで、今の優先順位づけを歪めるため。

---

## 7. 旧版から落とした主な項目

今回の再評価で、このファイルからは次の「完了済みなのに残っていた項目」を整理対象から外した。

- `S05-capture/array-alias.t`
- `S05-match/capturing-contexts.t`
- `S03-binding/attributes.t`
- `S03-binding/nested.t`
- `S04-statements/gather.t`
- `S06-advanced/lexical-subs.t`
- `S12-methods/accessors.t`
- `S32-io/io-path.t`
- `S32-list/skip.t`
- `S09-hashes/objecthash.t`
- `S17-promise/nonblocking-await.t`
- `S17-supply/categorize.t`

これらはすでに whitelist 済みであり、
今後この文書では「最近完了した項目」としても原則扱わない。
