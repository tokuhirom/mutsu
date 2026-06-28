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

→ **「1 失敗の浅いターゲットは枯渇」が今の正しい結論。** 残りは container identity /
lazy-iterator / lexical-scope (dual-store) / 並行実行 という §3〜§4 の基盤工事に
帰着する。次の前進はこれら基盤のいずれかに腰を据えて着手すること。各ファイルの
最小再現と詳細は §2 / `TODO_roast/S02.md` / `S04.md` に記録済み。

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

今いちばん効く大分類は次の 4 つ。

1. **第一級コンテナ / container identity**
   `:=`、`is rw`、`.VAR`、属性・要素の共有、Capture の書き戻し、
   typed-hash default、wrapper/closure の container capture など。
2. **真の lazy 配列 / 無限列**
   `@a[0..*]`、lazy slurpy、same-type lazy iterable の
   `X::Cannot::Lazy`、配列操作の reify-on-demand。
3. **dispatch / MOP / parameterized role**
   qualified method、role parameter forwarding、call cache、
   lexical `&` shadowing、qualified dispatch の曖昧性処理など。
4. **並行実行基盤**
   Semaphore、lock contention、nonblocking await、Supply combinator、
   detached `start/react` の駆動保証など。

---

## 1. 優先度A — main track と衝突しにくく、効果が大きい

### 1.1 `S32-exceptions/misc.t`

- **優先度**: 最優先
- **難度**: Hard
- **現状**: `TODO_roast/S32.md` ベースで 129/180 pass まで前進済み
- **理由**:
  1 ファイルだけを見ると重いが、ここで実装する型付き例外は他ファイルへ再利用できる。
  例外の数は多いが、main track のコア構造を大きく壊さず進められる。
- **主な残件**:
  `X::Comp::Group`、`X::Comp::BeginTime`、`X::NotParametric` の順序、
  `X::InvalidType`、source-format を保った sink warning、
  `gather { return }` の制御フロー、細かな parser/signature edge case。
- **評価**:
  「単一ファイルとして最優先」という旧評価は維持。
  ただし数字は旧版よりかなり改善しており、内容も
  「丸ごと未着手」ではなく **終盤の one-off 群** に入っている。

### 1.2 `S32-array/adverbs.t`

- **優先度**: 高
- **難度**: Hard
- **理由**:
  parser と subscript/adverb surface の隙間に残っている大きな塊。
  `X::Adverb` 自体はかなり整っており、残りは「未整理の構文面」が中心。
- **評価**:
  main track 依存というより parser/compiler 側の広い未整備。
  直し始める価値は高い。

### 1.3 `S12-methods/qualified.t`

- **優先度**: 高
- **難度**: Hard
- **現状**: 6/7 subtests pass
- **残件の本体**:
  parameterized role まわり。
  具体的には role-to-role type parameter forwarding、
  `$?ROLE` / `$?CLASS` の parameterized 名、
  qualified call の複数 concretization 返却。
- **評価**:
  旧版の「MOP で Hard」は妥当。
  ただし残っているのは 1 サブテストだけなので、
  今は **広い MOP 一般** ではなく **parameterized role の集中課題**として扱うべき。

### 1.4 shaped native typed arrays

- **対象**:
  `S09-typed-arrays/native-shape1-int.t`、
  `native-shape1-num.t`、`native-shape1-str.t`
- **難度**: Hard
- **理由**:
  固定次元の意味論が未完成。
  `.map`、slice、`.raku` の `:shape(...)`、空代入による初期値復元、
  `.grep` / `.values` / `.pairs` などが不完全。
- **評価**:
  孤立サブシステムとして扱ってよい。
  main track と衝突しにくいので、独立ブランチ向き。

### 1.5 IO の残三兄弟

- **対象**:
  `S32-io/io-handle.t`、`S32-io/io-cathandle.t`、`S32-io/lock.t`
- **難度**: Hard
- **評価**:
  旧版の大筋は妥当。
  ただし `io-path.t` はもう対象から外す。

内訳:

- `io-handle.t`:
  user-subclassable `IO::Handle` と `READ` / `WRITE` / `EOF` の多相 dispatch が必要。
- `io-cathandle.t`:
  `IO::CatHandle` 自体が未実装。
  さらに一部は rakudo 側の挙動確認を要する。
- `lock.t`:
  `flock` ではなく `fcntl` 系の record lock が必要。
  cross-process の再現も絡み、テストが timing-sensitive。

### 1.6 Unicode / RakuAST / Collation

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

### 3.2 真の lazy 配列 / 無限列

- `S09-subscript/slice.t`
- `S03-operators/eqv.t` の lazy case
- `S06-signature/slurpy-params.t` の lazy slurpy half

ここは `docs/lazy-arrays.md` の L2b-L4 と同じ話。
closure-based infinite sequence はかなり進んだが、
**Array が lazy を保ったまま mutation / slice / slurpy に耐えられない**。

### 3.3 object-hash / junction key

- `S32-list/classify.t`

`objecthash.t` 自体はもう whitelisted なので、
旧版の「%{Mu} 全体が未着手」という書き方は粗すぎた。
今残っているのは、**junction を key にした classify 結果の取り出し**という、
より狭い意味論の問題。

### 3.4 dispatch-sensitive cluster

main track ほどではないが、局所修正を積み上げるより
dispatch 基盤を意識してまとめて触った方がよい群。

- `S06-advanced/wrap.t`
- `S06-advanced/dispatching.t`
- `S03-operators/assign.t`
- `S03-metaops/hyper.t`

これらは parser、multi dispatch、call cache、wrapper lexical visibility が絡む。

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
2. `S32-array/adverbs.t`
   parser / subscript surface の整理を進める。
3. `S12-methods/qualified.t`
   parameterized role の残件を詰める。
4. shaped native typed arrays 3 本
   main track 非衝突のまとまった塊として進める。
5. IO 三兄弟
   `io-handle` / `io-cathandle` / `lock` を別軸で片づける。

その一方で、次は**局所修正を積み上げても効率が悪い**ので、原則として後回し。

- `S26-documentation/12-non-breaking-space.t`
- `S12-introspection/walk.t`
- `S32-array/splice.t`
- `S32-hash/adverbs.t` の typed-hash default 群
- `S09-subscript/slice.t`

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
