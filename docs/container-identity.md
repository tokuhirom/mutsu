# 第一級コンテナ (container identity) 実装台帳

PLAN.md「🟣 第2優先: 第一級コンテナへの移行」の実装ログ。
`docs/vm-dual-store.md` / `docs/vm-decoupling.md` と同じく、**現状の正確な地図 →
段階スライス → 各 PR の進捗**を残す台帳。コードに触る前に「どこを直すか」を
確定させ、過去の ContainerRef プロトタイプが踏んだ "deref everywhere" 回帰を避ける。

最終ゴール: **最速かつ最もメンテしやすい** Raku インタープリタ。コンテナ統一は
正しさ修正であると同時に、散在する workaround を削除するメンテ性の勝ち筋。

---

## 1. 現状: コンテナ/itemization が **3 つに断片化**している

mutsu は「Raku のコンテナ identity」を持たず、代わりに重複する 3 表現が併存する。
これが断片化＝負債の実体で、統一の対象。

| # | 表現 | 定義 | 役割 | 主な構築箇所 |
|---|------|------|------|------------|
| 1 | `Value::Scalar(Box<Value>)` | `src/value/mod.rs:1075` | itemization ラッパ (`$(...)`) | `vm.rs:1814`(WrapScalar), `vm.rs:1803`(Seq itemize), `runtime/methods.rs:215/2327` |
| 2 | `Value::ContainerRef(Arc<Mutex<Value>>)` | `src/value/mod.rs:1078` | `:=` の変数間共有セル | `into_container_ref()` (`value/mod.rs:2136`)。呼び出しは**わずか4箇所**: `vm.rs:1475`, `vm_register_ops.rs:271`, `vm_var_assign_ops.rs:4880` |
| 3 | `ArrayKind::ItemList` / `ItemArray` | `src/value/mod.rs:844-846` | Array 値に焼いた itemization フラグ | `ArrayKind::itemize()` (`value/mod.rs:867`), `OpCode::Itemize` (`vm.rs:1792`) |

decont（脱コンテナ）ヘルパも分散:
- `Value::deref_container()` — `value/mod.rs:2112`（ContainerRef を読む）
- `Value::decontainerize(&self) -> &Value` — `value/mod.rs:2367`（Scalar を剥がす）
- `ArrayKind::decontainerize()` — `value/mod.rs:878`（item フラグを落とす）

→ **「コンテナを剥がす」操作が 3 つあり、呼ぶ側が文脈ごとにどれを使うか判断している。**
これが "deref everywhere" の温床。

---

## 2. 標準ケーススタディ: `:=` 束縛リストが平坦化されない (reduce.t 62)

```raku
my $l := (1,2,3);   # raku: $l は List そのもの。.VAR は List
my @a = $l;         # raku: 3 要素（List が平坦化）
say @a.elems;       # raku: 3 / mutsu: 1   ← バグ
```

### なぜこうなるか（トレース）

1. `my $l := (1,2,3)` と `my $l = (1,2,3)` は **どちらも `Value::Array(items, List)`（非
   itemized）を格納**する。`:=` はコンパイラで `MarkBindContext` により区別されるが
   (`compiler/stmt.rs:768/795`)、スカラー格納パスは束縛/代入で同じ bare List を置く
   （`normalize_scalar_assignment_value` は itemize しない、`vm_var_assign_ops.rs:766`）。
   → **格納段階でコンテナ status が失われる**。`$l.VAR.^name` は両方 `Scalar`（raku は
   束縛なら `List`）。
2. `@a = $l` のコンパイル時、`compile_assignment_rhs_for_target`
   (`compiler/stmt.rs:233`) が **`@`ターゲット + `Expr::Var` なら無条件で
   `OpCode::Itemize` を emit**する (`stmt.rs:245`)。
3. `OpCode::Itemize` (`vm.rs:1792`) が `Array(.., List)` → `Array(.., ItemList)` に変換。
4. 配列代入の flatten は ArrayKind を尊重する（`ItemList`→1要素、`List`→平坦化）。
   実際 `@a = (1,2,3)`（リテラル、Var を経由しない）は 3 要素、`@a = $(1,2,3)` は 1 要素で
   **既に正しく動く**。問題は (2) の **Var 経由のスカラー読みが無条件 itemize される**こと。

### なぜ局所修正できないか

`stmt.rs:245` の `Itemize` を「束縛変数なら skip」したくても、**ランタイム値は束縛/代入で
完全に同一**（どちらも bare `Array(.., List)`）。コンパイラも別宣言の `$l` の束縛性を知らない。
→ 区別するには **格納レベルでコンテナ status を持たせる**しかない＝ Phase 1。
束縛変数集合を別管理する等の局所ハックは、まさに戦略が警告する散在 workaround。

---

## 3. 設計: decont を単一チョークポイントに集約する

Rakudo/MoarVM 流。コンテナは**格納サイト**（変数スロット・配列/ハッシュ要素・属性）に
のみ存在し、**値読み出しは VM のオペランド取得という唯一の経路で必ず decont**する。
算術・比較・ディスパッチ等の値 op は「decont 済み」をスタックから pop するので**無変更**。
コンテナが見えるのは明示的に lvalue/コンテナを要求する数少ない経路だけ:

- `:=` bind（コンテナ差し替え）
- `is rw` / `is raw` パラメータ（caller コンテナをエイリアス）
- `.VAR` / `=:=`（コンテナ identity の reflection）
- take-rw（要素コンテナの identity 保持）
- itemization 判定（list 文脈で 1 要素か平坦化か）

この設計なら消費面が「数百の値 op」→「上記一握り」に**反転**する。

### mutsu での着地

1. **canonical `decont()` の制定**: 現状 3 つの decont ヘルパを 1 つに統合
   （`Scalar`/`ContainerRef`/item-flag をすべて剥がす単一関数）。これを VM の値読み出し
   不変条件にする。
2. **値スタック不変条件**: 「スタックに積む値は常に decont 済み」。lvalue/コンテナが要る
   経路だけ専用 opcode（`GetLocalContainer`/`IndexContainer` 等）で生のセルを取得。

---

## 4. 段階スライス（big-bang 回帰を避ける順序）

### Phase 0 — decont チョークポイント整備（挙動不変）
- [ ] 3 つの decont ヘルパを単一 `decont()` に統合（`value/mod.rs`）。既存呼び出しを置換、
      挙動は完全不変＝ roast 完全一致で検証。
- [ ] 値読み出し経路を棚卸しし、「decont 済みを積む」不変条件を確立。lvalue 専用 opcode を追加
      （まだコンテナは殆ど無いので挙動不変）。
- 検証: `make test` + 全 roast 完全一致（差分ゼロが Phase 0 の合格条件）。

### Phase 1 — スカラーの第一級コンテナ化
- [ ] `$` 変数が `Scalar` セルを保持。`=` はセルへ格納、`:=` は束縛差し替え（bare 値）、
      itemization はセル wrap。`§2` の格納段階コンテナ status を実装。
- [ ] `stmt.rs:245` の無条件 `Itemize` を、**コンテナ保持変数のときだけ itemize** に変更
      （束縛変数の bare List は平坦化）。
- 解ける: reduce.t 62, `=:=`/`.VAR`（スカラー）, 兄弟クロージャ共有（レバー C 完了）,
  S02 variables-and-packages 変数捕捉, S03-binding/scalars。

### Phase 2 — 配列/ハッシュ要素のコンテナ化
- [ ] 要素を COW な `Arc<Vec<Scalar>>` 等のセルに。最もホットな表現なので Phase 1 の後。
- 解ける: take-rw（gather.t 38）, `@a[0] :=`, 深い `>>++`/`deepmap(++*)`（hyper.t 330-333）,
  object-hash, S12-methods/accessors, S12-attributes/instance, S03-binding/nested。

### Phase 3 — 属性コンテナ + 属性束縛
- [ ] `$!x :=` / per-attribute container template。
- 解ける: S03-binding/attributes, S14-traits/attributes 5-8。

---

## 5. 統一で削除できる workaround（メンテ性の勝ち筋）

コンテナ統一が進むと以下の散在ハックが**不要化**する（= 削除対象）:
- dual-store env↔locals の双方向 sync（レバー B と連動）
- Arc-pointer-keyed 型メタ副テーブル（PLAN Q2 項目。flaky の根。コンテナに型メタを載せれば消滅）
- ad-hoc itemization フラグ（`ArrayKind::ItemList/ItemArray` と `Value::Scalar` の二重化）
- grep-rw-view binding（`Arc`-pointer keyed、`=` 代入を跨いで残存するバグ持ち）
- name-based writeback reconcile（map/grep の rw エイリアス）

## 6. 速度の担保

- **エスケープ解析**でコンテナを省略（捕捉も `.VAR` もエイリアスもされないローカルは bare 値）。
- 配列は **COW** で読みはクローン無し。
- decont は単一分岐で予測が効く。
- 中期の NaN-boxing で payload 8 byte 化すればセルも安価。

---

## 7. 値読み出し opcode の棚卸し（Phase 0.5 地ならし / Phase 1 の設計図）

「スタックに積む値は常に decont 済み」不変条件を Phase 1 で確立するための前段棚卸し。各値読み出し
opcode が **今 push しうるコンテナ形** と **Phase 1 の目標** を対比する。`GetLocalRaw` が「生セルを
push する lvalue 読み」の既存前例（アンカー）で、Phase 1 の新 lvalue opcode
（`GetLocalContainer`/`IndexContainer`）はこれを一般化する。

| opcode | ハンドラ | 今 push しうる形 | 今の deref | Phase 1 目標 |
|--------|---------|-----------------|-----------|-------------|
| `GetLocalRaw` | `vm_var_assign_ops.rs:exec_get_local_raw_op` (~4101) | **生セル**（deref/descalarize 無し。DeferredHashAccess/HashSlotRef も解決しない） | なし（意図的・`=:=` 用） | **lvalue opcode の設計テンプレ** |
| `GetLocal` | `vm_var_assign_ops.rs:exec_get_local_op` (~4108) | ContainerRef は `into_deref` 済み（本 PR で集約）。Scalar/ItemArray は素通し | ContainerRef→inner（単一段） | 常に decont 済み |
| `GetGlobal` | `vm.rs:851-1078` (inline) | 同上 | ContainerRef→inner（単一段） | 常に decont 済み |
| `GetArrayVar` | `vm.rs:1079-1138` (inline) | Hash→Pairs 変換のみ。Scalar/ContainerRef/ItemArray 素通し | なし | 常に decont 済み（**実挙動変化・次段**） |
| `GetHashVar` | `vm.rs:1139-1201` (inline) | 素通し | なし | 常に decont 済み（次段） |
| `Index` | `vm_var_index_ops.rs:exec_index_op_with_positional` (~265-269) | Scalar は unwrap、ContainerRef/ItemArray は素通し | 部分（Scalar のみ） | 常に decont 済み（**実挙動変化・次段**） |

**ContainerRef 値読みの集約面は厳密に 2 サイト**（`GetLocal` / `GetGlobal`）。他の
`arc.lock().unwrap().clone()` は別軸（LazyList cache）か read-modify-**write**（increment サイト
`vm_misc_ops.rs:879/927/956/1004`, `vm_var_assign_ops.rs:1468/1514/1606/1652` — 同じ arc に書き戻すので
into_deref で arc を consume したら壊れる）であり**値読みではない**。よって本 PR の集約対象外。

**次段（Phase 1 と同梱）で入る挙動変化**: `GetArrayVar`/`Index`/`GetHashVar` を「常に decont 済みを
push」に変えると、配列要素が `Value::Scalar`/`ContainerRef`/`ItemArray` を持つケースで観測可能な挙動変化
が起きる（`$(...)`/`.VAR`/itemization）。「いつ decont し、いつコンテナを保持するか」は Phase 1 の
スカラー・コンテナ意味論が前提なので、新 lvalue opcode の本配線と合わせて Phase 1 と同一 PR で実施する。

## 進捗ログ

（PR ごとに追記）

- 2026-06-06: 台帳作成。現状の 3 表現と `:=` 平坦化バグの完全トレースを記録。コード未変更。
- 2026-06-08: **Phase 0 着手（decont ヘルパ統合・挙動不変）**。3 軸（Scalar / ContainerRef /
  ArrayKind itemization）は別の型・別の意味論なので**融合せず**、各軸に正規ヘルパを制定して散在する
  アドホック展開を集約する方針に確定（融合は `is rw` writeback の Pair 判定・`@a=$l` 平坦化を壊すため）。
  - PR1 (#2736): `Value::decontainerize` → `descalarize` 改名 + owned `into_descalarized` 追加 + decont
    family doc。重複していた再帰 `strip_scalar` を削除、`methods.rs` の再帰 `while let Scalar` ループ 4 件を
    集約。`OpCode::Decont`（単一段）と recursive helper の違いをコメントで明示。
  - PR2 (#2737): 単一 Scalar-arm 関数の再帰アドホック展開を `descalarize`/`into_descalarized` へ集約
    （ops の bag/mix multiply、utils の coerce_to_hash/numeric/set/quanthash、methods_narg の 1arg/2arg）。
    単一段サイト・`.VAR` ガード付き dispatch・mixed-axis（eqv/truthy/isa）・exhaustive match の
    value_type_name は意図的にインライン維持。
  - PR3: ContainerRef 読み軸。既存 `deref_container` は常に clone するため in-place 読み（lock して
    `&inner` を使う）サイトには不適（clone 追加）、hot read-opcode には致命的（全読みで clone）。
    そこで**非 clone の正規リーダ `Value::with_deref<R>(&self, f) -> R`** を新設し `deref_container` を
    その上に再定義、in-place 読み 6 箇所（utils value_type_name / introspect dispatch_what /
    display to_string_value / types eqv・truthy・isa_check・what_type_name の ContainerRef arm）を集約。
    hot read-opcode（GetLocal/GetGlobal の move-or-clone）と Group B（束縛/identity/write-through）は
    Phase 0.5 / Phase 1 へ後送り（現状維持）。
  - いずれも挙動不変を確認: `make test` PASS（5274）、binding/flatten/set/bag/mix/eqv の roast は
    plan/ran/failed カウントがベースライン一致（pre-existing 非 whitelist 失敗は不変）。
- 2026-06-08: **Phase 0.5 第1段（挙動不変な地ならし）**。ユーザー判断で段階的アプローチを採用 — 挙動変化を
  伴う stack 不変条件（`GetArrayVar`/`Index` の auto-decont）と新 lvalue opcode の本配線は Phase 1 と同梱の
  次段へ送り、本 PR は安全に切り出せる地ならしのみ:
  - **`Value::into_deref(self)` 追加**（ContainerRef 軸の owned move-through 版。`descalarize`↔
    `into_descalarized` と同型の `deref_container`↔`into_deref` ペアを完成）。PR3 が「hot read-opcode
    GetLocal/GetGlobal の move-or-clone … 現状維持」として明示的に後送りした分を集約。
  - `GetLocal`（`vm_var_assign_ops.rs` ~4206、`is_container_ref()` gate で早期 return 構造維持）と
    `GetGlobal`（`vm.rs` ~1071）の手書き inline `arc.lock().unwrap().clone()` を `into_deref` へ集約。
    非 ContainerRef は move 維持（hot path の挙動/perf 完全不変）。
  - §7 に値読み出し opcode の棚卸し表を追加（Phase 1 の設計図。`GetLocalRaw` を lvalue 読みのアンカー前例に）。
  - 挙動不変を確認: build/clippy/fmt PASS。binding/`.VAR`/`=:=`/sigilless の t/ + S03-binding roast 全 PASS。
    全 roast は CI に委譲。
- 2026-06-08: **Phase 1 第1スライス（非ループ兄弟クロージャのコンテナ共有 = レバー C 完遂）**。挙動変化スライス。
  - `box_captured_lexicals`（`vm_register_ops.rs`）のループ限定ゲート（246 の `loop_local_vars.is_empty()` 句、
    257 の per-symbol ループフィルタ）を撤去し、**非ループの捕捉＋変異 `$` スカラー**も共有 `ContainerRef` セルへ
    box。兄弟 Sub の env は `flattened` で Arc を共有するので同一セルを指し、read=GetGlobal `into_deref`／
    write=SetGlobal write-through で往復が成立。PR #2742 の `into_deref` チョークポイントが decont を担保。
  - **唯一の値漏れを修正**: メソッド fast-path の `try_eval_simple_protect_expr`（`vm_call_method_compiled.rs`）の
    `Expr::Var` arm が GetLocal/GetGlobal を経ない直接読みだったので `into_deref` を付与。
  - **型制約スカラーは box しないガードを追加**: ContainerRef write-through（`$x++`/`=`/`.=`）は代入チョーク
    ポイントの制約再チェックを回避するため、`var_type_constraint` を持つスカラーは box 対象外（subset/`where`/
    `my Int`）。`t/where-constraint-var.t` の `$x++` 回帰を防止。制約付き捕捉スカラーの共有は広範な write 監査へ後送り。
  - 解決: 兄弟クロージャ共有（`t/closure-container-capture.t` の todo 2 件を解消）。S02-names-vars/
    variables-and-packages.t が 16→13 fail に改善（副次）。
  - 検証: build/clippy/fmt PASS、`make test` PASS（456 unit + 5274 prove、回帰なし）、closure/binding/native-int
    （whitelist）roast 全 PASS。`gather.t` 38 は Phase 2 take-rw の既知未対応（非 whitelist・無関係）。全 roast は CI。
