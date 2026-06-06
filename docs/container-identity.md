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

## 進捗ログ

（PR ごとに追記）

- 2026-06-06: 台帳作成。現状の 3 表現と `:=` 平坦化バグの完全トレースを記録。コード未変更。
