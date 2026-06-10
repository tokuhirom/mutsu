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

> **正準の実装順序は PLAN.md §🟣「実装順序（アーキテクチャ第一）」を参照。** 北極星は最もクリーン × 最速で、
> 進捗メトリクスは **「削除した重複/特例メカニズムの数」**（roast 通過数ではない）。下の Phase 区分は地図として
> 残すが、着手は「キーストーン = escape 解析」→「スカラーのセル化」の統合解決で進める（個別 boxing パッチを
> 足さない）。

### 現状の重複（クロージャ捕捉だけで 4 メカニズム併存・step 1 で subsume → 削除）

クロージャが捕捉変数を見るための機構が場当たりに 4 つ併存し、隙間（単一脱出クロージャ等）を残している。
これらは escape 解析（PLAN.md 実装順序 step 1）が単一の needs-cell 判定に統合し、step 2 のセル化で**削除**する:

**⚠️ 2026-06-08 実証で確定: 下表の「step 2 で削除」想定は誤り。`multi_captured_mutated_locals` のみ #2758 で
subsume・削除済み。残り 3 機構は非冗長で削除不可（理由は各行）。** PLAN.md §🟣 STATUS と一致。

| 機構 | 役割 | 削除可否（実証） |
|------|------|----------------|
| `owned_captures`（`compute_owned_captures`） | ループ per-iteration **値凍結** | ❌ read-only 捕捉はセル化されず代替不可 |
| `closure_captured_state`（per-instance 値凍結 + writeback） | 返却クロージャの state 維持 | ❌ 事前コンパイル deserialize で load-bearing（セルはシリアライズ不可、`precompilation.t` 回帰）|
| `box_captured_lexicals` | 捕捉時セル生成（escape/loop） | ❌ 捕捉時が正位置。宣言時移設は `let`/`temp` 復元を壊す（`let.t` 回帰）|
| `multi_captured_mutated_locals`（#2751） | 兄弟クロージャ boxing（暫定 proxy） | ✅ #2758 escape 信号が subsume・削除済み |

env snapshot（`clone_env`/`flattened`）は捕捉の土台として残る。**セルは「ランタイム共有変異」は包含するが、
宣言時機構（let/temp）・シリアライズ・read-only 値凍結 は包含しない**ので、上 3 機構はセルを*補完*する別役割。

### step 2 の撤回ログ（2026-06-08）

- **「box を宣言時へ移設して削除」= 試行→revert**: 捕捉ローカルは block/loop/sub で sigil 無し名（`a`）＝
  `simple=false`→slow path、と解明し動かしたが、`let`/`temp` 復元（`S04-blocks-and-statements/let.t`）を決定的回帰。
  宣言時セル化が `let`-save より前に走り save がセル Arc を保存し復元不可。**box は捕捉時が正位置。**
- **「closure_captured_state を削除」= 試行→close（PR #2765）**: step 1 で代入/返却クロージャがセル化され冗長と
  仮説。無効化→make test PASS・166 state-sensitive ファイル release 0-fail。だが `S10-packages/precompilation.t`
  （GH2897）を回帰: `gen-counter` の返すクロージャは BEGIN/事前コンパイル時に生成・**ディスク serialize** され、
  ContainerRef セルはプロセス跨ぎで保存されないため deserialize 後は本副テーブルが per-call 状態を担う＝**load-bearing**。
- **教訓**: ①セル化の境界（runtime 共有変異のみ）。②これら release-only 回帰は `make test` を通過する — クロージャ
  捕捉に触る変更は **release で main vs branch 比較** ＋ `precompilation.t` を必ず含める。③container の機構削除はここで
  頭打ち。残る正しさは PLAN.md §🟣「機会的バックログ」で単発対応。

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

### Phase 3 — 第一級 instance セル（属性コンテナ + 属性束縛）

> ユーザー方針 (2026-06-10): dynamic var / instance mutation の closure 跨ぎ問題は、修正オプション
> (A) writeback 全チェーン走査 / (B) exit-writeback by-id / (C) 第一級 instance セル のうち **(C) で進める**。
> 構造的解決＝最大改修。下記が (C) の設計。

#### 解く問題（根本原因。詳細は docs/vm-state-ownership.md「調査記録 2026-06-10」）

instance を**値で保持**しているため、closure frame 内の変異メソッド（`$*ERR.print`、`$obj.set-x` 等）の
writeback `overwrite_instance_bindings_by_identity`（`methods_mut.rs:415`）が `self.env.values_mut()`＝
**scoped env の overlay tier のみ**を走査し、caller frame（parent tier＝immutable `Arc<Env>`）が保持する同一
instance の binding に届かない。同一 id でも変異が frame 復帰で消える。`sub cap(&code){ my $*ERR=F.new; &code() }`
で `code` 内の `note` が rebound `$*ERR` に書けない、が代表症状（pre-existing・`&code()` で再現）。map/for は
**同一フレーム実行**で overlay==caller のため偶然成立しているだけ。

#### 現状の表現

`Value::Instance { class_name: Symbol, attributes: Arc<InstanceAttrs>, id: u64 }`（`value/mod.rs:1000`）。
`InstanceAttrs` は `attributes: HashMap<String,Value>` を **Deref/DerefMut で包む**（`:455`）。
- 共有: `Arc<InstanceAttrs>` を clone で共有（CoW）。
- 変異: `Arc::make_mut`（CoW で別コピー化）→ `overwrite_instance_bindings_by_identity`（**id 一致の全 env binding を
  scan して置換**）で伝播。**この scan が overlay-only ＝バグの本体**。

#### 目標の表現（C）

`attributes` を **共有可変セル**にする。`Value::Instance` を clone してもセルを共有 → 変異が全 holder
（caller env / closure overlay / ネスト属性）で可視 → **`overwrite_*_bindings_by_identity` の by-id scan を全廃**。
- スレッド越境（`clone_for_thread`）があるので `Rc<RefCell>` 不可 → **`Arc<RwLock<HashMap<String,Value>>>`**
  （または `InstanceAttrs` 自体を内部可変セル化し `Value::Instance` は従来どおり `Arc<InstanceAttrs>` を持つ）。

#### blast radius（実測）

`Value::Instance` 参照 **875**、`Instance { attributes, .. }` パターン **127**、`make_mut`(methods_mut) **16**。
**Rust の借用モデル上、ロックした HashMap への `Deref` は不可**（guard の lifetime）。よって 127 の
`attributes.get(k)` 系 read サイトは API 経由（lock+clone）へ移す必要がある。一括変更は不可能。

#### 段階導入（big-bang を避ける。各段 CI を安全網に）

- [ ] **Stage 0 — compat read API（挙動不変・最大の機械的下準備）**: `InstanceAttrs` に内部表現非依存の
      メソッドを足す（`get(&self,k)->Option<Value>`〔cloned〕/`contains_key`/`iter` 代替の `snapshot()->HashMap`/
      `keys`/`len` 等）。**内部表現は HashMap のまま**で 127 read サイトを Deref から API へ移行＝byte-identical。
      これで Stage 1 の表現切替が局所化される。最も大きく退屈だが低リスク。
- [ ] **Stage 1 — 表現切替（共有セル化）**: `InstanceAttrs` 内部を `Arc<RwLock<HashMap>>` に。read は Stage 0 の
      API（read-lock + clone）、変異は **in-place（write-lock で書く）**。clone は **セルを共有**（Arc clone）。
      これで closure 跨ぎの変異が caller に可視になり、note/$*ERR バグが解消。
- [ ] **Stage 2 — 伝播ハック全廃**: `overwrite_instance_bindings_by_identity` と by-id scan、CoW `make_mut` 16 箇所を
      削除（変異が by-reference で可視になったので不要）。dual-store の instance writeback 経路も縮小。
- [ ] **Stage 3 — 仕上げ / perf**: escape 解析で「捕捉も `.clone` もされない instance はセル省略」（hot path 救済。
      Phase 1 のスカラーと同型）。型メタ副テーブルの Arc-ptr keying もセルに載せて廃止（Q2 flaky 吸収）。

#### 必須の正しさ監査（切替時に同梱）

- **`.clone`（Raku の独立コピー）は新セルを作る（deep copy）**: 共有セル化すると clone がセル共有になり
      `$b = $a.clone; $b.x = 1` が `$a` を汚す。`clone` メソッド（および `but`/mixin の独立コピー意味論）は
      **明示的にセルを複製**する必要がある。現状の「値 clone = 独立コピー」が無料で与えていた意味論を再現せよ。
- **dynamic var は lexical capture しない**: `call_compiled_closure` の captured-env merge（`vm_closure_dispatch.rs`
      ~230）が captured `$*ERR`（stale）を overlay に入れ live parent を shadow する。twigil `*` を capture から除外。
      （セル化単独でも読み取り側でこれが要る。）
- **制御フロー名除外**: lexical `&`-var dispatch を将来再開する際、`return`/`take`/`emit`/`callsame`/… は
      `call_function` の match が直接処理するので dispatch から除外（`&r=&return` 無限再帰防止。`is_builtin_function`
      に無い名が複数）。
- **eqv / WHICH / 比較**: instance 同一性比較（`===`/`.WHICH`）は id ベースのまま（セル共有でも id 不変）。
- **シリアライズ / precompilation**: セルは ContainerRef 同様プロセス越境で保存されない。`closure_captured_state`
      相当の per-instance 状態経路を壊さないか確認（[[project_lever_b_slice63_prep]] の precompilation 回帰の轍）。

#### これで解ける roast / 症状

- `$!x :=` / per-attribute container template（S03-binding/attributes, S14-traits/attributes 5-8）。
- closure-provided block 内の `note`/dynamic-handle 書き込み（テストハーネス `sub cap(&code){ my $*ERR=...; code() }`）。
- closure 内の任意の caller-held instance への変異メソッド。
- トラック A lexical `&`-var dispatch の前提解消（[[project_lexical_amp_var_blocked]]）。

#### 最初のスライス

**Stage 0（compat read API + 127 read サイト移行、挙動不変）**から。これが入れば Stage 1 の表現切替が
レビュー可能な単位になる。Stage 0 自体は機能変化ゼロなので CI でほぼ確実に緑、安全に大きく入れられる。

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
- 2026-06-08: **Phase 1 第1スライス（非ループ兄弟クロージャの broad boxing）を試行 → revert（重要な教訓）**。
  `box_captured_lexicals` のループ限定ゲートを撤去し非ループの捕捉＋変異 `$` スカラーも `ContainerRef` 化する
  approach を試したが、**性能・正しさの両面で under-scoped** と判明し revert。理由を記録（次スライスの設計制約）:
  - **(1) mutation-writeback ギャップ（部分的に対処可）**: `into_deref`（PR #2742）は**読み**だけを cell 対応に
    したが、**変異の書き戻し**は cell 非対応だった。具体的に `overwrite_instance_recursive`
    （`runtime/methods_mut.rs`、変異メソッドの結果を instance identity で全 env 束縛へ伝播するチョークポイント）が
    `Value::ContainerRef` の中を見ず、boxed スカラーが instance を保持して変異メソッドを受けると変異が消えた
    （`my $x; lives-ok { $x = Obj.new }; lives-ok { $x.mutate }; $x.read` という **roast 頻出のテストヘルパ
    パターン**。submethods.t / S24-testing / test-util 等が回帰）。`ContainerRef` arm 追加で修正可能と確認したが、
    これは「mutation 経路を全て cell 対応にする broad audit」の入口に過ぎない。
  - **(2) 深刻な性能回帰（approach を否定する決定打）**: broad boxing は **closure 生成毎に**捕捉＋変異の全
    自由変数を `Arc<Mutex>` 化し env へ insert する（= env COW のディープクローン誘発）。loop-only 制限は
    正しさだけでなく **boxing コストの上限**でもあった。撤去した結果 `roast/S32-num/int.t` が **1 秒 → 150s+
    に激遅化**（3 ファイルを main へ戻すと 1 秒に復帰、と差分で確定）。`make test` では露見せず CI の release
    全 roast で timeout として顕在化。
  - **教訓 / 次スライスの設計制約**: 兄弟クロージャ共有は **escape 解析（脱出する＝返却/外部格納される
    クロージャの捕捉だけ box。`lives-ok {…}` のような即時呼び出し引数クロージャは box しない）** で
    boxing 対象を絞り、かつ **mutation-writeback を cell 対応化**してから入れる必要がある。素朴な
    「ループ限定を外すだけ」は不可。これは §6「速度の担保＝エスケープ解析でコンテナを省略」の実証でもある。
  - revert で main は clean（`#2742` の `into_deref` 地ならしのみ残す）。`int.t` は 1 秒に復帰。
- 2026-06-08: **Phase 1 第1スライス再設計（escape-aware）= 成功**。前回の教訓どおり box 対象を精密 signal で
  絞り、非ループ兄弟クロージャのコンテナ共有を実装。
  - **コンパイラ signal `multi_captured_mutated_locals`**（`opcode.rs` `compute_free_vars`）: own-local を
    捕捉する distinct な子クロージャ数を数え、**≥2** かつ `captured_mutated` のものを集合化。
  - **`box_captured_lexicals`**（`vm_register_ops.rs`）: box 条件を (A) ループローカル（既存・不変）OR
    (B) `multi_captured_mutated_locals`（新・非ループ兄弟）に。型/`where` 制約ガードは (B) のみに適用
    （ループパスは byte 単位で維持）。`>=2` 制限が `lives-ok {…}`（1 クロージャ）を除外し、前回の perf 爆発
    （`Arc<Mutex>`+env COW を closure 生成毎）と correctness 回帰（テストヘルパパターン）を**構造的に回避**。
  - **mutation-writeback を cell 対応化**: `overwrite_instance_recursive`（env 経路）と
    `overwrite_instance_in_locals`（locals 経路）に `ContainerRef` arm を追加（boxed スカラーが instance を
    保持して変異メソッドを受けても共有セルを通して伝播）。`try_eval_simple_protect_expr` の Var 直接読みに
    `into_deref` を追加（fast-path の値漏れ防止）。
  - 検証: ターゲット `make(); $s(42); $g()` → `42`。**`int.t` は ~1s/165 維持（性能回帰なし）**、重量級 roast
    サンプル 11 ファイル 2314 テストが 3 秒で完走。`where-constraint-var.t`（block/whatever where がガードで保護）/
    `submethods.t` / `S24-testing` / closure 一式 全 PASS。`make test` PASS（456 unit + 5315 prove）。
    `gather.t` 38 は Phase 2 take-rw の既知未対応（非 whitelist・無関係）。
  - **範囲外（次スライス）**: 単一の脱出クロージャ（`my &f; { my $a=3; &f=sub{$a++} }` → 3,0）は 1 クロージャで
    multi に入らず未修正。`.VAR.^name` 反映 / `is rw` 3-way persistent も別スライス。
- 2026-06-08: **step 1 = escape 解析（コンパイラ・キーストーン）= 成功**。PLAN.md §🟣「実装順序」step 1。
  `multi_captured_mutated_locals`（「≥2 兄弟クロージャに捕捉」という proxy）を、本来の信号 **escape 解析**
  （捕捉する子クロージャの値がフレームを脱出するか）に置換。proxy ではなく本機構にしたことで、単一脱出
  クロージャを**構造的に**捕捉し、かつ即時呼び出し（`lives-ok {...}`/`map {...}`）は非 box のまま維持。
  - **コンパイラ signal**: `Compiler::escaping_position`（bool）+ `with_escape(escaping, f)` save/restore ヘルパ。
    escaping=true: 代入/`:=` RHS（`compile_assignment_rhs_for_target`）、`return`/`fail` operand、ブロック/
    ルーチン tail（`helpers_sub_body.rs` の last-`Stmt::Expr` 3 箇所 + `mod.rs` top-level tail）、配列/ハッシュ/
    capture リテラル要素。escaping=false（既定）: 呼び出し引数（`compile_call_arg`/`compile_method_arg` で
    リセット＝#2746 ガード。`my @r = map {...}` のように call が脱出位置でも引数クロージャは非 box）。
  - **データ**: `CompiledCode.closure_escapes: Vec<bool>`（`closure_compiled_codes` と添字一致）。
    `add_closure_code(code, escapes)` が `escaping_position` を記録。`compute_free_vars` は子を enumerate して
    `closure_escapes[i]` を見、捕捉×変異 own-local を**脱出する子に捕捉**されたら `needs_cell_locals`
    （旧 `multi_captured_mutated_locals` をリネーム）へ。`captured_mutated_locals`（path A ループ boxing 用）は不変。
  - **VM**: `box_captured_lexicals` の path B が `needs_cell_locals` を参照（型/`where` ガード・path A は byte-for-byte 不変）。
  - 検証: `$f = sub{$a++}` 単一脱出（`my $f`/`&f()`）→ 3/4（旧 3/0）。兄弟 `make(); $s(42); $g()` → 42（subsume）。
    factory `return sub{$n++}` → 0/1/0。即時呼び出し `for`/`map` → 非 box（3 / 12）。**perf カナリア
    int.t 0.21s 維持（#2746 回帰なし）**。`make test` PASS（458 unit + 5315 prove）、clippy clean、
    S03-binding/closure・S04-declarations/state・pointy(-rw) 等 whitelist PASS。
  - **bareword `f()` 抜けたブロック捕捉バグも修正**: `my &f; { my $a=3; &f=sub{$a++} }; f(); f()` が
    `3,0`（read-only `sub{$a}` は `3,Nil`）だった件。bareword `f()` は interpreter の `call_sub_value` 経由、
    `&f()`/`$f()` は VM の cell 対応 dispatch（`call_compiled_closure`）経由 — dual-store 分岐で VM 経路は元々正しい。
    原因: `call_sub_value` のキャプチャ env マージが `merge_all=true` で `ContainerRef`（共有セル）も
    `entry_or_insert`（caller 優先）していたため、宣言ブロックが漏らした stale 値や前回呼びの writeback が
    生きたセルを隠し、2回目の呼びで失われていた。修正: **マージで `ContainerRef` だけ `insert_sym`（上書き）して
    生きたセルを必ず採用**（VM `call_compiled_closure` と同じ「セルが真実の源」原則。`box_captured_lexicals` が
    捕捉×変異スカラーを box するので read-only/mutating とも $a はセル）。**dynamic 変数や非セルの値には触れない**ので
    `note`/`$*ERR` rebind（`note-gist-and-dynamic-handle.t`）も非回帰。
    - **試して revert（重要教訓）**: 当初「クロージャの実スカラー lexical free var も `insert_sym` で強制」する広い版を
      試したが、`S17-scheduler/{at,in,every}.t`・`S32-io/IO-Socket-Async.t` を回帰させた（`:in`/`:at` 遅延スケジューリングの
      内部クロージャ scalar を強制上書きしてコールバックが過剰発火、every.t "seen 38 runs"）。`make test`・isolated 再現では
      露見せず、**release roast で main 5/5 PASS vs branch 5/5 FAIL の比較で確定**。`ContainerRef`-only の最小版に縮小して解決
      （`trans.t`/`squish.t` の二重カウント回帰も同時に消滅）。教訓: call_sub_value マージは全 interpreter クロージャ呼びの
      hot path で、scalar 値の強制は scheduler 等の内部クロージャに広く干渉する。**触るのは既に共有な `ContainerRef` だけに限る。**
    `make test` PASS（458 unit + 5336 prove）、clippy clean、int.t 0.20s、4 回帰ファイル release 8/8 PASS。
  - **次（step 2）**: needs-cell な `$` local を宣言時にセル化し、`owned_captures`/`closure_captured_state`/
    `box_captured_lexicals` の boxing ヒューリスティック（4→1）を削除（PLAN.md 実装順序 step 2）。
