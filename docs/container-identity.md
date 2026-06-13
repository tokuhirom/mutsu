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

#### 調査記録 2026-06-11（プロトタイプ実証 → 段階順を確定。コードは revert）

ユーザー方針 (2026-06-11): 「束縛要素のみセル化（escape-aware）」で進める — 全要素をセル化せず、
`:=` で束縛された要素だけを `ContainerRef` セルに昇格し、読みは単一チョークポイントで decont。
プロトタイプを当て、**機構の正しさと write-surface の広さを実証**した（その後 revert）:

- **現状の要素束縛は ad-hoc な後方参照**: `$x := @a[0]` は `Value::ArraySlotRef { array: Arc<Vec<Value>>,
  index }`（`value/mod.rs` `array_slot_ref`、`IndexAutovivify` opcode 経由・`:=` バインド時のみ emit）を
  `$x` に置く。`HashSlotRef`/`DeferredHashAccess` も同型。**浅い束縛（`$x:=@a[0]`/2 段）は動く**が、深い
  `$struct[1]<key><subkey>[1]` は**後方参照が stale 化**して落ちる（nested.t 3/4/7/8/19/21/26/28/31-37/40-43）:
  後続の write が経路を再解決する際、中間コンテナが COW で別 Arc にクローンされると SlotRef の捕捉 Arc が
  古いまま＝書きが束縛変数に届かない。
- **セルが正準解な理由（実証）**: 要素を `ContainerRef(Arc<Mutex<Value>>)` に昇格すると、**COW クローンでも
  Arc<Mutex> はクローン間で共有**されるため、leaf セルの identity が任意深度の経路を跨いで生存する。
  `array_slot_ref` を「要素を ContainerRef に in-place 置換し同じ cell を返す」へ変えると束縛が cell 共有になる。
- **read チョークポイント**: 単一 int 読みは `resolve_array_entry`（`vm_var_ops.rs:146`）の `Some(value)=>value.clone()`
  に `ContainerRef => cell.lock().clone()` を足せば decont される（**通常配列は ContainerRef 要素を持たないので no-op**）。
- **❌ ブロッカー = write-surface が広い（"deref/write-through everywhere"）**: `@a[i] = v` の要素書きが
  **十数箇所**に散在（`vm_var_assign_ops.rs` の 158/180/2511/2622/2692/3070… + 深い/named/generic index-assign の各
  arm、`array_slot_write`）。各サイトで「既存要素が ContainerRef なら cell へ write-through、でなければ置換」が要る。
  1 箇所でも漏らすと、その経路の書きが leaf cell を**置換して束縛を切る**（プロトタイプで単一レベル `@a[0]=99` が
  別経路を通り束縛が切れて実証）。同様に slice/iteration/`.raku`/native method の**要素 read も items を直接舐める**ので
  ContainerRef がそれらに漏れる（"deref everywhere" 回帰の入口）。

#### 確定した段階順（次セッションはここから）

big-bang は不可。**Phase 3 Stage 0 と同じく「チョークポイントを先に確立（挙動不変）」してからセル切替**する:

- [~] **Stage 0+1 — 配列要素の束縛セル化（着手・RHS 束縛 landed）**: Stage 0（write チョークポイント）と
  Stage 1（cell 切替）を 1 PR に統合実装:
  - **write チョークポイント** `Value::assign_element_slot(slot, val)`（ContainerRef なら write-through、でなければ
    置換）を新設し、配列要素 write の主要経路にルーティング（`assign_array_multidim` の leaf、`exec_index_assign_*`
    の単一 int / 2-level / deep-nested arm）。
  - **read decont** を `resolve_array_entry` / `resolve_hash_entry`（単一読みチョークポイント）に追加（通常配列は
    ContainerRef 要素を持たないので no-op）。
  - **cell 切替** `array_slot_ref` を「要素が *スカラー leaf* なら `ContainerRef` へ in-place 昇格、Array/Hash の
    *中間* 要素は従来 `ArraySlotRef`（Arc 共有 traversal を保持）」に。`$x` は同じ cell の ContainerRef を持ち、
    Phase 1 のスカラー cell 機構を再利用。**escape-aware**: 昇格は `:=` 束縛時のみ＝通常配列は bare 値のまま
    （perf 崖回避。#2746 の轍を踏まない）。
  - **値コンテキスト透過化**: 束縛要素 cell が native メソッドの raw items 読みで leak しても無害になるよう、
    `to_int`/`to_float_value`/`to_f64`/`arith_*`（reduce 等が raw items を fold）を ContainerRef decont 対応に。
  - **解けた**: 単一/2-level/深い array/hash 混在パスの **RHS 要素束縛**（`$x:=@a[i]`、`$x:=$s[1][1]`、
    `$abbrev:=$struct[1]<key><subkey>[1]`）— 書きが COW を跨いで届く（cell 共有が SlotRef の stale を解消）。
    nested.t 25→30。`make test` 緑（native-array-mut.t の flaky は Arc-ptr メタ既存・main と同率 ~35%）、
    common 配列/ハッシュ操作で leak なし。`t/element-bind-cell.t` 13 ケース。
  - **残（次スライス）**: LHS 束縛（`$struct[..]:=$var`、nested.t 7/8/26/28）・要素間束縛（`$a[i]:=$b[j]`、31-37）は
    BOUND_*_REF sentinel / varref 機構なので別途。ハッシュ要素の cell 化（`%h<k>:=` 深いパス）も未（HashSlotRef のまま）。
  - **調査記録 2026-06-11（hash 要素 cell 化を試行 → revert）**: `hash_autovivify` / `hash_slot_ref_lazy` に
    array 同型の scalar-leaf 昇格（`promote_hash_entry_cell`）と単一 hash-key write の write-through（`exec_index_assign_expr_named`
    の 3115 付近）を入れたが、**nested.t が 30→7 に激減＋ hash bind 自体も write-through せず**で revert。array 経由の
    深い束縛（`$struct[1]<key><subkey>[1]`）はハッシュ中間層を多数の hash-read/write 経路で traverse しており、hash 昇格が
    それらと干渉した。**教訓**: hash 要素 cell 化は (a) 全 hash element read を `resolve_hash_entry` 系チョークポイントに
    集約し、(b) 全 hash insert（`vm_var_assign_ops` に ~15 箇所: 163/1843/2144/2180/2614/2762/2784/3096/3113/3934…）を
    write-through ヘルパへルーティングしてから、で**ないと array-through-hash が壊れる**。array より write-surface が広い。
    順序: まず hash write チョークポイント確立（挙動不変）→ それから昇格、と array と同じ Stage 0→1 を hash でも踏む。
  - [x] **ハッシュ要素 Stage 0 — write チョークポイント確立（挙動不変・landed）**: 全 Raku-hash-value の
    要素 write を新ヘルパ `Value::hash_insert_through(map, key, val)`（`value/mod.rs`、`assign_element_slot` の
    hash 版＝既存 entry が `ContainerRef` なら write-through、でなければ insert/replace）へルーティング。配線済みサイト:
    `vm_var_assign_ops.rs` の 1843（typed hash element）/ 2144（fast-path 単一 hash key）/ 3115（generic index-assign
    の plain insert 分岐）/ 3699・3703（nested hash-in-hash）/ 3759（深い hash-in-hash）/ 3934（deep multidim final-level、
    既存インライン write-through を helper へ抽出）/ 4017（interior-mutation hash write）＋ `value/mod.rs` の
    `hash_assign_at` / `hash_slot_write`。**brand-new 空 map への insert**（2180/3350/3373/3953 等、`contains_key` 後の
    autoviv 中間層 3873/3910）は既存 entry が無く ContainerRef になり得ないので routing 不要・据え置き。
    現状はどの hash も ContainerRef entry を持たないので全呼びが plain insert/replace に縮退＝**挙動不変**（nested.t 30/43
    維持、`make test` 緑、`hash_chokepoint_tests` 3 ケース）。
  - [x] **ハッシュ要素 Stage 1 — 束縛 hash 要素の cell 昇格（landed）**: `array_slot_ref` 対称の新ヘルパ
    `Value::hash_slot_ref(key)`（`value/mod.rs`）を新設し、`:=` 束縛経路（`exec_index_autovivify_lazy_op` の
    `Value::Hash` arm、`vm_var_index_ops.rs`）を `hash_slot_ref_lazy` から差し替え。挙動: **既存 scalar leaf** →
    `ContainerRef` cell へ in-place 昇格（COW を跨いで生存）、**既存 Array/Hash 中間層** → 従来 `HashSlotRef` 保持
    （深い traversal を壊さない）、**欠落 key** → lazy のまま（write まで生成しない）。解けた: **深い hash leaf RHS 束縛**
    `my $y := %g<outer><inner>; %g<outer><inner> = 55; say $y # 55`（旧 HashSlotRef は inner hash の COW で stale→10）。
    - **過剰昇格バグ＋修正**: 中間 `%h<key>()...` の `<key>` が **Sub 値**（callable）の場合、bind context が CallOn の
      invocant にまで伝播して `<key>` を cell 化→`()` が ContainerRef を decont できず "CALL-ME on Sub" で abort
      （nested.t がテスト 9 後に全 abort）。修正: `compile_expr_call_on`（`expr_block.rs`）で `scalar_bind_autovivify` を
      save/false/restore（call の invocant・args は値読みで bind target ではない＝escape analysis の call-arg リセットと同型）。
    - **leak 修正**: hash `.values`（`collection.rs`）が ContainerRef を decont せず collect→`.sort`/比較が壊れる
      （`%h.values.sort` が `10,2,3`）。`v.deref_container()` で decont（**array `.values` にも同型の既存 leak あり**＝別途）。
    - 検証: nested.t 30/43 維持（Failed 7-8/11-12/26/28/31-37 は LHS/element-element の別機構で不変）、`make test` 緑
      （native-array-mut.t 26 は Arc-ptr メタ既存 flaky・main 3/8≈branch）、int.t 0.16s（perf 崖なし＝escape-aware で
      `:=` 束縛要素のみ昇格）、`t/element-bind-cell.t` 22 ケース（hash leaf 単一/深い + callable 非昇格 + iteration 非 leak）。
    - **残（Stage 2 へ）**: 欠落 key の lazy 束縛は DeferredHashAccess のまま（write で生成・promote）。slice/`.kv`/`.pairs`/
      `.raku` 等の他 raw-items 読みは cell leak の可能性（束縛要素限定で稀＝CI release roast で炙り出す）。
  - [x] **深い LHS 束縛の cell 化（landed・Stage 2 着手）**: `$struct[..]<..>[..] := $scalar`（`IndexAssignDeepNested`
    → `exec_index_assign_deep_nested_op`）に bind 処理を新設。従来この handler は bind_mode を一切扱わず `val`（bind payload）
    を生のまま代入していた＝深い LHS 束縛が完全に未動作だった。新ヘルパ `unwrap_bind_index_value`（payload 展開）で
    `(val, source)` を取り出し、**plain scalar source**（`\0idx\0` 無し）に限り: ①`val` で `ContainerRef` cell 作成、
    ②final-level で element に同 cell を直接格納（fresh `:=` は write-through でなく置換）、③ループ後に同 cell を source 変数
    スロット（`set_env_with_main_alias` + `update_local_if_exists`）へ書き戻し。両側が同 cell を共有＝RHS の対称形で、既存
    read（`resolve_array_entry` decont）/ write（`assign_element_slot` write-through）/ scalar var cell（Phase 1）チョークポイント
    を再利用。COW を跨いで生存（旧 `BOUND_ARRAY_REF_SENTINEL` の by-name back-ref は深さで stale だった）。
    - 結果: **nested.t 13→7**（7-8/26/28/31/34 を修正）、`make test` 緑、int.t 0.21s、let.t PASS（let/temp×cell OK）、
      `t/element-bind-cell.t` 31 ケース。単一レベル LHS 束縛（`@a[0]:=$v`、sentinel 経路 `IndexAssignExprNamed`）は未変更で温存。
    - **残**: ①`<key>()` を挟む LHS 束縛（nested.t 11-12、`IndexAssignGeneric`/method-lvalue 経路）。②element-element 束縛
      （`$a[i]:=$b[j]`、nested.t 32-33/35-37、`\0idx\0` encoded source）。
  - [x] **単一レベル LHS 束縛の cell 化＋配列コピー decont（landed）**: `@a[i] := $scalar`（`IndexAssignExprNamed` の
    単一-int array arm）を `BOUND_ARRAY_REF_SENTINEL` から cell へ移行。plain scalar source 限定で element に `ContainerRef`
    cell を格納＋ source 変数へ書き戻し（`pending_source_cell`、deep handler と同型）。element source（`\0idx\0`）は当面 sentinel
    温存。cell-bind は `mark_bound_index` 不要（cell 自体が alias）。**副次バグ修正**: `my @new = @array`（配列値コピー）が
    `coerce_to_array`（`runtime/utils.rs`）で items Arc を共有＝**cell 要素が漏れて snapshot されない**（`$var` 書きが copy に
    伝播）。これは LHS だけでなく **RHS 束縛の pre-existing leak**でもあった（Stage 1 #2902）。cell 要素がある場合のみ
    `deref_container()` で decont（Raku の `=` 値意味論。通常配列は Arc 共有のまま＝perf 維持）。arrays.t 回帰修正＋RHS leak も解消。
    nested.t 7 維持、`make test` 緑、int.t 0.16s、`t/element-bind-cell.t` 35 ケース（LHS/RHS の copy-snapshot 含む）。
    **sentinel 削除はまだ**（element source bind ②と read 経路が残存利用）。
  - **調査記録 2026-06-11（element-element / container-leaf 束縛 = 最難・プロトタイプ revert）**: nested.t 32-37 と
    `my $x := $foo[1]<key>`（**container-valued な終端要素**への束縛）を解こうとした。**再挑戦用の実装プラン（プロトタイプ
    コード片・残作業・循環安全性・leak チェックリスト・検証手順を埋め込み済み）= `docs/element-element-bind-plan.md`。**
    プロトタイプ（その後 revert）の知見:
    - **根本**: container 値の束縛終端要素（`$foo[1]<key>` が Hash）が HashSlotRef のままで COW stale。RHS scalar-leaf と異なり
      **container leaf を cell 化**する必要。だが `array_slot_ref`/`hash_slot_ref` は深い traversal 用に Array/Hash leaf を
      cell 化しない設計。→ **terminal-promotion** で解決: 束縛 RHS の最外（終端）index を compiler フラグ `bind_terminal`
      ＋新 op `IndexAutovivifyLazyTerminal` で識別し、終端だけ container leaf も cell 昇格（中間は SlotRef 保持）。`=:=` には
      非適用（過剰昇格回避）。terminal フラグは stmt.rs:767 ＋ SyntheticBlock/MarkBind 経路（helpers_block_inline）両方で要設定。
    - **しかし container cell への深い write-through が全 assign handler に波及**: cell が **Hash/Array を保持**するため、
      `%h<key><inner>=50`（cell 経由の深い write）は assign handler が ContainerRef 中間/root を decont して **cell の内側
      container を in-place 変異**せねば届かない。素朴には match が `_=>{}` で **write が落ちる**。対処として
      (a) 2-level nested handler に `assign_into_nested_container`（ContainerRef descent）, (b) deep handler の raw-pointer walk に
      `descend_container_ref`（Mutex data ポインタへ降りる）を入れたら **単一レベル container 束縛は解決**（`my $x:=%h<key>` の
      双方向 OK）。だが **RHS container-leaf は 1/3 方向のみ**（逆方向 `$x<subkey>[1]=8` は root `$x` 自身が cell＝nested/generic
      handler が root を Hash として読めず write 落ち）、**element-element は未解決**。
    - **結論＝最難たる所以**: container-leaf cell は **全 index-assign handler（named/nested/deep/generic）の root 読みと
      中間 traversal すべてに ContainerRef-descent が要る**（"deref everywhere" surface）＋ **循環安全性**（nested.t 268+ の
      自己参照束縛で descend ループが無限ループ化しうる）＋ **container cell の leak 硬化**（iteration/.raku/slice）。
      単一 PR では安全に閉じない。terminal-promotion 機構と 2 つの descent ヘルパは有効だが、root-cell descent の全 handler
      展開＋循環検出＋leak 検証を伴う専用作業が要る。プロトタイプは binding sweep 回帰無しだが target（32-37）未修正・
      make test full 未確認のため revert。再挑戦時はこの順で: ①terminal-promotion ②全 handler root/中間 descent（循環検出付き）
      ③container-cell leak 硬化 ④release roast。
  - **✅ LANDED (PR #2922 + #2925, 2026-06-12)**: 上記プランを Phase 1-5 順で逐語適用し element-element /
    container-leaf / cyclic 束縛を実装、`docs/element-element-bind-plan.md` 通りに完遂。**nested.t 43/43（ホワイト
    リスト追加）**。#2922 が 32-37（terminal-promotion ＋ `assign_into_nested_container`/`descend_container_ref` ＋
    nested/named handler の `env_root_descended_mut` root descent ＋ RHS の `compile_bind_index_value` terminal
    promotion ＋ read-side decont ＋ `MAX_DESCENT` 循環ガード ＋ `.raku`/`.gist` leak 硬化）。**プラン外で発見した真因**:
    deep-nested traversal の中間 array level の `needs_viv` が既存 `ContainerRef` 要素を vivify で上書き破壊（cyclic の
    off-by-one の正体）→ `needs_viv` から `ContainerRef` 除外で解決。#2925 が 11-12（`$struct[1]<key>()<subkey>[1]`
    = `is raw` sub mid-path 束縛）: stack-target IndexAssign の generic handler Array arm を `assign_element_slot` 経由に
    して bound cell を write-through（直接置換が cell を破壊していた）。`t/element-bind-cell.t` 35→54。
- [x] **`is rw` param 真の共有セル（scalars.t 24/27）= LANDED (#2928)**: `is rw` param への `:=` 再束縛が
  live `ContainerRef` cell で caller 変数と共有される。take-rw も #2930 で landed（gather.t 38）。
  実装プラン（履歴）= `docs/is-rw-shared-cell-plan.md`。scalars.t 33/33。
- [~] **Q2 型メタ Arc-ptr flaky の構造的除去（2026-06-12、2 本立て）**:
  - **① Hash = HashData 埋め込み（完全吸収）DONE (#2952)**: `Value::Hash(Arc<HashData>)` に
    value_type/key_type/declared_type を埋め込み、`hash_type_metadata` 副テーブルと
    `reconcile_hash_type_metadata_from_name` を削除。メタが COW と共に移動＝ptr 再利用の誤継承が Hash で消滅。
    計画・残作業（original_keys の埋め込み = Stage 2、その後 Array/Set/Bag/Mix へ同 wrapper 展開）=
    `docs/hashdata-migration-plan.md`。
  - **② 未 wrapper テーブルの Weak-guard 化 (#2953)**: `runtime/ptr_keyed.rs` の `PtrKeyedMap`
    （`HashMap<usize,(Weak<T>,V)>`）へ: `array/mix/set/bag_type_metadata`・`container_defaults`
    （array/hash に分割）・utils グローバルの `shaped_array_ids`・`grep_view_bindings`。
    - **機構**: エントリの `Weak` が ArcInner を pin → **エントリ生存中はアドレス再利用が物理的に不可能**＝
      死んだ typed コンテナのメタを新コンテナが誤継承する間欠 flaky（S02-names-vars/perl.t・native-array-mut.t 等の
      alloc 順依存死）が構造的に消滅。lookup は `strong_count>0` 検証、insert 時に閾値超過で dead エントリ sweep
      （無限成長と pin メモリも解消）。`remove` は dead でも値を返す（pre-COW 回収 = migrate 用）。
    - **衝突防御ハック撤去**: `clear_container_default`（+4 サイト）、`vm_call_method_mut_ops` の make_mut 後
      防御 unregister ×2。
    - **★ 重要副作用と対処**: `Weak` 保持により `Arc::make_mut` は strong==1 でも**必ず再配置**（weak>0 ルール）→
      「unique 所有 in-place 変異はポインタ安定」という暗黙依存が壊れる（guard 付きコンテナのみ）。顕在化 3 経路を
      heal: VM native push（`exec_array_push_op` に `capture_container_meta`/`restore_container_meta` = 型メタ+
      `is default`）、interpreter push arm（`reattach_array_type_metadata`）、**`.WHICH` 安定性**（2-level nested
      hash write の outer を strong==1 時は raw-ptr in-place 書き — cast は必ず `*mut HashData`、
      hashdata-migration-plan の UB 教訓参照）。**新たな heal 漏れは「typed/shaped コンテナの変異後メタ消失」または
      「.WHICH 変化」の形で出る** — tmp/typed-meta-smoke{,2}.raku / tmp/which-map.raku を main と比較が検出法。
    - **hash original-keys 2 テーブル（`hash_object_keys`・`hash_original_keys_registry`）は意図的に対象外**:
      object-hash 構築は反復 make_mut の in-place ポインタ安定性に依存しており（guard を被せると
      S09-typed-arrays/hashes.t の .antipairs/.invert が回帰）、正解は ① Stage 2 の HashData 埋め込み。
    - **残（未ガード）**: Seq 系 ptr-key（`predictive_seq_iters`・`squish_iterator_meta`・
      `__mutsu_predictive_seq_iter::` env キー）と WHICH 文字列の `{:p}` 埋め込みは別サブシステムで同 hazard が残存（小）。
    - 検証: cargo test 466（PtrKeyedMap 単体 5 含む、stale-inheritance 不可能性の決定的テスト）、make test 727/6588、
      whitelist S09×17 + perl.t×3 + map.t + S02-types/hash.t + let.t + typed-hash canary 3467 テスト、int.t 0.163s。
- [~] **Stage 2 — 深い `>>++`/`deepmap`/object-hash・既存 SlotRef 機構の撤去**（着手 2026-06-13、スライス進行中）:
  - [x] **slice 1 = deepmap コンテナ渡し (#2964)**: leaf を transient `ContainerRef` cell 経由で callable に渡し、
    呼び出し後に cell 値を source Array/Hash slot へ in-place 書き戻し（cell は残らない）。結果は decont して
    identity block の cell leak を防止。hyper.t 330-333 修正（12→9 fail、残は nodal/callable 別機能)。
    t/deepmap-container.t (16)。
  - [x] **slice 2 = bind 経路の中間 SlotRef 撤去 (#2966)**: `exec_index_autovivify_lazy_op` の中間 Array/Hash
    要素を SlotRef back-ref で包まず**要素値そのもの（inner Arc 共有）を返す** — #2902-#2925 後は leaf promotion が
    共有 Arc 内で in-place に起きるため back-ref は不要だった。`array_slot_ref`/`hash_slot_ref` の中間 arm 2 箇所。
  - [x] **slice 3 = BOUND_HASH_REF_SENTINEL 全廃**: 生成 3 経路（単一 hash-key bind named handler・
    slice bind・`.BIND-KEY` ×2）を「`ContainerRef` cell を hash entry に格納 + source 変数へ同 cell
    書き戻し」へ置換し、consumer（resolve_hash_entry/hash_has_sentinels/resolve_hash_for_iteration の
    BOUND arm・sentinel write-through arm）と sentinel 定数を削除。設計ポイント:
    (a) **element source（`%h<k> := @a[0]`）は val が既に LazyTerminal 昇格済みの cell** — そのまま entry に
    格納（旧 sentinel は `\0idx\0` 名を env に書く壊れた経路で、hash-key write が element に届かなかったのを
    正攻法修正）。(b) **cell 再利用 pre-read**: source が既に cell-bound（`@arr[0] := $x; %h<k> := $x`）なら
    既存 cell を共有（新 cell で上書きすると別 alias が切れる — これも既存バグ修正）。borrow 前に pre-read
    する `BindSourceCell` 型。(c) **代入スナップショット**: `%new = %hash` は `resolve_hash_for_iteration` で
    cell を decont（assignment creates new containers、S03-binding/hashes.t 30）。(d) `@`/`%` source の
    cell-bound 変数への `.push` は `exec_array_push_op` fallback で decont→dispatch→cell 書き戻し
    （array 側 #2914 由来の既存 fail も修正）。t/hash-bind-cell.t (19)。
  - **残スライスのマップ**:
    1. **BOUND_ARRAY_REF_SENTINEL 撤去**: 残る生成は element-source bind（`\0idx\0` encoded、vm_var_assign ~4263 の
       ArraySlotRef 設置と対）。#2922 の terminal-promotion を named 単一レベル handler の element-element にも適用。
    2. **hash_autovivify / 非 lazy 連鎖の SlotRef 撤去**: write-autoviv 連鎖（`%h<a><b> = 5`）の中間/終端 slot-ref。
       終端 assign 先の表現を要再設計（毎 write の cell 化は cells-everywhere になるため、終端は呼び出し側で
       書き戻す形が候補）。
    3. **hash_slot_ref_lazy / DeferredHashAccess（missing-key lazy bind）**: `:exists` を汚さない遅延束縛が要件。
       cell 化するなら「phantom entry」相当の設計が必要 — 最後に回す。
    4. **grep-rw-view 撤去**: 登録ゲート下で matched 要素を cell 昇格し view registry（最後の ptr-keyed
       グローバルの一つ）を全廃する案。for-loop rw topic (vm_control_ops ~410/613) が消費者。
    5. 全生成が死んだら consumer 群（ArraySlotRef 41 / HashSlotRef 62 / DeferredHashAccess 31 refs）と
       variant 自体を削除。

**必須検証（各 Stage）**: nested.t、`make test`、**release roast の main-vs-branch 比較**（PR #2898 で
typed-array 修正が make test を通過しつつ release roast の `S06-currying/misc.t` を回帰させた轍＝要素変更は
release でしか出ない leak がある）、int.t の wall-clock（perf 崖カナリア）。

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

- [x] **Stage 0 — カプセル化境界の確立（挙動不変）完了 (#2856)**: `InstanceAttrs` から `Deref`/`DerefMut` を撤去し、
      代わりに `HashMap` と同シグネチャの inherent メソッド（`get`/`contains_key`/`insert`/`get_mut`/`entry`/`iter`/
      `keys`/`values`）＋owned アクセサ `as_map()->&HashMap` / `to_map()->HashMap` を追加。これで属性ストレージへの
      全アクセスがメソッド境界を通る（Deref 漏れを排除）。**内部表現は HashMap のまま**＝byte-identical。
      Deref 撤去でコンパイラが ~197 サイトを列挙：deref-coercion（`fn(&HashMap)` へ `&Arc<InstanceAttrs>` 渡し）は
      `.as_map()` へ、`(**attributes).clone()` は `.to_map()` へ機械置換。make test 6112 全緑・cargo test 458/0・
      clippy 緑。これで Stage 1（内部を共有可変セルへ）の表現切替が局所化された。
- [x] **Stage 1 — 表現切替（共有セル化）完了**: `InstanceAttrs.attributes` を `HashMap` → `Arc<RwLock<HashMap>>`
      （`AttrCell`）に。`Value::Instance` は従来どおり `Arc<InstanceAttrs>` を保持。
  - **read API**: `as_map()` は `RwLockReadGuard`（`Deref`→`&HashMap`）を返す。チェーン呼び（`.get`/`.iter`/`for`）は
        無改変、`fn(&HashMap)` へ渡す ~87 サイトは `&` 付与のみ。`get()`（owned 化は `*x`/`.cloned()`/`and_then(fn(&Value))`
        を全部壊す）は**削除**し、全 `.get()` を `.as_map().get()` へ降ろして借用セマンティクスを温存（504 サイト機械置換）。
  - **mutation**: `insert`/`insert_if_absent`/`with_attr_mut` は `&self`（write-lock で in-place）。`make_mut(attributes)`
        サイトは直接 `&self` 変異へ。
  - **クロスフレーム可視化（バグ修正の本体）**: `id→Weak<cell>` レジストリ（`instance_cells`）を新設。
        `make_instance_with_id` は既存の生きたセルを**再利用**（map を書き込んで alias 共有のインスタンスを返す）。
        `overwrite_instance_bindings_by_identity` はスキャン前に `update_instance_cell(id, &updated)` で**生きたセルへ
        直接書き込む** → スキャンが訪れない caller フレームの alias にも変異が届く。これで `note`/$*ERR・closure 経由
        instance mutation が解消。レジストリは Stage 2 で scan ごと撤去するための足場。
  - **`.clone`（Raku 独立コピー）保全**: `InstanceAttrs::clone` を手書きで**deep copy（新セル・queue_destroy=false・
        非登録）**に。共有は `Arc<InstanceAttrs>`（Value clone）経由のみ。これで ~30 の `(*attributes).clone()`
        writeback サイトは無改変で独立コピー意味論を維持。`temp`/`let` save も同型の独立スナップショットが必要だった
        （`into_temp_snapshot` で instance を deep copy、restore は `make_instance_with_id` で**生きたセルへ書き戻し**
        identity と alias 可視性を保つ）— これを怠ると `t/lvalue-method-rw.t` の temp 復元が壊れる（実際に踏んで修正）。
  - 検証: cross-frame note/closure mutation・alias 共有・`.clone` 独立・`===`/`.WHICH`/`eqv` identity・DESTROY(1回)・
        temp/let 復元 すべて raku 一致。clippy 緑。
- [ ] **Stage 2 — cell を唯一の真実源に（keystone）＋ 伝播ハック全廃**: ユーザー判断 (2026-06-10) で当初**一括 PR** 想定 →
      **配列/ハッシュ属性の in-place 変異（`@!a.push`/`%!h<k>=`）の cell 書き戻し配線が別プロジェクト規模**と判明したため、
      ユーザー再判断 (2026-06-10) で **sigil でスライス**（スカラー先行）に変更。
  - [x] **Stage 2a — スカラー属性 (`$!x`/`$.x`) を cell 直結（landed: branch `phase3-stage2-scalar-cell`）**:
        `Var("!x")`/`Var(".x")` の read を `self` の共有 cell 直読に（`read_self_attr_cell`、atomic/CAS チェックの後段＝CAS は
        従来 `shared_vars` 維持で livelock 回避）。write 経路 — `exec_set_local_op`/`exec_assign_expr_local_op`（ラッパーで
        `mirror_attr_local_to_cell`）、名前ベース `exec_assign_expr_op`（`$.x = v`/`$!x = v`、`mirror_attr_value_to_cell_by_name`）、
        post/pre inc・dec（`sync_attr_local_from_cell_by_name` → 実行 → mirror）— を全て cell 書き込みに。**スカラー writeback
        撤去**（`writeback_attributes*` を array/hash 専用化、`AttrSlots::private/public` 削除）。代わりに method exit の
        live env/locals 段階で `reconcile_scalar_attrs`：「env/local が entry スナップショットから変化 → その値、不変 → live cell
        値（cross-frame/cell-direct 変異を採用）」で attr マップを確定し make_instance_with_id へ。これが **attributive param
        (`method m($!s)`)・no-twigil sigilless attr (`has $x`)・`is rw` accessor 書き込み**（cell 非経由で env を書く 3 経路）を
        cell に届ける。**cross-frame スカラー変異バグ修正**（`self.bump` が caller の `$!x` に可視、11 vs 旧 10）。配列/ハッシュ属性・
        CAS・registry/scan/detached/`instance_cells` は Stage 2b まで維持（まだ削除不可）。
        検証: make test 全緑、S12-attributes/instance・native、S12-methods/{attribute-params,lastcall,defer-call,defer-next}・
        S17-lowlevel/cas（whitelist）PASS。継承同名 private（Parent/Child `$!p`）は main と同挙動の pre-existing バグで非回帰。
  - [x] **Stage 2b — 配列/ハッシュ属性 (`@!a`/`%!h`) を cell 直結（landed: branch `phase3-stage2b-array-hash-cell`）**:
        read を `GetArrayVar`/`GetHashVar` で `self` の cell 直読（`read_self_attr_cell` を全6 twigil 対応に一般化＝
        `scalar_attr_twigil_base`→`attr_twigil_base`、`(bare, is_private)` 返却）。各変異 op の **後**に env→cell ミラー
        （`mirror_array_hash_attr_to_cell`）を配線: `CallMethodMut`/`CallMethodDynamicMut`/`ArrayPush`/`IndexAssignExprNamed`/
        `MultiDimIndexAssign`/名前ベース `AssignExpr`。**重要＝op 前に env スナップショット**（`array_hash_attr_env_snapshot`）を取り、
        env が実際に変化したときだけミラー：非変異メソッド（`@!a.join`）も `CallMethodMut` で来るため、stale env コピーを
        無条件ミラーすると cross-frame の cell 変異を clobber する（cf3 で実証・修正）。**スカラー reconcile を全 sigil 統一**
        （`reconcile_scalar_attrs`→`reconcile_attrs`、ルール「cell が entry から変化→cell 優先、でなければ env 変化を採用」＝
        cross-frame/per-op ミラーは cell が勝ち、attributive array param `method m(@!a)`・sigilless は env が勝つ）。
        これで **array/hash writeback (`writeback_attributes*`) を完全撤去**（`AttrSlots` 構造体 + `compute_attr_slots` も削除、net -115 行）。
        **cross-frame 配列/ハッシュ変異バグ修正**（nested method の `@!a.push`/`%!h<k>=` が caller に可視）。
        検証: make test 全緑、S12-attributes/methods・S14-traits・S17/cas（whitelist）PASS、Stage 2b edge 21/21
        （push/pop/shift/unshift/要素代入/`:delete`以外/whole-assign/cross-frame array+hash/attributive param/`.=`/chain/read-no-clobber）。
        既知の非対応（pre-existing・非回帰）: `%!h<x>:delete`（DELETE-KEY が Index target 経由でミラー外）、`@b[0]=v` のスペース無し
        パース、継承同名 private。**materialize の array/hash 挿入 / by-id scan (`overwrite_instance_bindings_by_identity`) /
        registry (`instance_cells`) / detached は未撤去**＝Stage 2c（次）で cell 単一源を根拠に全廃。
  - [~] **Stage 2c — 伝播ハック全廃（着手・slice 1 done）**: cell が全 attr の単一源になったので伝播スキャンを撤去。
        - [x] **slice 1 — by-id env/locals scan 撤去（branch `phase3-stage2c-drop-byid-scan`）**: `overwrite_instance_bindings_by_identity`
              の `for bound in self.env.values_mut()` 走査 + 再帰ヘルパ `overwrite_instance_recursive`（Instance/nested-attr/Mixin/
              ContainerRef の各 arm）と、locals 走査 `overwrite_instance_in_locals`（+ 6 call sites）を**全削除**。伝播は単一の
              `update_instance_cell(id, &updated)`（live shared cell への直書き）に集約。**根拠**: Stage 1/2a/2b 後、instance の
              全 alias（同一フレーム・caller フレーム・`ContainerRef` boxed capture・role `Mixin`・他 instance のネスト属性）は
              `Arc<InstanceAttrs>` 参照共有で**同一 cell を見る**（deep-copy は明示 `.clone` のみ）→ scan が rebuild していた
              holder は元から同じ cell を共有しており scan は観測上 no-op。`_class_name` は vestigial（cell は id keying）だが
              ~40 call site churn 回避のため signature 維持（後続 slice で撤去）。検証: make test 6247・S12/S14/S17 whitelist
              106 ファイル 2111 テスト PASS（cross-thread cas・全 role/mixin・cross-frame note/closure・nested・escaping-closure
              scalar-holding-instance すべて緑）、clippy 緑。net -108 行。
        - **materialize/reconcile 撤去（ユーザー選択 2026-06-11、段階 PR）**: reconcile は `attributes`(entry snapshot) を最終値へ
              書き換え、cell 非経由の2経路を merge する: (a) attributive param (`method m($!x)`)、(b) sigilless `has $x`（bare `Var("x")`
              にコンパイルされ実行時 alias テーブルのみが属性と判別）。`is rw` accessor 等は既に cell に届く。reconcile を単純撤去すると
              caller の cell writeback が snapshot で cell を巻き戻すため、最終的に `reconcile → attributes = base.cell.to_map()`（cell 単一源）
              へ置換する。exit-mirror は 2b 実証の stale-clobber を起こすため write-time の cell routing が必須。→ (i) attributive param、
              (ii) sigilless cell-direct（2a 相当）、(iii) reconcile/materialize 撤去、の3段に分割。
          - [x] **(i) attributive param → cell（landed: branch `phase3-stage2c-registry-removal`）**: binding 直後に
                `mirror_attributive_params_to_cell`（`mirror_attr_value_to_cell_by_name` 再利用、scalar/array/hash 全 twigil）で param を
                self の cell へ。**併せて read-only fast path から attributive param を除外**（`attr_twigil_base(pd.name).is_some()` を
                `has_complex_params` に追加）—— attributive param は属性を変異させるのに fast path は writeback を落とすため `$obj.m($!x)`
                後に変異が消えるバグ（`method set-and-read($!x){ say $!x }` が in-body 7 でなく stale cell 99 を読み属性も 99 のまま）を修正。
                検証: make test 6247、S12/S14/S17/S06 whitelist 140 ファイル 2890 テスト PASS、named `:$!x`/BUILD/array/hash param・
                in-body read・cross-frame すべて raku 一致。reconcile は (ii)(iii) まで維持（attributive param 経路は今や mirror で冗長だが無害）。
          - [x] **(ii) sigilless `has $x` を cell-direct 化（landed: branch `phase3-stage2c-sigilless-cell`）**: bare `Var("x")`
                （sigilless attr）を実行時 `__mutsu_sigilless_alias::` テーブル参照で cell に回す。**read**: `read_self_attr_cell` を
                `canonical_attr_twigil`（直接 twigil → そのまま / bare sigilless → alias chain 辿って `!x` twigil 解決）経由に拡張。
                sigilless `$x` は method local 宣言でないため **GetGlobal** で読まれる（GetLocal でない）と判明 → vm.rs の GetGlobal handler
                にも cell-direct read を追加（env read の前）。**write**: 6つの alias-chain 伝播ループ（inc/dec 4 + name-based assign 1 +
                SetGlobal 1）の各 alias target に `write_self_attr_cell` を追加し attr-twigil alias (`!x`) を cell へミラー。inc/dec の
                4ループは重複していたので共通ヘルパ `propagate_sigilless_alias_chain` に factor（cell mirror 込み）。**perf**: read 経路は
                hot なので新フラグ `Interpreter::sigilless_attrs_active`（materialize で sigilless alias 設定時に立てる process-sticky bool）で
                ゲート → 非 sigilless プログラム（大多数）は string check のみ。**修正バグ**: 同フレーム/nested-frame の sigilless read が
                entry env コピー（stale）を読んでいた（`method outer { self.inner; $y }` が inner の変異後 10 でなく 99 を返すべき所で 10）。
                検証: make test 6247、S12/S14/S17/S06/S32-num whitelist 171 ファイル 6432・S02/S03/S04 318 ファイル 32004 PASS、
                int.t perf 0.085s（回帰なし）、nested-read/same-method write-read/inc-dec/closure/multi-attr すべて raku 一致。
                reconcile は (iii) まで維持（sigilless 経路は今や cell 直結で reconcile case-2 は冗長だが無害）。
          - [x] **(iii-a) reconcile を cell 単一源に簡素化（landed: branch `phase3-stage2c-reconcile-removal`）**: `reconcile_attrs` の
                entry-snapshot-vs-env の値比較（case-1 cell-changed / case-2 env-changed の優先判定 + sigil-by-type twigil 推定）を撤去し、
                `*attributes = base.cell.to_map()`（cell が単一源）に。全 plain write（2a/2b/2c-i/ii）は write-time で cell に着地済みなので
                cell snapshot が最終値。**例外 = `:=` 束縛属性**（`$!x := $outer`）: 外部 ContainerRef を env/locals に持ち cell を経由しない
                **第3の bypass**（has-attr-binding.t で発覚）。cell.to_map() 後に env/locals の各 attr key（bare sigilless + 6 twigil）を走査し
                ContainerRef なら優先採用して `:=` alias を method exit 越しに保全。検証: make test 6260、S12/S14/S17/S03-binding/S06 144 ファイル
                2994 PASS、has-attr-binding 6/6、全 bypass/sigilless/attrparam テスト raku 一致。（S32-temporal/DateTime.t の 2 失敗は
                `Date.today`=UTC vs `DateTime.now.Date`=local の pre-existing timezone バグで、変更退避した baseline でも同一＝非関連・
                ローカル JST のみの artifact、CI=UTC では両者一致でパス。）
          - [x] **(iii-b) slow-path materialize の attr-value env コピー撤去（landed: branch `phase3-stage2c-materialize-removal`）**:
                complex-param メソッド経路（`call_compiled_method`）の attr value env コピー挿入（`!attr`/`.attr`/`@!attr`/`@.attr`/
                `%!attr`/`%.attr`、~30行）を撤去。reads は全 cell-direct（2a/2b/2c-ii）、exit reconcile は cell.to_map()（iii-a）なので冗長。
                sigilless alias テーブル設定 + `is default` 登録は保持。検証: make test 6265、S12/S14/S04/S03-binding/S06 144 ファイル 3076 PASS、
                closure が private/array/hash/sigilless attr を捕捉・読み書きするケース（reader/writer 返却・nested+closure・map/gather）すべて raku 一致。
                ~~**fast-path（`call_compiled_method_fast`）の array/hash env コピーは保持**~~ → **(iii-c) で撤去済み（下記）**。
          - [x] **(iii-c) 変異 op の pre-op cell sync + fast-path env コピー撤去（branch `phase3-attr-cell-presync`）**:
                `array_hash_attr_env_snapshot`（全変異 op サイトの pre-snapshot）を「**self の live cell から env/locals を refresh**
                してから cell 値を pre として返す」へ拡張（Arc ptr 比較で同一なら no-op、`:=` ContainerRef は legacy 維持）。
                これで closure-captured env コピー（closure 生成時の stale snapshot）を変異してミラーする事故が消え、
                **pre-existing バグ「closure 経由 `%!h{$k}=$v` は最後の write しか残らない（最初の write が `(Any)`）」を修正**。
                各 closure 呼び出しが live cell 値から開始する。修正後、`call_compiled_method_fast` の array/hash attr env コピー
                （iii-b で保持した分）を撤去 — 読みは cell-direct（2b）、変異は pre-op sync が env を埋めるので不要。
                **おまけ: `DeleteIndexNamed`（`%!h{$k}:delete`）にも同じ pre-sync + mirror を配線**し、Stage 2b の
                pre-existing gap「DELETE-KEY がミラー外で cell に届かない」も修正。回帰テスト `t/closure-attr-element-write.t`（16）。
        - [x] **registry 撤去（done, branch `phase3-drop-instance-cell-registry`）**: `instance_cells`（`id -> Weak<cell>` グローバル
              Mutex registry）+ `register_instance_cell`/`lookup_instance_cell`/`update_instance_cell`/`overwrite_instance_bindings_by_identity`
              を**全廃**。~98 の writeback/rebuild サイト（`overwrite_*` 47 + rebuild `make_instance_with_id` 51）を、receiver の
              `Arc<InstanceAttrs>` cell を直接 in-place 変異する 3 つの新ヘルパへ全変換:
              `InstanceAttrs::commit_attrs(map)`（live cell へ in-place 書き、deadlock-safe な `write_cell_respecting_reads` 経由）/
              `Value::instance_sharing_cell(attrs, class, id)`（Arc を共有して rebuild、class 変更時のみ cell 共有の新 InstanceAttrs）/
              `Value::write_back_sharing(attrs, class, map, id)`（commit + share の複合）。**`make_instance_with_id` から id→cell reuse 分岐を削除**
              （以後 fresh cell 専用＝genuinely-new / sentinel id 用のみ）。snapshot+id しか持たなかった `proxy_store`・buf write・MRO multi
              invocant 等は receiver の Arc を呼び出しチェーンへ通して解決。`make_instance_detached` は registry 撤去後 `make_instance_with_id`
              と等価になったが CAS の atomic-sync 意図マーカとして名前のみ残置（→ `make_instance_with_id` 委譲）。`_class_name` vestigial param 撤去。
              副次効果: グローバル Mutex registry の Weak エントリ蓄積（メモリリーク）解消、id-0 sentinel Supply の cell 誤共有 latent バグ解消。
              検証: cargo test 461、clippy 緑、whitelisted S12/S14/S17/S32-temporal/buf 40 ファイル + cross-frame/closure/temp-let/proxy/buf/iterator/grammar 全緑。
        - [x] **CAS cell-CAS 化（branch `phase3-cas-cell`）**: instance 属性の cas/atomic ops を shared_vars 側道
              （`!attr::{id}` 値キー / `__mutsu_atomic_attr::{id}::{attr}` / `__mutsu_instance::{id}` 親回収 + env scan-replace /
              `make_instance_detached` / `sync_atomic_attribute_to_instance`）から **cell 直接の atomic primitive** へ全面移行。
              `InstanceAttrs::compare_and_swap(key, matches, new)`（単一 write lock 下で比較+格納）/ `fetch_update(key, f)`
              （単一 write lock 下で RMW、atomic add/inc/dec 用）を新設し、`builtin_cas_var`（3-arg/2-arg）・
              `builtin_atomic_update_unit`・`builtin_atomic_{add,fetch_add,fetch,store}_var` に attr-cell 分岐
              （`self_attr_cell_target`: `!x`/`.x` → self の cell + qualified key 解決）。Stage 1 の livelock は
              cell-direct read（2a）完備で解消（cas.t 5 連続 PASS で確認）。
              **発見した第2の lost-update race**: method exit の reconcile snapshot を caller が毎回
              `cell.commit_attrs(new_attrs)` で whole-map 書き戻し → 並行 cell-RMW を TOCTOU clobber
              （8スレッド `$!count⚛++` ×500 が 2448/4000。main では親に全く伝播せず **0** だった）。
              **修正**: `reconcile_attrs` が「cell snapshot を超える調整（`:=` ContainerRef 回収）があったか」の
              bool を返し、`(Value, HashMap, bool)` で呼び出し元へ伝播 — 未調整なら exit commit / `write_back_sharing`
              を **skip**（snapshot==cell なので no-op、かつ race 源）。調整あり（`:=`）のみ commit。
              副次効果: 毎 method exit の冗長 whole-map 書き込み消滅（perf）。回帰テスト `t/cas-cell-attr.t`（14、
              4スレッド increment/2-arg cas/await 後親可視を含む、raku 一致）。
  - 旧「一括」設計（下記）は Stage 2a/2b/2c に分割。以下は参照マップ。

  #### 現状メカニズム（CI 調査で確定したフルマップ）
  method frame は instance attr を **env/locals コピー**で持ち、cell は writeback でしか同期されない:
  - **materialize（method entry）**: `vm_method_dispatch.rs:~380-430` が attr ごとに env `!attr`/`.attr`/`@!attr`/`@.attr`/
    `%!attr`/`%.attr` を **value コピー**で挿入。private は `qualified_key = "{owner_class}\0{attr}"` を優先
    （継承の Parent/Child 同名 disambiguation）。`is default(...)` は `set_var_default("!attr")` で登録。
    sigilless alias（trait handles）は `__mutsu_sigilless_alias::` で双方向。
  - **read（body）**: `$!attr` は **plain `Var("!x")`** にコンパイル → `exec_get_local_op`（`vm_var_assign_ops.rs:4131`）が
    `locals[idx]`（env `!attr` から dual-store sync 済み）を返す。cas dirty は env 優先（4147 コメント）。
  - **write（body）**: `exec_set_local_op`（4281）/`exec_assign_expr_local_op`（5480）。type-object への `$!attr=` は die。
    bind/rebind/constant/vardecl/decont-marker など多数の context と絡む。
  - **writeback（method exit）**: `writeback_attributes_from_locals`/`writeback_attributes`
    （`vm_method_dispatch.rs:586/607/1088/1105`）が locals/env の `!attr` を instance attrs へ集約 → 再構築。
  - **cas 側道**: `$!attr` の cas は env `!attr` + `shared_vars`（`__mutsu_atomic`/`__mutsu_instance::`）が真実源、
    cell は `make_instance_detached` で別途同期（Stage 1 で race 回避のため detached 化）。

  #### keystone（実装方針）
  `$!attr`/`$.attr`/`@!`/`@.`/`%!`/`%.` の read/write を **self の cell 直結**にする:
  - var handler に attr-twigil 判定（`attr_twigil_base(name) -> Option<&str>`: `!x`/`.x`/`@!x`/… → `x`）を入れ、
    env `self`(Instance) の cell から read / cell へ write。継承 qualified key は cell 側に `owner\0attr` を保持するか、
    owner_class を frame に持たせて解決（materialize と同じ優先順位を cell read で再現）。
  - **materialize 撤去**（env コピー挿入をやめる）、**writeback 撤去**（`writeback_attributes*`）、
    **cas を cell-CAS 化**（cell の write-lock を atomic primitive に。Stage 1 で livelock したのは read が env コピー経由
    だったから＝cell 直結後は解消する。これが cell-direct read の検証ケース）。
  - これらが入れば **`overwrite_instance_bindings_by_identity` + by-id scan（17ファイル~50サイト）/ `instance_cells`
    レジストリ / `HELD_READ_CELLS`+deferred-write / `make_instance_detached` / `update_instance_cell` / CoW `make_mut`16**
    を全廃。`temp`/`let` の cell 書き戻しも素直化。

  #### リスク / 検証
  - hot path（var read/write）と dual-store（locals↔env sync）の中核を触る＝Stage 1 級の blast radius。CI 反復前提。
  - 必須検証: 継承同名 attr（Parent/Child `$!priv`）、`@!`/`%!` の container type metadata、sigilless alias（trait handles）、
    `is default`/`.VAR.default`、rw accessor、cas（cell-CAS で 4000）、cross-frame、temp/let、DESTROY。
  - perf: cell read は env コピー読みより重い（self 取得 + read-lock + clone）。Stage 3 の escape 解析で非エスケープ
    instance を bare 値に戻して救済。
- [ ] **Stage 3 — 仕上げ / perf**: escape 解析で「捕捉も `.clone` もされない instance はセル省略」（hot path 救済。
      Phase 1 のスカラーと同型）。型メタ副テーブルの Arc-ptr keying もセルに載せて廃止（Q2 flaky 吸収）。
      **再評価（2026-06-12 perf 計測で確定）**: 両サブ項目とも**現時点では着手しない**。
      - **escape 解析セル省略は数値が支持しない**: bench-class heavy の release プロファイルで cell の
        RwLock は self-time top30 に**現れない**。支配的コストは **allocator ~48%**（`_int_malloc` 18% +
        `memmove` 12% + realloc/free）＝ `Value::clone` + HashMap churn。startup 補正後の実効倍率は
        bench-class **~4x** / method-call **~5.5x** raku（startup 込みでは 2.1x/2.2x、mutsu startup 0.01s vs
        raku 0.09s）。セル省略より alloc 削減が先。
      - **型メタの instance 側は既に安定キー**: `instance_type_metadata` は `HashMap<u64(instance id), _>`
        で Arc-ptr ではない（flaky 無関係）。Q2 flaky の根は Array/Hash/Set/Bag/Mix の Arc-ptr keyed
        副テーブル → **要素セル Phase 2（トラック B）の領域**であり本 Phase 3 では扱えない。
      - **プロファイルで見つけた安い perf 候補**（Stage 3 の代替実体）:
        (a) `reset_atomic_var_key_decl`（self 3.7%）— VarDecl 毎に `format!` + shared_vars **write lock**、
        atomic 未使用でも。既存の `atomic_var_seen()` process-sticky bool でゲートするだけ。
        (b) method exit の `reconcile_attrs` が毎呼び出し `cell.to_map()`（attr map 全 clone）— 返り値の
        HashMap は今や proxy_fetch と（gated）commit でしか使われない。lazy 化／必要時のみ clone へ。
        (c) 残りは `Value::clone`/env insert の alloc 削減＝より大きな独立プロジェクト。

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
