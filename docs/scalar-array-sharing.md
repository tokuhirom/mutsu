# `$x = @arr` reference sharing — first-class container (bug ②) の設計

> **Status:** Slice 2a + chained `$r = $q` + **Slice 2b (`@aoa[i] = @row` / `%h<k> = @row`
> 要素・ハッシュ値共有, 2026-06-19, §5.2 参照)** IMPLEMENTED. `$scalar = @arr` /
> `$scalar = %hash` (VarDecl と reassign 両方) が source を共有 `ContainerRef` cell
> に昇格し、`.push`/`.unshift`/`.pop`/要素 write/whole-reassign が双方向に伝播する
> ようになった。pin=`t/scalar-array-share.t`(24)。**副次修正**: cell 経由 array push
> (`vm_data_ops.rs` の `ArrayPush` ContainerRef 分岐)を `Arc::make_mut` COW 化し、
> cell から取った copy(`my @copy = @z`)が source の in-place mutation で漏れる
> **pre-existing `:=` バグ**(`my @a:=@b; my @copy=@a; @a.push` が @copy に漏れていた)
> も同時に修正。**延期(Slice 2b/2c で対応)**: chained scalar share `my $r = $q`
> (RHS が scalar var で array を保持)= open-q #3、配列要素/ハッシュ値代入 `@aoa[i]=@row`
> = Slice 2b。
>
> 以下は当初の DESIGN メモ(2026-06-18)。env↔locals coherence Stage 1 の for-rw site A
> (array #3259 / hash #3260) 完了後に残った **bug ②**(`my @a := @$n` 非伝播)の root-cause が
> 「`$x = @arr` が *コピー* で参照共有しない」ことと確定したのを受け、その正攻法を設計する。
> `docs/container-identity.md`(第一級コンテナ)Phase 1/2 の延長であり、`docs/env-locals-coherence.md`
> Stage 1 の outer cell 化と同じ ContainerRef cell 機構を使う。

---

## 0. TL;DR

Raku では **Array/Hash を *scalar スロット* に代入すると同一オブジェクトを参照共有**する(Array は reference 型):

```raku
my @z = (1,2);
my $n = @z;       # $n は @z と同じ Array を指す(コピーでない)
@z.push(9);       # → $n も [1 2 9]
$n.push(8);       # → @z も [... 8]
```

`@`変数への代入(`my @copy = @z`)は **要素コピー**(独立)。区別は **代入先 sigil/スロット種別**:
- **`@`変数** ← copy(独立)。**現状正しい。**
- **`$`変数 / 配列要素(`@aoa[i]`) / ハッシュ値(`%h<k>`)** ← share(同一オブジェクト)。**現状コピー=バグ。**

`my $n := @z`(bind)は mutsu でも正しく共有する。壊れているのは **`=`(代入)で array が scalar スロットに入る**ときだけ。

---

## 1. 現状の正確な切り分け(2026-06-18 probe)

| 操作 | mutsu | raku | 判定 |
|---|---|---|---|
| `my @copy = @z; @z.push(9); say @copy` | `[1 2]` | `[1 2]` | ✅ copy 正しい |
| `my $n = @z; @z.push(9); say $n` | `[1 2]` | `[1 2 9]` | ❌ share すべき |
| `my $n = @z; $n[0]=8; say @z` | `[8 2]` | `[8 2]` | ✅ **要素書込は既に伝播** |
| `my $n = @z; $n.push(8); say @z` | `[1 2]` | `[1 2 8]` | ❌ **push が detach** |
| `@aoa[0]=@row; @row.push(9); say @aoa[0]` | `[1 2]` | `[1 2 9]` | ❌ share すべき |
| `%h<k>=@row; @row.push(9); say %h<k>` | `[1 2]` | `[1 2 9]` | ❌ share すべき |
| `my $n := @z; @z.push(9); say $n` | `[1 2 9]` | `[1 2 9]` | ✅ bind は共有 |

**核心**: `$n = @z` は **既に同じ outer `Arc` を共有している**(`$n[0]=8` が @z に伝播するのが証拠)。
だが **`.push`/構造変異は `Arc::make_mut` が strong_count>1 で deep copy → detach** するため伝播しない。
∴ 問題は「初期共有が無い」ことではなく「**構造変異が COW で割れる**」こと。要素 write は既に in-place
(`assign_element_slot`)で通っているのと同じ問題が、whole-container 構造変異(push/unshift/再代入)で残っている。

---

## 2. なぜ ContainerRef cell が正準解か

`ContainerRef(Arc<Mutex<Value>>)` は COW deep-copy 後も **`Arc<Mutex>` がクローン間で共有**される
(`docs/env-locals-coherence.md` §2)。array を **cell に包み、source(`@z`)と target(`$n`/要素/ハッシュ値)が
同じ cell を保持**すれば:
- `.push`(構造変異)も cell をロックして in-place mutate → 両者に可視(make_mut detach が起きない)。
- 要素 write は既に cell-aware(`assign_element_slot`/`hash_insert_through`)。
- 読みは `into_deref()` で decont 済(Stage 0 監査で確認済)。

これは for-rw site A 修正(#3259/#3260)が使った `write_back_container_source` と同じ cell 機構の **格納側**版。

---

## 3. 設計: scalar スロットへの array 代入を escape-aware に cell 共有

### 3.1 トリガー(いつ cell 化するか)

array/hash 値が **scalar スロットに格納される瞬間**に、source 変数と target を共有 cell にする:
1. **scalar var 代入** `$n = @z` — `SetLocal`/`AssignExpr` で RHS が Array/Hash かつ LHS が `$`。
2. **配列要素代入** `@aoa[i] = @row` — `assign_element_slot` の値が Array/Hash。
3. **ハッシュ値代入** `%h<k> = @row` — `hash_insert_through` の値が Array/Hash。
4. **(将来)** sub 引数で scalar param に array を渡す `f($x)` where `$x` binds array、戻り値等。

`@`変数代入(`@copy = @z`)・list 代入は **対象外**(コピー維持)。

### 3.2 source 昇格(escape-aware)

`$n = @z` のとき、RHS の Array が **裸の Arc**(cell でない)なら、source 変数 `@z` を **ContainerRef cell に
昇格**し、その cell を `$n` にも格納する。`@z` が既に cell ならそれを共有。**source が anonymous な一時値
(`$n = (1,2,3)` / `$n = @a.map(...)`)** のときは昇格不要 = 単に cell に包んで `$n` だけが持つ(誰とも共有しない
が、後で別の scalar に渡ると同 cell を共有)。

これは `box_captured_lexicals`(`vm_register_ops.rs`)が `@`/`%` を skip しているのを、**scalar スロットへ escape
する時点で昇格**する形に一般化する(closure capture と同じ "escape したら cell" 規律)。

### 3.3 裸ローカルは据え置き(perf 崖回避・#2746 の轍)

scalar スロットへ escape しない裸の `@arr` は **従来の Arc のまま**。値 op(算術 fold・iteration・native
raw-items)が ContainerRef を毎回 decont する "deref everywhere" を避ける。昇格は escape 点のみ。

---

## 4. blast radius(格納サイト) — 着手前 audit

`docs/env-locals-coherence.md` §5 Stage 1 audit の write サイトと重なる。array/hash 値を scalar スロットへ
格納する経路:
- **scalar var 代入**: `SetLocal`/`AssignExprLocal`/`AssignExpr`/`SetGlobal`(`vm.rs`/`vm_var_assign_ops.rs`)。
  RHS 型判定 + LHS sigil 判定が要る。
- **配列要素代入**: `assign_element_slot`(`value/mod.rs`)— 値が Array/Hash のとき cell 化。
- **ハッシュ値代入**: `hash_insert_through`(`value/mod.rs`)— 同上。
- **読み consumer**: `into_deref`(済) + raw-items を舐める native method/slice/`.raku`/`.kv`(leak 監査・Phase 2 既知課題)。

**hazard**: cell が漏れて値 op に流れる経路の網羅(open-q#1)。Stage 0 監査で read チョークポイントは
単一化済みだが、scalar に入った array は `$n.method` 経由で **scalar-value-method dispatch** に流れるため、
そこで decont されるか要確認(for-rw site A で `deref_container` を足したのと同型の補完が要るかも)。

---

## 5. 段階スライス(big-bang 不可)

1. **Slice 2a — scalar var 代入の source 昇格 [DONE 2026-06-18]**: `$n = @z` だけを cell 共有化。
   - **実装**: 新 opcode `MarkArrayShareContext` + flag `array_share_context`(`MarkArrayShareContext`
     が立てる)。compiler の `try_emit_array_share`(`compiler/stmt.rs`)が scalar LHS + `ArrayVar`/
     `HashVar` RHS を検出し、RHS 値を `WrapVarRef(source)` で包んで `MarkArrayShareContext` を emit。
     VM は `array_share_assign`(`vm_var_assign_ops.rs`)で source/target を共有 `ContainerRef` cell に
     昇格し、scalar slot に `__mutsu_array_share::<name>` マーカーを立てる。**SetLocal(VarDecl)/
     AssignExpr(reassign)/SetLocal slow path の3経路**全てに promotion と replace-on-reassign を入れた
     (`$n` は array を持つと `simple_locals=false` になり slow path を通るため、fast path だけでは不足
     だった=実装中の最大のハマりどころ)。`our`/global は SetGlobal でフラグを消費(copy・Slice 2d)。
   - **rebind vs mutate-through(設計通り実装)**: scalar への非-array/別コンテナ whole-reassign は
     `__mutsu_array_share` マーカーを drop して slot 置換、`@z = (...)`(array var)と `.push` は cell
     write-through。マーカーが `:=` write-through と `=` replace を per-variable で区別する。
   (旧設計メモ:) `$n = @z` だけを cell 共有化。
   `@z.push`/`$n.push` 双方向伝播を pin(`t/scalar-array-share.t`)。`@copy = @z` は不変(copy 維持)を guard。
   読み・既存 roast 完全一致が合格条件。**最小スライス。**
   - **[実装知見 2026-06-18] RHS source はコンパイル時に判別可能**: `my $n = @z` は
     `VarDecl { name:"n", expr: ArrayVar("z") }` に compile される(`--dump-ast` 確認)。∴ source 変数名 "z" は
     compile 時に既知＝`:=` の varref 追跡を新設せずとも、**scalar LHS + `ArrayVar`/`HashVar` RHS** を検出して
     source 昇格 op を emit できる。再代入 `$n = @z`(非 decl)も `AssignExpr` で同様に RHS が `ArrayVar` か判定可能。
   - **★rebind vs mutate-through の区別が肝**: cell 共有後、
     - `$n.push` / `@z.push` / `$n[0]=x` / `@z[0]=x` → cell を lock して in-place mutate(双方向伝播)。
     - `$n = 5`(scalar を非 array に再代入) → **$n の slot を `Value::Int(5)` で置換**(cell に触れない＝@z 不変)。
       `$n = @other`(別 array) → $n の slot を @other の cell に張り替え(@z の cell 不変)。
     - `@z = (9)`(array var 再代入) → **@z の cell を mutate-through**(中身入れ替え＝$n に可視)。
     つまり「scalar slot への非-array 代入 / 別コンテナ代入 = slot 置換」「array var 代入 = cell 中身置換」。
     `:=` bind(scalar container 自体の共有)とは異なり、**共有されるのは array オブジェクト(cell)だけで scalar
     container は別**。SetLocal が「既存 ContainerRef を維持して中身 write-through」する現挙動
     (`vm_var_assign_ops.rs:5543` 系)を、scalar への非-array 代入では「slot 置換」に分岐させる必要がある。
2. **Slice 2b — 配列要素 / ハッシュ値代入 [DONE 2026-06-19 / PR #3274]**: `@aoa[i] = @row` / `%h<k> = @row` を
   cell 共有化。AoA・HoA の参照共有。
   - **実装(2026-06-19)**: scratch から書くのではなく **`:=` element-bind 機構を再利用**した。コンパイラ
     (`compiler/expr_closure.rs` の `element_share_bind_value`)が `@var[単一添字] = @containervar` を検出し、
     RHS を `__mutsu_bind_index_value` で包んで **bind と同一バイトコード**にコンパイル(cell 設置 + source 昇格を
     bind 機構がそのまま行う)+ 新 opcode `MarkElementShare` を emit。VM は `exec_index_assign_expr_named_op` で
     `element_share_pending` を消費し、store 後に **element-keyed marker** `__mutsu_elem_share::<var>`(Hash of
     encoded-index、`mark_element_share`/`is_element_share`/`clear_element_share`、scalar 2a の per-variable marker
     を要素単位へ拡張)を立てる。**`=` と `:=` の唯一の差=reassign 時の replace vs write-through** を、3 つの
     write-through チョークポイント(array `arr[i]` cell 分岐=旧 BLOCKER の ~3836 行 / hash slow
     `hash_insert_through` / hash fast `try_fast_hash_element_assign`=ContainerRef で bail させ slow へ送る)で
     `elem_is_value_share` ゲートにより replace に分岐(precompute はコンテナ borrow 前、marker clear は store 後)。
     ∴ §10/旧 BLOCKER が指摘した「per-element の share-vs-bind 区別」を element-keyed marker で実現。
   - **★ハマり所**: ① **スライス代入の誤検出** — `@b[*-3,*-2] = @x` は distributing slice assignment であって参照
     共有でない。index が `ArrayLiteral`/Range/Whatever のとき share させない(単一スカラー添字=`Literal(Int/Str/Num/Bool)`
     /`Var`/`Binary`/`Unary` のみ whitelist)。② **self-reference** `%h<k> = %h`(infinite HoH)は既存の
     `is_self_hash_ref`→`self_hash_ref_marker` 経路を維持すべきで、source(=target)を ContainerRef 昇格すると
     `isa-ok %h, Hash` と循環読みが壊れる(hash_ref.t whitelist 回帰)→ `source == target_name` で share 除外。
   - pin=`t/element-array-share.t`(28 ケース)。make test PASS、whitelist S02/S03 全 251 PASS。
   - **[chained `$r = $q` DONE 2026-06-18, open-q#3 / #3267]**: scalar source の chained 共有も実装。Slice 2a の
     `WrapVarRef`+`MarkArrayShareContext` を **`MarkArrayShareSource(name_idx)` + `array_share_source` field**
     に統一リファクタ(WrapVarRef は plain `$x=$y` を bind 化する副作用があったため不可)。compiler
     `try_emit_array_share` が `Expr::Var(n)` RHS も対象に追加。runtime は **source 値が deref して
     Array/Hash のときだけ共有**(`with_deref` ゲート)→ plain `$x=$y`(scalar source)・`:=` bound Int
     scalar の chained `=` は copy 維持。pin に 9 ケース追加(計 33)。make test/roast green。
3. **Slice 2c — bug ② の deref bind [array 形 DONE 2026-06-18(6th) / PR #3268]**: `my @a := @$n`(value-alias
   scalar `$n` の deref bind)が 2a の共有 cell を辿って caller @z に届くようにした。
   - **実装**: `@$n` は parser が `ArrayVar("n")` に desugar するため、bind の source は `"@n"`(WrapVarRef)。
     実際の cell は scalar `$n`(env key `"n"`)に在る。SetLocal bind 経路(`vm_var_assign_ops.rs` の
     `if let Some(source_name) = bind_source` ブロック)に **scalar-source フォールバック**を追加:
     `resolved_source`("@n")が**実行時に**コンテナ値でない(env.get("@n") が Array/Hash/ContainerRef でない)
     とき、`@`/`%` を剥がした bare 名("n")の scalar が `ContainerRef` を持てばその cell を reuse。
     `effective_source` で promotion/env/frame 書き戻しを bare scalar 名に向ける。pin=`t/deref-bind-value-alias.t`。
   - **★ハマり所**: `source_in_same_scope`(`code.locals` に "@n" が在るか)は **function 全体の locals** を見る
     ため、*別ブロック*の `my @n` でも真になり fallback を誤抑制した。∴ gate は locals 在否でなく
     **resolved_source の実行時値がコンテナか**で判定する(`source_resolves_to_container`)。
   - **hash 形 `my %h := %$m` [DONE 2026-06-19(24th) / PR pending]**: parser が `%$m` を `$m.hash`
     (MethodCall)に desugar するため `ArrayVar` のような simple-var bind 経路に乗らなかった。**修正**=
     bind 文脈(`handle_binding`, `my_decl_assign.rs`)で、target が hash かつ RHS が plain-scalar 形の
     `$m.hash`(`MethodCall{target:Var(m), name:"hash", args:[]}`)のとき `HashVar("m")` に rewrite。
     これで array 形と同一の deref-bind 経路(scalar-source フォールバックは `['@','%']` 両対応済)に乗る。
     `%$/`・`%$_` 等の CaptureVar 形は `.hash` coercion のまま(value-alias でないため)。
   - **★同時に発見・修正した対称バグ(array にも存在)**: deref bind の**要素代入**(`@a[0]=` / `%h<k>=`)が
     共有 cell を write-through せず detach していた(`.push` は別 opcode で動いていた)。真因=SetLocal
     bind 経路が `__mutsu_sigilless_alias::%h = "%m"`(sigil 付き source 名)を立てるが、index-assign は
     この alias を辿って存在しない `%m` を autovivify → fresh detached container。**修正**=`scalar_source`
     が Some(deref bind via 昇格 scalar)のとき alias を **effective_source**(bare scalar "m")に向け直す。
     これで index-assign が共有 cell に到達。array `@a[0]=` も同時に修正された。
   - **残: sub-param 経由(headline bug② `sub f($n){ my @a := @$n; @a.push }`)**: param `$n` は `.push`
     直接(case A)は伝播するが、その上に deref bind を重ねると伝播しない=param が `"n"` env key に
     `ContainerRef` を持たない(`arg_sources` return-writeback 共有=別機構)。Slice 2d の
     param-by-reference 化が前提。
4. **Slice 2d — sub 引数 / 戻り値の scalar-array 共有**: probe では `g($x){$x.push}` / `return @arr` /
   `our $g = @z` の単純 push 伝播は **既に動作**。残は上記 2c-② の多段(param+deref bind)=array arg を
   scalar param へ渡す際に共有 cell へ昇格する(call 境界での `array_share_assign` 相当)。`$m = $n` は #3267 で完了。

各 Slice は **make test(t/ 回帰は whitelist 非収録=必須)+ release roast の main-vs-branch 比較 + int.t/
method-call wall-clock**(#2746 教訓: perf 回帰は roast timeout でしか出ない)で固める。

---

## 6. open questions

1. **scalar-value-method dispatch の decont**: `$n.push`/`$n.elems` 等が cell を decont してから native array
   method に渡るか。漏れると "deref everywhere" の入口。
2. **`@copy = @z` との確実な分離**: 代入先 sigil/スロット判定が list-assign / flatten / slice 代入で誤らないか。
3. **anonymous 一時値の扱い**: `$n = (1,2,3)` は誰とも共有しないが cell に包むか(包めば後続の `$m = $n` 共有が
   自然)。包まないなら後で `$m = $n` の共有が別途必要。
4. **cross-thread**: 共有 cell(`Arc<Mutex>`)と `clone_for_thread`/`shared_vars` の整合(Track C)。
5. **COW コスト**: cell 化した array の値 op が毎回 lock するコスト。escape-aware で裸ローカルは除外するが、
   scalar に入った array の hot path(`$n[i]` ループ)の wall-clock を計測。

---

## 8. 実装試行の結果(2026-06-18) — Slice 2a は動くが Arc-identity ripple で revert

> **★SUPERSEDED (2026-06-18, PR #3264 MERGED)**: この §8 は `MarkValueAliasSource` を使った
> 並行試行が Pair-value 回帰で revert された記録。その後の **PR #3264 で Slice 2a は実際に landing
> 済み**(別 opcode `MarkArrayShareContext`)。§8 が「不可」とした Arc-identity ripple は、**blanket な
> Sub-slice 1a(全 write-chokepoint の behavior-invariant decont)を先行させずに**、cell が壊した
> identity 機構を**個別に cell-aware 化**することで解消した: ①`overwrite_array/hash_bindings_by_identity`
> (needle Arc を inner に持つ ContainerRef cell の中身を write-through)②`builtin_index_assign_method_lvalue`
> の `current` を deref ③regex `interpolate_regex_scalars` の var-read を `into_deref` ④`UndefineAggregate`
> を cell-aware(`clear_aggregate_cell`)。**∴ "leak を見つけ次第 deref で塞ぐ" 漸進アプローチで十分**で、
> blanket Stage 0 1a 先行は不要だった。§5 Slice 2a の DONE 注記が最新の実装真実。以下の §8 本文は歴史的記録。

Slice 2a(scalar `$n = @z` の cell 共有 = "value-alias")を実装し検証した。**設計は正しく、§5 の
スライス順序を裏付けた。** 結論: **value-alias は Sub-slice 1a(write-chokepoint decont)を先に landing
しないと既存の Arc-identity 機構を壊す。** revert 済み。

### 動いた実装(次回そのまま再利用可)

- 新 opcode `MarkValueAliasSource(name_idx)`(`opcode.rs`)を compiler が `my $n = @z`/`$n = @z`
  (scalar = whole `@`/`%` var)で SetLocal 直前に emit(`compiler/stmt.rs` VarDecl else 分岐 ＋
  Stmt::Assign plain-assign 分岐、**local scalar 限定**＝SetLocal が flag を消費するため・SetGlobal だと
  flag が dangling)。**gotcha**: `Expr::ArrayVar(s)` の `s` は **sigil 無し**("z")。src は
  `format!("@{}", s)` で sigil 付与しないと local slot "@z" にマッチしない(これで最初 share せず空振り)。
- VM: `value_alias_source: Option<String>` field(`runtime/mod.rs`)。SetLocal 冒頭で take し
  `setup_scalar_value_alias` を呼ぶ: src を共有 `ContainerRef` cell 化(bind path
  `vm_var_assign_ops.rs:6192` と同型 — slot+env+saved frames へ書き戻し)、$n に同 cell、
  `__mutsu_value_alias::$n` marker を env へ。snapshot(GetArrayVar の deref 値)は cell 不在時の初期値。
- detach(§5 Slice 2a の「scalar への非-array 代入 = slot 置換」): value-alias marker 持ち ContainerRef
  scalar への再代入は **cell write-through せず slot 置換**(`$n = 5` が @z を壊さない)。**gotcha**:
  value-alias scalar は `simple_locals=false` になり read が env 経由になるため、detach は
  `flush_local_to_env`(simple 限定で no-op)ではなく `env_mut().insert(name, v)` で env を直接上書き
  しないと stale な ContainerRef が残り `say $n` が旧 array を読む。

### 検証結果

- **core 全 PASS**(`t/scalar-array-ref-sharing.t` 18 ケース): `my $n=@z; $n.push(99)` → @z 3 件、
  element/push 両方向伝播、detach(push 後含む)、hash 版、itemized copy(`my @c=$n` → `[[1 2] 3]` raku 一致)。
- `cargo test` 466/0、array/binding/list/signature roast spread clean。

### revert 理由 — open-q#1/#2 が現実の壁

`my $a = @src` が $a を **ContainerRef にすると、source を Arc identity で追跡する既存機構が壊れる**:
**`t/pair-value-element-writethrough.t`(#2943)が回帰** — `$p = (k => $a); $p.value[3] = "x"` の
write-through は @src/$a の **plain Array Arc を identity で照合**するが、cell 化で `Value::ContainerRef`
になると照合に失敗し空書き込みになる。これは §5 が「2a の前に Stage 0 チョークポイント先行」と書いた
依存そのもの＝**全 mutation/write-through サイトを ContainerRef-aware(decont)化する Sub-slice 1a が前提**。
open-q#1(scalar-value-method dispatch の decont)も同根: cell が漏れない監査が要る。

**∴ 次回の正しい順序: (1) Sub-slice 1a を behavior-invariant に landing(全 write-chokepoint で
ContainerRef を decont、roast byte 一致が合格) → (2) その上に value-alias(本実装を再適用)。**
value-alias 単独 PR は Arc-identity 機構を壊すので不可。

---

## 9. 参照

- `docs/env-locals-coherence.md` — Stage 1 outer cell 化(for-rw site A 完了)。本書はその格納側の延長。
- `docs/container-identity.md` — 第一級コンテナ台帳(Phase 1 scalar cell / Phase 2 要素 cell)。
- `docs/vm-single-store.md` — 二重ストア統合(Slice F)。scalar-array 共有も env↔locals 同一 cell が前提。
- 実証 probe: §1 の表(2026-06-18)。実装試行: §8(2026-06-18)。

---

## 10. Slice 2b 着手 handoff(2026-06-18 偵察完了・次セッション即着手用)

`@aoa[i] = @row` / `%h<k> = @row`(配列要素 / ハッシュ値への whole `@`/`%` 代入)を参照共有化する。
**Slice 2a の cell 機構 + `MarkArrayShareSource`/`array_share_source` をそのまま流用**できる。偵察で
判明した実装地図・raku 意味論・leak 評価・最大の設計課題を以下に固める。

### 10.1 現状(2026-06-18 probe・全て mutsu=copy / raku=share でバグ)

| 操作 | mutsu | raku |
|---|---|---|
| `@aoa[0]=@row; @row.push(9); say @aoa[0]` | `[1 2]` | `[1 2 9]` |
| `@aoa[0]=@row; @aoa[0].push(8); say @row` | 非伝播 | `[1 2 ... 8]` |
| `%h<k>=@v; @v.push(3); say %h<k>` | `[1 2]` | `[1 2 3]` |

### 10.2 ★最重要: cell-install 機構は `:=` 版が**既に存在**=trigger を `=` に広げるだけ

`@aoa[0] := @row` / `%h<k> := @v`(`:=` 要素/値 bind)は mutsu で**既に完全双方向共有**する
(probe 確認: `@aoa[0].push`→@row, `@v.push`→%h<k> 双方向 OK)。つまり **要素 cell の格納・読み機構は
完成済み**。Slice 2b は「`:=` でやっていることを `=`(array_share_source 経由)でもやる」。
- **named handler**(`@aoa[0]=@row`: target が `ArrayVar`/`HashVar`)= `IndexAssignExprNamed` →
  `exec_index_assign_expr_named_op_inner`(`vm_var_assign_ops.rs:2772`)。**ここに `bind_mode` ゲートの
  `bind_cell` 機構が既にある**(cell を要素に install + source var へ cell 書き戻し)。これを
  `array_share_source` でも発火させる。
- **generic/computed handler**(`($ref)[i]=@row`, `f()<k>=@row`)= `vm_var_assign_ops.rs:4787-4897` に
  同型の `bind_cell` 機構。同様に `=` 対応。
- **compile hook**: `compile_expr_index_assign`(`compiler/expr_closure.rs:286`)。`@aoa[0]=@row` は
  `IndexAssign{target:ArrayVar("aoa"), index, value:ArrayVar("row")}`(`--dump-ast` 確認)。value を
  `compile_bind_index_value`(:268)で compile した直後に、**value が `ArrayVar`/`HashVar` のとき
  `MarkArrayShareSource(value_source_name)` を emit**(2a と同じ opcode・field)。target も
  `ArrayVar`/`HashVar`(要素 LHS)のときのみ。

### 10.3 raku 意味論(probe 確定・要素も 2a の `$n` と同一の rebind/replace)

- `@aoa[0]=@row` → 共有(cell)。`@row.push`/`@aoa[0].push` 双方向。
- `@aoa[0]=5`(要素を非 array 再代入) → **要素 slot を 5 に置換・@row は不変**(`[1 2]`)。
- `@aoa[0]=@other`(別 array) → @other の cell へ張り替え・@row はもう追わない・`@other.push` のみ伝播。
- hash 値も同様(`%h<k>=@v` 共有 / `%h<k>=5` 置換 / `%h<k>=@other` 張り替え)。

### 10.4 ★最大の設計課題: 要素 cell には name marker が無い(`=`-share と `:=`-bind の区別)

2a は `__mutsu_array_share::<name>` marker で「`=`-share の whole-reassign は cell write-through せず
slot 置換」を実現したが、**要素には変数名が無いので同じ marker を貼れない**。**★具体的 BLOCKER 箇所
(#3268 でユーザーが特定)**: 単一要素 array store `exec_index_assign_expr_named_op_inner`
(`vm_var_assign_ops.rs` ~3836 行)の `else if let Value::ContainerRef(cell) = &arr[i] { *cell.lock()=val }`
が **ContainerRef 要素を再代入時に無条件 write-through**。`=`-share では replace したいが bind-cell は
write-through。`assign_element_slot`(`value/mod.rs:3009`)も同型。
- **★probe で確定(2026-06-18)**: raku では `@aoa[0]:=@row; @aoa[0]=5` **も** `@aoa[0]=(7,7)` **も**
  "Cannot assign to an immutable value" **エラー**(非-array でも別 array でも write-through ではない)。
  ∴ **「要素 cell へ値を write-through する」正しい raku 経路は存在しない**(現 mutsu の write-through は
  元々非準拠)。
- **方向=案A(新値型で分岐)**: 要素 write 時、新値が array(かつ array_share_source 有)→ re-share
  (別 cell 張り替え) / 新値が非-array → 要素 slot replace(cell 捨て)。name 不要。**ただし前提**:
  既存 `t/element-bind-cell.t`(`:=` bound 要素)が ~3836 の write-through に依存しているケースがある
  (ユーザー指摘「per-element の share-vs-bind 区別が要る」)→ **着手時に element-bind-cell.t が要求する
  write-through を洗い出し、`=`-share だけ replace に分岐**する(bound-index マーカー有=write-through
  維持 / array_share_source 起源の cell=replace)。「値側で ContainerRef を push して通常 store に任せる」
  ショートカットはこの write-through で破綻するので不可。
- **案B(代替)**: cell に "value-share" フラグ(新 field/wrapper)を持たせ per-cell 区別。案A が
  element-bind-cell.t と両立しない場合のフォールバック。

### 10.5 leak 評価: **2b は 2a より低リスク**

`@aoa[0]:=@row` 経由で要素 cell の read 面を warmup probe(deref read/`.elems`/parent stringify/
nested index `@aoa[0][1]`/`for @aoa`/`@aoa.map`/itemize/`.WHAT`/`.raku`)→**全て raku 一致**
(`element-bind-cell.t` が既に網羅)。唯一 trivial 差=`@aoa.raku` の単要素 trailing-comma
(`[[1,2,9]]` vs `[[1,2,9],]`・pre-existing・cell 無関係)。∴ 要素 cell の read leak 面は枯れている。
2a で踏んだ regex/undefine 級の leak は要素では出にくい(要素は into_deref/resolve_*_entry に集約)。
それでも着手時に同じ warmup probe を `=`-share で再走させ確認すること。

### 10.6 pin テスト草案(`t/scalar-array-share.t` に追記 or 新 `t/scalar-array-share-2b.t`)

```raku
# AoA element share
my @row = (1,2); my @aoa; @aoa[0] = @row;
@row.push(9);      is-deeply @aoa[0].Array, [1,2,9];     # @row.push -> element
@aoa[0].push(8);   is-deeply @row.Array, [1,2,9,8];      # element.push -> @row
# element reassign to non-array replaces (raku value semantics)
my @r=(1,2); my @a; @a[0]=@r; @a[0]=5; is-deeply @r.Array,[1,2]; is @a[0],5;
# element reassign to another array re-points
my @r2=(1,2); my @o=(7,8); my @b; @b[0]=@r2; @b[0]=@o; @o.push(99);
is-deeply @b[0].Array,[7,8,99]; @r2.push(88); is-deeply @b[0].Array,[7,8,99];
# HoA value share
my @v=(1,2); my %h; %h<k>=@v; @v.push(3); is-deeply %h<k>.Array,[1,2,3];
%h<k>.push(4); is-deeply @v.Array,[1,2,3,4];
# plain element copy unaffected: @x[0]=@y where used as copy? (raku still shares — verify)
```

### 10.7 検証順序

(1) compile hook + named handler `=`-share install を実装し AoA/HoA 双方向共有を pin で固める →
(2) 要素 rebind/replace(§10.4 案A)→ (3) `make test`(t/ 必須)+ `MarkArrayShareSource` の単要素
re-share/replace 境界 → (4) generic/computed handler 対応 → (5) `make roast` で leak/回帰確認。
**Slice 2a/chained と同様、source promotion で要素値が ContainerRef 化した read 経路を warmup probe で
先に洗う**(§10.5 で枯れ確認済みだが `=`-share 実装後に再確認)。
