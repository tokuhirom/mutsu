# `$x = @arr` reference sharing — first-class container (bug ②) の設計

> **Status:** Slice 2a IMPLEMENTED (2026-06-18, PR pending). `$scalar = @arr` /
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
2. **Slice 2b — 配列要素 / ハッシュ値代入**: `@aoa[i] = @row` / `%h<k> = @row` を cell 共有化
   (`assign_element_slot`/`hash_insert_through`)。AoA・HoA の参照共有。
3. **Slice 2c — bug ② 解消**: `@$n` deref bind が 2a の共有 cell を辿って caller @z に届くことを確認(2a/2b が
   前提)。pin に bug② の元ケースを追加。
4. **Slice 2d — sub 引数 / 戻り値の scalar-array 共有**(必要なら)。

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

## 7. 参照

- `docs/env-locals-coherence.md` — Stage 1 outer cell 化(for-rw site A 完了)。本書はその格納側の延長。
- `docs/container-identity.md` — 第一級コンテナ台帳(Phase 1 scalar cell / Phase 2 要素 cell)。
- `docs/vm-single-store.md` — 二重ストア統合(Slice F)。scalar-array 共有も env↔locals 同一 cell が前提。
- 実証 probe: §1 の表(2026-06-18)。
