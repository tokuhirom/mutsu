# GC Level 1 詳細設計メモ

Status: Draft  
Related: [ADR-0001](adr/0001-gc-strategy-and-phasing.md), [PLAN.md](../PLAN.md), [ANALYSIS.md](../ANALYSIS.md)

この文書は ADR-0001 の「レベル1 = cycle collector on Arc」を、実装可能な設計へ落とすための詳細メモ。
ADR 自体は変えず、未決だった **起動方式 / root 境界 / Track B との接合面** を具体化する。

## 1. 先に固定する判断

### 1.1 初期実装は **同期・協調的（cooperative）cycle collection**

最初の collector は **background thread を使わない**。

- collect は VM の **明示 safepoint** でのみ走らせる
- mutate 側は「candidate 登録」まで
- trial-deletion / reclaim は **現在の実行スレッド**が担当する

理由:

- 現在の mutsu は `stack` / `locals` / `upvalues` / `call_frames.saved_*` を `Interpreter` が直接持つ
- background collector を入れると、root snapshot の一貫性・lock 順序・`clone_for_thread` との整合が一気に難化する
- ADR の主眼は「moving を避け、実装爆発を避ける」ことなので、最初から concurrent collector を入れるのは逆行

結論:

- **Level 1a = synchronous collector** を先に出す
- background 化は Level 1b の後続最適化に分離する

### 1.2 safepoint は「再入境界」に限定する

collect を許可するのは、`arc_contents_mut` 的な長い借用や lock を持っていない地点だけにする。

初期 safepoint 候補:

- bytecode dispatch ループの backward edge
- call / return / invoke 境界
- `await` / `start` / `hyper` / `race` の join / merge 境界
- lazy force / gather drain / map-grep nested-run の出入口

逆に、以下では collect しない:

- container 内部を借用中の mutation helper 内
- `ContainerRef` / array/hash slot の write-through 中
- `shared_vars` / registry / attrs などの lock 保持中

これは Track B の「再入安全な chokepoint 設計」と同一問題なので、一体で設計する。

## 2. root モデル

### 2.1 Rust スタック走査はしない

Level 1 は tracing GC ではなく cycle collector なので、**Rust スタック上の生 `Value` を走査対象にしない**。

代わりに:

- **GC 管理対象オブジェクト同士の辺**だけを collector が追う
- root は **`Interpreter` が所有する永続レジスタ / 保存フレーム / 共有表** から列挙する

つまり「collector が知らない Rust ローカルに一瞬載る値」は問題ではない。non-moving なのでアドレス更新も不要で、
refcount が通常の生存を保証し、cycle collector は「孤立 cycle を切る」役だけに徹する。

### 2.2 初期 root 集合

1 回の collect で辿る root は次に固定する。

- `Interpreter.stack`
- `Interpreter.locals`
- `Interpreter.upvalues`
- `Interpreter.env`
- `Interpreter.call_frames[*].saved_env`
- `Interpreter.call_frames[*].saved_locals`
- `Interpreter.call_frames[*].saved_upvalues`
- `Interpreter.block_stack`
- `Interpreter.topic_save_stack`
- `Interpreter.last_topic_value`
- `Interpreter.element_source` / `container_ref_var` / bind 系一時状態のうち `Value` を持つもの
- `shared_vars`
- `closure_env_overrides`
- pending destroy / pending writeback / lazy-force / gather-resume など、`Value` を保持する待機キュー
- process-global async/supply registries のうち `Value` / `SharedPromise` / `SharedChannel` を保持するもの

除外:

- `CompiledCode` / opcode / function metadata のみで `Value` を持たないもの
- `readonly_vars` など文字列集合
- Rust の一時ローカル

### 2.3 A' の完了条件を弱く定義する

ADR の A' は root 集約を楽にするための地ならしだが、Level 1a の着手条件を厳しくしすぎない。

着手条件:

- `stack` / `locals` / `upvalues` / `saved_*` から root 列挙できる
- `env` / `shared_vars` / pending queues に残る `Value` を明示列挙できる

未完でもよい:

- env 経路の完全撤去
- すべての lexical を upvalue index に統一

理由:

- Level 1 は moving しないので「完全な root relocation 情報」は不要
- ただし root 列挙コードが `Interpreter` 各所へ散るのは避けるべきなので、collector 着手前に
  `visit_roots(&mut impl RootVisitor)` を 1 箇所へ集約する

## 3. 管理対象オブジェクト

### 3.1 `Value` 全体は GC 化しない

`Value` は今の plain enum のまま残す。全部を `Gc<Value>` にする案は **レベル2** であり、Level 1 の範囲外。

Level 1 で GC 管理対象にするのは「循環を作り得る heap node」だけ。

初期対象:

- `Array`
- `Hash`
- `Set`
- `Bag`
- `Mix`
- `Sub`
- `Instance`
- `ContainerRef`
- `Pair` / `ValuePair`

初回スコープに含める対象:

- `Promise`
- `Channel`
- `LazyList`

理由:

- ADR の問題提起に Promise/Supply の相互参照が入っている
- ここを対象外にすると「GC を入れたのに async cycle は漏れる」状態になり、設計目的とズレる

ただし **「初回スコープに含める」ことと「first wave で同時実装する」ことは分ける**。

- `Promise` / `Channel` は async cycle の本丸であり、内部状態も比較的閉じている
- `LazyList` は必要だが、`env` / `cache` / coroutine state / lazy pipe / closure sequence を持ち trace 面積が広い

したがって実装波は次で固定する:

- **first wave**: `Array` / `Hash` / `ContainerRef` / `Promise` / `Channel`
- **second wave**: `Sub` / `Instance`
- **third wave**: `LazyList`（および必要なら supply graph の残部）

要するに、**`LazyList` は Level 1 の対象から外さないが、最初の移行波からは外す**。

逆に対象外:

- `Int` / `Num` / `Rat` / `Complex` / `Bool`
- `Str`
- `Nil`
- `Range` 系
- metadata-only な type objects

### 3.2 表現方針

Level 1 の基本形は次。

- `Value::Array(Arc<ArrayData>, ...)` → `Value::Array(Gc<ArrayData>, ...)`
- `Value::Hash(Arc<HashData>)` → `Value::Hash(Gc<HashData>)`
- `Value::Sub(Arc<SubData>)` → `Value::Sub(Gc<SubData>)`
- `Value::Instance { attributes: Arc<InstanceAttrs>, ... }` は
  `Instance` 本体と `attributes` cell のどちらを GC 管理対象にするかを分けて考える

重要:

- **外側の `Value` は move しない**
- **内側の node も move しない**
- 既存 API の大半は `Arc<T>` を `Gc<T>` に置き換えるだけで済ませる

### 3.3 `ContainerRef` は `Arc<Mutex<Value>>` のままではなく GC cell に寄せる

Track B と統合する以上、`ContainerRef` だけ `Arc<Mutex<Value>>` のまま残すのは中途半端。

初期案:

- `ContainerRef(Arc<Mutex<Value>>)` を `ContainerRef(Gc<CellValue>)` に置換
- `CellValue` は typed-scalar 制約を保持できるようにする
- lock が必要な cross-thread ケースは `CellValue` の内部に閉じ込める

これで:

- scalar bind cell
- array/hash element leaf cell
- captured outer lexical cell

が同一の GC 管理モデルに乗る。

### 3.4 Supply graph は「Supplier value」だけでなく process-global registry 群まで含める

Supply 系は `Value::Instance("Supplier")` の属性だけでは閉じていない。
実際の live graph は process-global registry に分散している。

初回スコープに含めるべき supply-side registry:

- `supplier_state_map`
  - `emitted: Vec<Value>`
  - `quit_reason: Option<Value>`
  - `pending_promises: Vec<SharedPromise>`
- `supplier_subscriptions_map`
  - tap callback `Value`
  - done/quit/whenever-quit/close callback `Value`
  - `channel_sink: SharedChannel`
  - unique/classify/produce/start/batch/zip state が保持する `Value`
- `promise_combinator_map`
  - combinator promise → source promises の辺
- `supply_taps_map`
  - replay/deferred tap 用 `Value`

扱い:

- これらは **GC 管理対象 node ではなく root container** とみなす
- registry 自体を `Gc<T>` 化する必要はない
- collect 時に visitor が中の `Value` / `SharedPromise` / `SharedChannel` を列挙すれば足りる

理由:

- Supply cycle の多くは callback closure や pending promise が global registry に残る形で成立する
- ここを root visitor に含めないと、「Promise/Channel を対象に入れたのに Supply 経由の cycle は残る」状態になる

逆に、first wave で即座に対象化しないもの:

- socket/listener/udp など OS resource 主体の registry
- `mpsc::Receiver<SupplyEvent>` 自体
- collected text buffer など `Value` を持たない補助 state

ただし、それらが `Value` callback や `SharedPromise` を直接保持する場合は root visitor に追加する。

## 4. Track B との接合

### 4.1 「全要素 cell 化」はしない

Track B でも、配列/ハッシュの全要素を常時 cell にするのは避ける。

方針:

- 通常の element は plain `Value`
- alias / bind / captured-write / cross-thread 共有が必要になった leaf だけ cell 化
- array/hash の structural mutation は **単一 chokepoint** を通す

これは既存の `assign_element_slot` / `hash_insert_through` / `array_slot_ref` / `hash_slot_ref` の方向性を維持する。

### 4.2 mutation chokepoint を collector の write barrier 代わりに使う

Level 1a は世代別 GC ではないので本格 write barrier は要らない。
ただし cycle candidate 登録は mutation chokepoint から行う。

候補 chokepoint:

- array element write
- hash insert/replace
- instance attr write
- container cell store
- closure env capture / rebinding
- promise/channel wiring
- lazy-pipe / coroutine / closure-sequence state update

規則:

- scalar-only write は candidate 登録しない
- GC 管理対象 node を child に持つ可能性があるときだけ candidate buffer へ push

## 5. collector の中身

### 5.1 初期アルゴリズム

ADR に沿って Bacon-Rajan 系の trial-deletion を採るが、最初は最小構成にする。

各 GC node は持つ:

- strong refcount 相当
- color / state
- buffered フラグ
- trace children 用 vtable 相当

各 node は `trace(&mut Visitor)` を実装し、**GC 管理対象 child だけ**を列挙する。

### 5.2 collect の流れ

1. mutate chokepoint が candidate buffer に node を積む
2. safepoint で budget 超過なら collect を開始
3. root から reachable な node を mark するのではなく、
   candidate subgraph に trial-deletion をかける
4. 外部参照が残る node は黒へ戻す
5. cycle のみ reclaim する

最初は conservative でよい:

- candidate 重複可
- buffer は `Vec<GcId>` でよい
- pause 時間短縮より correctness 優先

## 6. `clone_for_thread` と cross-thread

### 6.1 初期実装は「shared_vars に live shared node がある」前提で設計する

今の `clone_for_thread` は snapshot と shared の混在モデルで、ここがもっとも危険。

Level 1a の rule:

- GC 管理対象 node を cross-thread 共有する場合、node 自体は共有可
- ただし collect は **その node を参照し得る全 interpreter が safepoint に入るまで開始しない**

初期実装では単純化のため:

- global collector state に「registered interpreters」を持つ
- collect 開始側が全 interpreter に stop request を立てる
- 各 interpreter は safepoint で協調停止し、自分の roots を列挙する

つまり **stop-the-world だが cooperative**。

これで background collector なしでも cross-thread cycle を正しく扱える。

### 6.2 async object と supply registry を初回スコープに含めるため global collect は必須

`Promise` / `Channel` / supply graph を対象に入れるなら、thread local collector では足りない。
最初から **process-global collector + per-interpreter root enumeration** に寄せる。

ここでいう supply graph には、`Supplier` value だけでなく §3.4 の process-global registry 群を含む。

## 7. 先に作る補助 API

実装前に次の API を用意すると設計が散らばらない。

- `Interpreter::visit_roots(visitor)`
- `Value::visit_gc_children(visitor)`
- `ArrayData::visit_gc_children(visitor)`
- `HashData::visit_gc_children(visitor)`
- `SubData::visit_gc_children(visitor)`
- `InstanceAttrs::visit_gc_children(visitor)`

禁止事項:

- collector から `self.interpreter.*` を直接いじる fallback
- root 列挙ロジックを call site ごとに重複実装

## 8. 計測

ADR の `MUTSU_VM_STATS` 案はそのまま採る。最初に欲しいカウンタは次。

- `gc_candidate_pushes`
- `gc_candidate_dedup_hits`
- `gc_collections`
- `gc_reclaimed_nodes`
- `gc_reclaimed_cycles`
- `gc_pause_ns_total`
- `gc_pause_ns_max`
- `gc_root_nodes_scanned`

成功条件:

- `fib` / int-heavy benchmark で `gc_candidate_pushes = 0`
- array/hash heavy benchmark でも pause が観測可能で、ハングしない
- async cycle regression を再現する pin test で reclaimed が増える

## 9. デバッグ運用

### 9.1 初期導入は `compiled in, default off`

Level 1a の最初の段階では、GC コードはバイナリに入れるが **デフォルトでは cycle collection を動かさない**。

方針:

- 通常実行では cyclic reference の回収はしない
- root visitor / child visitor / candidate push / verify hook までは実装する
- collect は opt-in のみ

理由:

- まず「GC コードが存在しても通常実行を壊さない」を確認したい
- candidate 登録や root 列挙のバグと、実際の reclaim のバグを分離したい

推奨 env:

- `MUTSU_GC=off|on`
- 初期値は `off`
- `on` でも最初は `collect_now()` / stress モードでしか collect しない

初期実装の厳密な意味:

- `MUTSU_GC=off`
  - candidate 登録は行ってよい
  - reclaim は一切行わない
  - manual hook (`gc_debug_collect_now`) は no-op ではなく「collect 試行ログだけ出して skip」でもよい
- `MUTSU_GC=on`
  - collect を許可する
  - ただし trigger が無ければ走らない

### 9.1a env var の基本ルール

GC 用 env var はすべて **未設定が既定**。

- boolean は `0|1` を正とする
- mode は小文字固定 (`off|on`, `summary|detail|trace`)
- 数値 parse 失敗時は warning を 1 回だけ `stderr` に出し、既定値へフォールバック
- 未知の enum 値も同様に warning + 既定値

優先順位:

1. `MUTSU_GC=off` なら他の `MUTSU_GC_*` trigger は **全て無効**
2. `MUTSU_GC=on` の上で、manual / deterministic / random を解釈する
3. deterministic trigger と random trigger は **併用可**
4. 同一 safepoint で複数条件が成立したら、reason は次の優先順位で 1 つに正規化する
   `manual > every_safepoint > every_candidate > explicit_safepoint > random > threshold`

この「reason の正規化」はログと stats 用であり、collect 実行回数は 1 回だけ。

### 9.2 stress は random より deterministic を主にする

random collect は有用だが、最初の主戦場にはしない。
**再現性のある deterministic stress を先に作る**。

最初に欲しいモード:

- `MUTSU_GC_EVERY_CANDIDATE=N`
  - N 回の candidate push ごとに collect
- `MUTSU_GC_EVERY_SAFEPOINT=1`
  - safepoint ごとに collect
- `MUTSU_GC_AT=call,return,await,...`
  - 特定 safepoint 種別でのみ collect
- `MUTSU_GC_COLLECT_NOW=1`
  - 起動直後 or 特定タイミングで 1 回だけ collect

最小実装セットとしては次を first cut にする:

- `MUTSU_GC`
- `MUTSU_GC_EVERY_SAFEPOINT`
- `MUTSU_GC_EVERY_CANDIDATE`
- `MUTSU_GC_LOG`
- `MUTSU_GC_VERIFY`

first cut では見送ってよいもの:

- `MUTSU_GC_AT`
- `MUTSU_GC_COLLECT_NOW`
- `MUTSU_GC_RANDOM_SEED`
- `MUTSU_GC_RANDOM_RATE`

理由:

- 最初に欲しいのは deterministic stress と logging/verify
- safepoint 種別指定や random は first cut が安定してからでよい

random はその後:

- `MUTSU_GC_RANDOM_SEED=<u64>`
- `MUTSU_GC_RANDOM_RATE=<0.0..1.0>`

random の解釈:

- `MUTSU_GC_RANDOM_RATE=0` は random 無効
- `MUTSU_GC_RANDOM_RATE>0` かつ seed 未指定なら、固定 seed を自動採番せず
  **起動時刻由来の seed を採用して必ずログへ出す**
- 再現試験では必ず seed 固定を使う

`MUTSU_GC_EVERY_CANDIDATE` の解釈:

- `0` または未設定 = 無効
- `1` = every candidate push
- `N>1` = push カウンタが N の倍数に到達した safepoint で collect

`MUTSU_GC_EVERY_SAFEPOINT` の解釈:

- `0` または未設定 = 無効
- `1` = すべての collectable safepoint で collect
- `>1` は受け付けず warning + `1` 扱い

### 9.2a safepoint 種別を enum で固定する

`MUTSU_GC_AT` 導入前に、内部の safepoint kind を先に固定しておく。

Level 1a の collectable safepoint:

- `backedge`
  - bytecode dispatch loop の backward edge
- `call`
  - call frame push の直前または直後
- `return`
  - call frame pop / return merge の直前または直後
- `await`
  - promise/channel await の poll / resume 境界
- `react_poll`
  - react/supply drive loop の 1 poll 単位
- `lazy_force`
  - `force_lazy_list*` の pull / resume 境界
- `nested_run`
  - `with_nested_registers` で入る nested VM 実行の出入口
- `thread_join`
  - `start` / `hyper` / `race` の join / merge 境界
- `manual`
  - debug hook / explicit collect

Level 1a では **collectable でない**もの:

- lock 保持中の内部 helper
- `arc_contents_mut` 相当の借用中
- attr/array/hash の write-through 中

実装規則:

- collector の trigger 判定は必ず safepoint kind を受け取る
- `MUTSU_GC_EVERY_SAFEPOINT=1` は上記の collectable safepoint 全てに反応する
- 将来 `MUTSU_GC_AT=call,await,...` を導入するときは、この enum 名をそのまま文字列化する

規則:

- random 実行時は **seed を必ずログに出す**
- CI の主系は deterministic stress
- random は nightly / 手元再現 / 長時間 fuzz 向け

### 9.3 roast / test の回し方

想定する 3 段階:

1. `GC=off`
2. `GC=on + deterministic stress`
3. `GC=on + random(seed固定)`

最初の実運用:

- 通常 `make test` / roast は `GC=off`
- 追加ジョブで `GC=on + every safepoint`
- さらに重いジョブで `GC=on + every candidate`

random は常設 CI に必須ではないが、少なくとも release 前や nightly では回したい。

### 9.4 GC ログは必須

GC はログなしでは壊れたときに追跡不能になる。最初から段階別ログを入れる。

推奨 env:

- `MUTSU_GC_LOG=summary|detail|trace`

既定値:

- 未設定 = no GC log
- `summary` = 実運用 / CI 向け
- `detail` = 手元再現向け
- `trace` = 1 テスト / 1 再現専用

`summary`:

- collect 開始/終了
- cycle id
- reason (`manual`, `threshold`, `safepoint-stress`, `random`)
- candidates / roots / traced / reclaimed
- pause 時間

`detail`:

- `summary` に加え phase ごとの件数
- revived 数
- type 別 reclaimed 数

`trace`:

- node 単位イベント
- `candidate_push`, `scan`, `revive`, `reclaim`
- 単体再現・unit test 専用

出力方針:

- `stderr` へ出す
- `tmp/make-test.log` / `tmp/make-roast.log` に残る前提で grep しやすい固定文言にする
- summary は 1 collect 2 行程度に抑える

summary 例:

```text
gc: start cycle=42 reason=safepoint candidates=18 roots=241
gc: end cycle=42 traced=55 revived=31 reclaimed=24 pause_ms=0.37
```

trace 例:

```text
gc: candidate_push id=123 kind=Array cause=hash_insert
gc: revive id=123 kind=Array ext_refs=1
gc: reclaim id=456 kind=Promise
```

### 9.4a `MUTSU_VM_STATS` と GC summary の出力形式

既存の `MUTSU_VM_STATS` は

- 1 行 summary
- process-global counters
- `stderr`
- grep しやすい固定 prefix

という文化なので、GC もそれに揃える。

追加する summary line の方針:

- prefix は `[mutsu gc]` または `[mutsu vm-stats] gc:` のどちらかに統一する
- first cut では **`[mutsu gc]`** を推奨する
- `MUTSU_VM_STATS=1` 時に 1 run の aggregate summary を最後に出す
- `MUTSU_GC_LOG=summary` 時は collect ごとの streaming log を出す

つまり:

- **streaming per-collect log** = `MUTSU_GC_LOG`
- **end-of-run aggregate summary** = `MUTSU_VM_STATS`

first cut の aggregate summary に入れる項目:

- `collections`
- `candidate_pushes`
- `candidate_dedup_hits`
- `reclaimed_nodes`
- `reclaimed_cycles`
- `pause_ns_total`
- `pause_ns_max`
- `roots_scanned`

推奨フォーマット:

```text
[mutsu vm-stats] gc: collections=12 candidate_pushes=48 dedup_hits=7 reclaimed_nodes=19 reclaimed_cycles=6 pause_ns_total=812345 pause_ns_max=220111 roots_scanned=1440
```

streaming summary の推奨フォーマット:

```text
[mutsu gc] start cycle=42 reason=await safepoint=await candidates=18
[mutsu gc] end cycle=42 traced=55 revived=31 reclaimed=24 roots=241 pause_ns=370000
```

`trace` の推奨フォーマット:

```text
[mutsu gc] candidate_push cycle=42 id=123 kind=Array cause=hash_insert
[mutsu gc] revive cycle=42 id=123 kind=Array ext_refs=1
[mutsu gc] reclaim cycle=42 id=456 kind=Promise
```

ログ設計上の規則:

- key は snake_case ではなく既存 `vm-stats` に合わせて `key=value`
- 1 event = 1 line
- grep 頻出 key（`cycle`, `reason`, `kind`, `id`）は固定順に並べる
- random 実行時は first log line に seed を必ず出す

例:

```text
[mutsu gc] config mode=on random_seed=12345 random_rate=0.050000 every_safepoint=1 verify=1
```

### 9.5 verify hook

GC 導入初期は **verify を collector 本体と同じくらい重視する**。

推奨 env / hook:

- `MUTSU_GC_VERIFY=1`
- `gc_debug_collect_now()`
- `gc_debug_stats()`
- `gc_debug_verify_heap()`

`MUTSU_GC_VERIFY` の解釈:

- `0` または未設定 = verify 無効
- `1` = collect 前後 verify
- 将来的に `2` を導入して phase ごと verify を入れてもよいが、first cut は `0|1` のみ

verify で最低限見るもの:

- root から到達可能な node を誤 reclaim していない
- reclaimed node を二重解放していない
- child edge が壊れていない
- candidate buffer の重複や stale id が heap を壊していない

verify failure 時のログ:

- failing node id
- node kind
- parent/child edge
- root からの到達性
- 現在の cycle id / seed / mode

### 9.6 `MUTSU_VM_STATS` との関係

既存の `MUTSU_VM_STATS` に GC カウンタを載せる。

狙い:

- hot path に GC が乗っていないことを数字で見る
- `fib` / int-heavy benchmark で `gc_candidate_pushes = 0` を確認する
- worker thread を含む process-global の実行実態を 1 行 summary で見られるようにする

## 10. テスト方針

### 10.1 roast だけでは足りない

roast は最終的な相互作用を見るのに必要だが、GC の正しさを roast だけで担保するのは危険。
**heap graph の unit test を先に厚くする**。

### 10.2 unit test の層

最低限必要な層:

- graph unit tests
  - self-cycle
  - 2-node / 3-node cycle
  - root あり / root なし
  - cycle + 外部参照 1 本
- root enumeration tests
  - `stack`
  - `locals`
  - `upvalues`
  - `call_frames.saved_*`
  - supply registries
- collector invariant tests
  - candidate 重複
  - revive
  - reclaim
  - external ref がある node を誤回収しない
- stress unit tests
  - every safepoint
  - nested call / await / lazy force
- cross-thread tests
  - `Promise`
  - `Channel`
  - supply registry callback

### 10.3 pin test

GC 導入時に専用 pin を作る。

例:

- promise-channel cycle が `GC=off` では残り、`GC=on` では reclaimed される
- supply callback closure cycle が deterministic stress で再現できる
- `LazyList` の closure/env cycle が third wave 導入後に reclaim される

### 10.4 テストモードでの collect

unit test / integration test では random 依存にせず、**明示 collect hook** を使う。

原則:

- test は `gc_debug_collect_now()` を直接呼べるようにする
- `MUTSU_GC_EVERY_SAFEPOINT=1` は integration stress 用
- random mode は回帰の炙り出し用で、期待値のある test には使わない

## 11. 実装順序

1. root visitor の導入
2. child visitor の導入
3. `MUTSU_VM_STATS` の GC カウンタ枠だけ先に追加
4. `Gc<T>` / node header / candidate buffer の最小実装
5. `Array` / `Hash` / `ContainerRef` を first wave として移行
6. `Promise` / `Channel` を first wave の async node として移行
7. supply registry root visitor（`supplier_state_map` / `supplier_subscriptions_map` / `promise_combinator_map` / `supply_taps_map`）を first wave で導入
8. safepoint で synchronous collect
9. `Sub` / `Instance` / captured env / attrs を second wave で移行
10. `LazyList` を third wave として移行
11. OS resource 主体の async registry で `Value` edge を持つものを必要に応じて追従させる

## 12. この設計で捨てるもの

- Level 1a での background collector
- Level 1a での generational 化
- Rust スタックの precise scan
- `Value` 全体の handle 化
- 先に NaN-boxing をやること

## 13. 残る open questions

残件は 3 つまで絞る。

1. `Instance` は「本体 node」と「attributes cell」を別 node にするか
2. OS resource 主体の async registry（socket/proc/udp 等）のうち、どこまでを supply first-wave root visitor に同時包含するか
3. cooperative STW の停止プロトコルを `shared_vars` 系の既存同期とどう共存させるか

この 3 つ以外は、Level 1a では未決にしないほうがよい。
