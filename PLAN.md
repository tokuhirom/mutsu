# PLAN.md — mutsu 今後の実装計画

> このファイルは**これからやる作業だけ**を載せる。完了したものは [news/](news/)（月別）へ移す。
> 過去の実装状況は [news/](news/)、パフォーマンス詳細は [PERFORMANCE.md](PERFORMANCE.md)、
> roast ブロッカー分析は [TODO_roast/BLOCKERS.md](TODO_roast/BLOCKERS.md) を参照。
>
> **最終更新 2026-06-21**: 全面再編。2大フラッグシップ（単一ストア化・tree-walking 撤去）が同一の
> 構造的前提に収束したことを反映（§1）。完了済みの大型キャンペーン詳細は news/2026-06.md へ移動。

## 方針

**実用的な Raku インタープリタ**として使ってもらえる品質を目指す。
起動 25 倍速い強みを活かし、CLI ツール・スクリプティング用途をメインターゲットとし、
最終的には **mutsu でウェブブログが作れる** レベルのライブラリ互換性を実現する。

### 🚫 標準ルール: 「1 操作 = 1 実装」を崩さない（ユーザー方針 2026-06-07）

実行エンジンは単一 struct `Interpreter`（＝ bytecode VM）に一本化済み。同じ Raku 操作を**二度書かない**:

1. 新規実装・修正は VM/native 層（`src/vm/` ＋ pure native `src/builtins/`）に **1 回だけ**書く。
2. carrier（EVAL / 正規表現の埋め込み `{}` ブロック等）が同じ処理を要するときは単一 native 実装へ**委譲**する。
3. 重複を見つけたら native を authoritative にして重複コピーを削除する。

---

## 1. 🎯 現在の戦略地図 — 2大フラッグシップは「第一級コンテナ＋状態所有」に収束する

2026-06-21 の棚卸しで判明した最重要の構造:

> **残る2つの大型キャンペーン（A:単一ストア化／B:tree-walking 撤去）は、別々の課題ではなく、
> 同じ2つの substrate 前提を共有している。**
>
> - **前提① 第一級コンテナ Phase 2 完了＋env↔locals がコンテナ cell を共有**（`docs/container-identity.md` /
>   `docs/env-locals-coherence.md`）
> - **前提② 状態所有（state ownership）＝レジストリ／IO ハンドル／型メタを Interpreter から VM が真に所有する**
>   （`docs/vm-interpreter-fallback-ledger.md` の ②③）

この収束を理解せずに各キャンペーンの「最後の一手」を急ぐと壁に当たる（本セッションで実証＝§A 参照）。
∴ **優先すべきは前提①②の substrate であり、それが A・B 両方を同時に前進させる。**

### A. 単一ストア化（locals↔env 二重ストア統合）— **correctness 目標は達成・物理削除は substrate 待ち**

設計＝[docs/env-locals-coherence.md](docs/env-locals-coherence.md) / [docs/vm-single-store.md](docs/vm-single-store.md)。

- ✅ **reverse pull（`sync_locals_from_env`）撤去済み（#3354, 2026-06-21）**。第27〜40セッションの write-through
  グラインド（約30 PR・roast OFF 依存 16/16 を precise 化）で「reverse pull なしで全 t/+roast green」を達成し、
  危険な同期処理を削除。**二重ストアの correctness hazard は解消。**
- 🟡 **`env_dirty` 物理削除は substrate グラインド進行中**（2026-06-22 着手）。実証で判明: `env_dirty` は
  ①blanket reconcile のゲート（boxing 下で無効＝除去可）と②**精密 reconcile（`reconcile_locals_from_env_at_site`・
  carrier/Proxy STORE/let-temp/closure 等のサイト）の perf ゲート（boxing 下でも load-bearing）**の2役。完全削除には
  精密 reconcile も不要化＝精密 reconcile 依存 surface を一つずつ precise writeback / cell 共有へ畳む必要がある。
  - **計測ハーネス `MUTSU_NO_PRECISE_RECONCILE`**（#3406）: `MUTSU_NO_BLANKET_RECONCILE=1 MUTSU_NO_PRECISE_RECONCILE=1`
    （double-OFF）＝boxing のみ＝env_dirty 削除後の到達状態。残 fail = 未変換 by-name writer。0（flaky 除く）で削除可能。
  - **✅ double-OFF surface: 16 → 0 到達**（2026-06-22・7 slice landed）: S1 bound-Proxy substr-rw/subbuf-rw/undefine
    （#3406）／S2 let/temp restore（#3408）／S3 closure-method nested-capture writeback（#3409）／S4 regex embedded
    `{ }`/`:let` cross-frame caller writeback（#3412・carrier 2 面消化）／S5 junction invocant autothread per-eigenstate
    writeback（#3414）／S6 resumable CONTROL handler writeback（#3416）／S7 react/whenever captured-outer writeback
    （**1 修正で 5 surface 一掃**＋do-whenever tap bind）。設計＝
    [docs/captured-outer-cell-sharing.md](docs/captured-outer-cell-sharing.md) §10。
  - **t/ サーフェス 0**（全 double-OFF pin 16 ＋ broad supply/react/concurrency/promise/start t/ が両モード PASS）。
    **⚠ だが全 roast whitelist（1285）の double-OFF sweep は 25 file の隠れサーフェスを露呈**（t/ pin は不完全＝
    第45「OFF roast survey こそ authoritative」と同型）。診断＝`tmp/roast-double-off-fails.txt`。env_dirty 削除は roast
    25→0 が前提。**S8（#3418・25→22）= user `.defined`（andthen/orelse/notandthen）**（CallDefined snapshot・1 修正 3 file）。
    **S9（#3419・22→21）= symbolic-deref store（`$::()=`/`::('$x')=`）**（`note_caller_env_write` ログ・scalar）。
    **S10（#3421・21→20）= user Proxy STORE（lvalue sub）**（`assign_proxy_lvalue` の env scalar スナップショット差分）。
    **S11（#3426・20→17）= lives-ok container carrier の Set/Bag/Mix writeback**（`exec_call_pairs_op` のスナップショット差分）。
    **S12（#3428・17→11）= 同 writeback の eligibility を slot-overwritable に拡張**（slot 現在値型で判定）。**6 file 消化**
    （our／pointy-rw／gather／coercion-methods／rw／kv）。**S13（#3430・11→7）= does/but mixin の captured-outer writeback**
    （①`Mixin` PartialEq inner 委譲対策の discriminant 差分判定 ②does/but op に carrier snapshot 差分 ③Hash slot 型変化の
    上書き許可）＝S14 roles `does`-mixin 4 消化（anonymous/mixin-6e/parameterized-mixin/submethods-6e）。**S14（#3432・7→6）= param `where` clause の captured-outer
    writeback**（`types/binding.rs` の where-eval 前後 env scalar スナップショット差分→`pending_caller_var_writeback`・
    named-parameters 消化）。**S15（#3433・6→5）= CAS block の captured-outer writeback**（`builtin_cas_var` の block 実行
    前後 env scalar スナップショット差分→`pending_caller_var_writeback`・cas-loop 消化）。
    **S16（#3437・5→4）= proto-multi 候補の captured-outer writeback**（`try_proto_method_body` の `run_proto_method`
    前後 env scalar スナップショット差分→`pending_caller_var_writeback`・defer-next 消化）。**★真因＝proto 候補は常に
    slow path（`run_instance_method_resolved`）経由で、捕捉 write を env に伝播するが `env_dirty` を立てない＝blanket pull
    すら発火せず default build でも slot stale（`is` 読みは reconcile site を踏み roast は偶然 green・`say` 直読みは壊れる）。
    ∴ S16 は ungated**（precise writeback は blanket の部分集合で正しい結果を変えず default 潜在バグも直す）。
    **S17（#3438・4→3）= custom HOW type-check メソッドの captured-outer writeback**（`Metamodel::Primitives.create_type`
    の HOW `type_check`/`accepts_type`/`find_method` が captured counter を `++`。3 dispatch サイトを
    `call_how_method_recording_writeback`（HOW 呼び出し前後 env scalar スナップショット差分→`pending_caller_var_writeback`）
    経由に置換 → smartmatch op 末尾の `apply_pending_caller_var_writeback` が drain・primitives 消化）。**counter が
    read-modify-write なので文跨ぎで消える**のが厄介。S16 同様 ungated（custom-HOW dispatch 限定スコープ）。
    **S18（#3439・3→2）= EVAL carrier の scalar 再代入 writeback を container slot にも適用**（`my $z=[]; EVAL q'$z=1'`
    の writeback が `writeback_carrier_writes`〔vm_env_helpers.rs〕で「古い slot 値が scalar か」判定だったため Array slot を
    スキップ→上書き拒否→blanket reconcile 頼み。適格判定を**新 env 値が scalar か**＋slot が `:=` bind cell でないことに変更）。
    **★terminator は「parser/auto-curly」誤分類で実体は EVAL writeback だった**（normal=z=1・double-OFF だけ z=[]）。一般的修正。
    **★writeback 候補は枯渇**（S16 proto-multi・S17 custom-HOW・S18 EVAL container-slot scalar）。**残 2 は両方 writeback で
    ない別軸**: ①**lazy-lists（真の laziness blocker・要 lazy 化）**②throttle（timing・flaky 系・決定的 pin 不可）。
    **★lazy-lists は精密切り分け済（重要）**: `gather{take $_ for 0..^$n; $was-lazy=0}.lazy` を `grep[^3]` で消費する
    `S02-types/lazy-lists.t` 14/16。**blanket だけ OFF でも precise だけ OFF でも fail（両方 ON の default のみ pass）**＝
    take counter 計測で double-OFF / blanket-OFF とも gather が **eager force される**（takes=10・normal は lazy で take 計上 0）。
    ∴ **precise writeback では解けず、blanket reconcile が laziness 維持に load-bearing**＝env_dirty 削除前に **grep/map が
    `.lazy` gather を eager force しない真 lazy 化が必要**（L 系別軸）。診断＝`tmp/lazy2.raku` 相当。
    **次セッション着手順**: ①lazy-lists の真 lazy 化（grep/map on lazy gather・最有力ブロッカー）→ ②throttle が flaky か最終確認
    （`.5〜.8 秒`タイミングアサート＝決定的 pin 不可なら env_dirty 削除のブロッカーから外す）→ ③§2-E（`env_dirty` 物理削除）着手。
- **✅ env↔locals 純 writeback コヒーレンス（blanket ON 下）は完了**（slice 1〜1.20・#3400）。lazy-lists.t laziness も
  解消（#3403）。OFF roast survey（blanket OFF）の決定的サーフェスは IO-Socket-Async.t flaky のみ。
- **∴ 次 = lazy-lists の真 lazy 化（確定した env_dirty 削除ブロッカー）→ throttle flaky 確認 → `env_dirty` 物理削除（§2-E）**:
  lazy-lists は blanket reconcile が laziness 維持に load-bearing（grep/map が `.lazy` gather を eager force するのを抑える）
  と精密切り分け済＝precise writeback では解けない genuine blocker。これを真 lazy 化で外してから、
  `blanket_reconcile_if_dirty`/`reconcile_locals_from_env_at_site` 空洞化 → `env_dirty`/`ensure_locals_synced`/
  `saved_env_dirty` 物理削除 → `cell_boxing_active()` gate 撤去で boxing 恒久 ON 化。

### B. tree-walking interpreter 撤去 — **struct 統合は完了・残フォークは状態所有待ち**

台帳＝[docs/vm-interpreter-fallback-ledger.md](docs/vm-interpreter-fallback-ledger.md) / [docs/vm-decoupling.md](docs/vm-decoupling.md)。

- ✅ **VM/Interpreter の struct 統合は完了**（CP-1/2/3・#3075〜#3104）。単一 struct が bytecode VM。
- **現状のフォールバック残量**: 記録される tree-walk フォールバックは **16 サイト**（メソッド 10／関数 6）。
  実測フォールバック率はメソッド ~1%／関数 ~18.6%（うち大半は EVAL **carrier**＝tree-walk ではない意図的委譲）。
  - **4 サイトは意図的 carrier**（MOP リフレクション `.WHAT`/`.HOW`/`^methods` 等）＝**撤去対象外**。
  - **残りは構造的ブロック**: 「easy/medium な個別フォールバック撲滅は枯渇済み」（台帳の結論）。残るのは
    ③状態所有（Buf/Failure/IO native メソッドが `io_handles`／型メタ／レジストリに依存）と Phase 2 コンテナ
    （hyper・array-backed instance メソッド）と multi-dispatch（proto/where 評価）。
- `src/runtime/`（~100 ファイル・~11万行）の内訳: **~60% は共有インフラ**（regex エンジン・クラス登録・IO・
  builtins＝VM も依存＝残す）、**~40% が純 tree-walk dispatch**（③ 完了後に削除可）。
- **∴ tree-walking の大量削除も前提②（状態所有）が前提。** 個別フォールバックの easy win は終わっている。

> **結論**: A も B も「地道なグラインドの続き」ではなく、**前提①②の substrate 着手**が律速。次章 §C がその substrate。

---

## 2. 🔴 順序依存・並列不可 — substrate（前提①②）

> 内部に着手順序があり、前段が終わるまで後段に着手できない。A・B 両フラッグシップの律速。

### C. 第一級コンテナ Phase 2 完了 → env↔locals コンテナ cell 共有（前提①）

実装台帳＝[docs/container-identity.md](docs/container-identity.md)。Phase 0/1 完了、Phase 3（インスタンス属性 cell）も
Stage 0〜2c 完了（Stage 3 = escape-aware cell 省略は perf 未正当化で deferred）。**残りは Phase 2 の最終キル＋coherence:**

- [ ] **Phase 2 Stage 2 slice 5（最終 SlotRef キル）**: 残る `HashSlotRef`/`DeferredHashAccess` 生成サイト
      （junction-bind / `is raw` reduce lvalue-read の autoviv）を cell 化し、variant を削除。
- [ ] **grep-rw-view 撤去**: 最後の ptr-keyed グローバル。matched 要素を cell 昇格し view registry を全廃。
- [x] **★env↔locals cell 共有 — captured-outer cell 化／純 writeback コヒーレンス（A の律速・完了）**: nested callee
      （closure・named sub）／carrier／cross-thread に捕捉＋変異される lexical の writeback コヒーレンスを precise 化。台帳＝
      [docs/captured-outer-cell-sharing.md](docs/captured-outer-cell-sharing.md)。**slice 1〜1.20 landed**（named-sub 捕捉
      scalar／metaop-thunk `Mu`／carrier single-frame／EVAL carrier multi-frame／container `@`/`%` cell 化／X-cross thunk／
      nested-method capture／cross-thread shared-var／object 添字代入 invocant／substr-rw・undefine lvalue／zip-topic・LAST
      phaser／proto `state %`／caller-frame write／param default self-scoping／**cross-thread DESTROY writeback（#3400）**）。
      **OFF roast survey の純 writeback サーフェスは枯渇**（残は別軸＝下記）。`:=` bind・closure captured scalar も done。
      **★残る OFF 依存は IO-Socket-Async.t の flaky のみ**:
      - **✅ lazy-lists.t 24-26 解消（2026-06-22）**: `.kv`/`.pairs`/`.antipairs` を lazy index-pipe（`MapGrepSpec.index_transform`）
        化し lazy ソースを eager force しないように修正。`my @res = one.kv` が gather body（`$was-lazy=0`）を走らせず、OFF で
        も PASS。値は eager 版とバイト一致（pairs=`i=>elem` / antipairs=`elem=>i` / kv=`i,elem` flat）。無限ソース上でも lazy。
      - IO-Socket-Async.t 5,7 = reactive 並行 flaky（決定的 pin 不可・env_dirty 削除の `blanket_reconcile_if_dirty` 空洞化で実挙動確認）。
- [ ] **★次の本丸 = `env_dirty` 物理削除（§2-E・→ §1-A 解禁）**: OFF survey の決定的サーフェスが枯渇したので、
      `blanket_reconcile_if_dirty` 空洞化 → `env_dirty`/`ensure_locals_synced`/`saved_env_dirty` 削除に着手できる。
- [ ] follow-up（pre-existing・小）: `$x = @arr` 共有の method param 版（`method m($n){ $n.push }`）・`is copy` $-param。
      設計＝[docs/scalar-array-sharing.md](docs/scalar-array-sharing.md) §5。

### D. 状態所有（state ownership）— VM がレジストリ／IO／型メタを真に所有（前提②）

台帳＝[docs/vm-interpreter-fallback-ledger.md](docs/vm-interpreter-fallback-ledger.md) ②③。**B の律速。**

- [ ] **レジストリ所有（②）**: クラス／ロール／enum／sub の宣言登録を VM 側へ。
- [ ] **IO ハンドル・型メタ所有（③）**: `io_handles` / `register_container_type_metadata` / regex キャッシュを VM が所有。
      これで Buf/Failure/IO native メソッドの catch-all フォールバック（`vm_call_method_compiled.rs:175/507/1543/1794`）が
      native 化でき、`runtime/` の ~40% 純 tree-walk dispatch を削除可能になる。
- [ ] **multi-dispatch の VM 化**: proto multi / where 制約評価を VM 側で（`vm_call_func_ops.rs:1051/1084`）。

### E. 単一ストア化の総仕上げ（C 完了後）

- [ ] **`env_dirty` / `ensure_locals_synced` / `saved_env_dirty` 削除**（§1-A）。**前提 = §C Sub-slice 1b で
      env↔locals がコンテナ cell 共有し乖離しなくなること。** ここで `pairs`/`slip` carrier-drop も安全化し、
      `locals` が単一権威・`env` は派生ビューになる。`ensure_locals_synced` は既に1行（`env_dirty=false`）へ縮退済。

---

## 3. 🟢 並列実装可能（独立・互いにブロックしない）

> substrate（§2）と critical path を共有しない。別ブランチで並行に進められる。着手時に該当 BLOCKERS/メモリを確認。

### F. roast backlog（[TODO_roast/BLOCKERS.md](TODO_roast/BLOCKERS.md) 駆動・インパクト順）

現状 whitelist **1285**。診断は `./scripts/roast-history.sh`（`tmp/roast-{panic,timeout,error,fail,pass}.txt`）。

- [ ] **★型付き例外（最高インパクトの単一ファイル）**: `S32-exceptions/misc.t`（42/157）。X::NotParametric /
      X::Undeclared / X::Redeclaration / X::Bind / X::TypeCheck 他 ~25 種の one-off 型実装。BLOCKERS.md §B。
- [ ] **lazy 無限配列 L2b–L4**: L1/L1b/L5/L5b/L2a は landed（→ news/2026-06）。残＝L2b（真のメモリ遅延化・seed `[1]`・
      `docs/lazy-arrays.md`「L2b」節に実行プラン確定済）→ `(1...*)`/closure 配列変換 → L4 slurpy 真 lazy 化。
      whitelist payoff（slurpy-params.t/slice.t）は Seq single-pass consumption（`X::Seq::Consumed`）が別軸。
- [ ] **Match キャプチャ番号付け / コンテナ kind**: (1) `$<x>=(...)` が positional スロットにも重複格納され番号がずれる、
      (2) `m:g//` を `my @m` 代入後 `@m.gist` が `(…)` を返す（receiver の List-kind dual-store）。S05-capture/array-alias.t（30/37）。
- [ ] 未実装演算子: `ff`/`fff`（flipflop 8 variants）/ `==>`・`<==`（feed precedence: `==>` が `=` より強く結合する差）/
      `~<`・`~>`（string bitwise shift・優先度低）。
- [ ] メタ演算子: generalized negation meta（`!op`）/ hyper assignment（`@a >>+=>> 1`）。
- [ ] Phasers: rvalue caching（INIT/CHECK/BEGIN as rvalues）/ PRE/POST（contract programming）。
- [ ] Signatures: type-check enforcement（X::TypeCheck）/ native int/uint overflow・bounds / 単一 sub の複数シグネチャ。
- [ ] OOP: namespaced construction（`A::B.new`）/ `augment class` / parameterized role mixin。
- [ ] Supply/Concurrency: backpressure / `supply`・`react` block scoping / Tap management（close, drain）。
- [ ] IO/Process: IO::Handle read modes（binary/encodings）/ Proc・Proc::Async 完全化 / file test operators（`-e`/`-f`/`-d`）。
- [ ] 孤立サブシステム（main-track 非衝突・BLOCKERS.md §A）: 残 regex（S05-substitution/match capturing-contexts）・
      Unicode CollationTest・shaped arrays・Pod。

### G. perf — 起動／実行速度（計測駆動・MUTSU_VM_STATS / timed roast）

現状（要再計測・PERFORMANCE.md）: **9/12 ベンチで raku 超え**。起動 0.04x（28倍速）。
**残ボトルネック**: method-call 2.7x / bench-class 2.3x / bench-fib（型制約付き）3.2x。
真因＝メソッド呼び出しの **env deep clone ~9μs/call（全コストの29%）**＝~100 entry の `Arc::make_mut`。

- [ ] **Lever 1: closure captures を indexed slot 化（高 payoff・設計済）**: closure 生成時の env deep clone を撤廃。
      コンパイル時に closure が read/write する変数を解析し `Vec<Value>` に格納。method-call <2x 狙い。
- [ ] **Lever 2: NaN-boxing（高 payoff・設計済）**: `Value` 48→8 bytes（Int/Num/Bool/Nil を NaN payload に）。
      int-arith 2x・fib ~30% 狙い。`value_size_guard` テストでサイズ監視中。
- [ ] **Lever 3: threaded dispatch（中 payoff・ラフ）**: opcode の `match` を関数ポインタテーブルに。命令律速ベンチ 10–30%。
- [ ] **Lever 4: JIT（Cranelift）/ Lever 5: 型制約チェックの tight-loop 省略**（ラフ・大）。
- [ ] 正規表現: 量指定子反復ごとの `RegexCaptures.clone()` 削減。
- 目標: method-call <1.5x、bench-class <1.5x、bench-fib（型制約付き）<2x。

### H. モジュール互換（Q3 — ウェブブログスタック）

目標: **mutsu でウェブブログシステムが構築できる**。**Template::Mustache 完動（#3395）**。
HTTP スタック/JSON/DB/ユーティリティは下記調査の通り NativeCall 非依存で動作可能、各々独立した一般機能の欠落待ち。

- [x] **Template::Mustache — 完動（#3395, 2026-06-21）。** 全テストがパス（外部 `JSON::Fast` 依存の
      91/92-specs を除く＝mutsu のバグではない）。最後のブロッカー 06-logging（深いフレームで投げた `warn` を
      unit の `CONTROL { default { …; .resume } }` で受けて深部を継続）を #3395 で解決＝resume_safe な CONTROL を
      raise 地点でインライン実行。詳細＝メモリ `project-template-mustache-status`、news/2026-06.md。
      残（非致命・別軸）: 50-readme #4 grammar パース性能（遅いが正しい）、`handles` 委譲経由 proto method（stderr のみ）。
#### モジュール動作状況調査（2026-06-21, mutsu でロード＋テスト試行）

候補モジュールを zef で取得し mutsu で `use`＋テスト試行した結果。**HTTP スタック・JSON・ユーティリティは
すべて NativeCall 非依存**（pure Raku）で、原理的に動作可能。各ブロッカーは独立した一般機能の欠落。
ハーネス＝`tmp/webstack/`（gitignored）。

- [ ] **HTTP::Server::Tiny スタック（全て pure Raku, NativeCall なし）— 想像以上に近い。**
      本体は `use`＋`.new`＋非同期サーバが TCP listen/accept まで実際に動く。リクエスト/レスポンス往復を阻む
      独立した4バグ:
  - [x] **HTTP::Server::Tiny v0.0.2 — `IO::Socket::Async.Supply(:bin)` が `Buf[uint8]` を emit するよう修正（#TBD,
    2026-06-22）。** real-TCP Supply パス（`async_socket_supply_real_tcp`）が `:bin` adverb を無視して常に `Str` を
    emit していた（reader thread が `from_utf8_lossy`）→ `:bin` を受け取り `Self::make_buf`（`Buf[uint8]`）を emit。
    in-memory パスの bin Buf 構築も `Self::make_buf` に統一（従来は素の `Buf`）。テスト `t/io-socket-async-bin.t`。
  - **残（別の既存バグ・live server 続行に必要）**: `whenever $conn.Supply(...)` の **内側**で `done`/`last` を呼ぶと
    制御シグナルが react のハンドラに捕捉されず「Unhandled exception in code scheduled on thread」（空メッセージ）で
    プロセス終了する（bin/非 bin 共通・`.tap` 回避なら OK）。real-TCP Supply の tap コールバックが worker thread 上で
    走り、react の control-flow フレームから切れているため。HTTP::Server::Tiny のリクエストループが `done` を使うなら要修正。
  - [x] **HTTP::Parser v0.0.2 — regex slang 内の括弧なし `"\xNN"` デコードを修正（#TBD, 2026-06-22）。**
    double-quote regex 文字列の `\x` ハンドラが `\x[HH]`（括弧付き）のみ処理し、`"\x20"` を "x20" に落としていた
    （`token SP { "\x20" }` で grammar 全体が失敗）→ クォート外と同じく括弧なし連続 hex 桁を読むように。テスト
    `t/regex-dq-hex-escape.t`。**これで HTTP::Parser の grammar がロード・パース実行できるようになった**（10 件中 2 件
    PASS）。残る 8 失敗は別軸の独立バグ（Buf/byte 列処理・`.subst`・encode 往復）で、`\x20` とは無関係。
  - [x] **HTTP::Parser — 完動 14/14（#3420/#3422/#3423, 2026-06-22, session 10）。** 残 8 失敗は grammar action
    ディスパッチの欠落だった: (1) action walk が numbered `( )` グループ内のルールを辿らない（#3422 positional 再帰）、
    (2) silent subrule `<.foo>` 自身の action が reduce-time に発火しない（#3423 隠し `silent_caps` チャネル — マーカーキー
    で `named_subcaps` に格納し `.hash` から不可視のまま action を発火、positional グループ降下時は silent_caps のみで
    named children は二重発火回避）、(3) `$req.first(* > 127, :k)` が Blob をバイト反復せず（#3423 `.first` メソッド
    dispatch で `buf_as_byte_items`）。併せて #3422 が混入させた `t/grammar-reduce-time-dynvar.t` 二重ディスパッチ回帰も解消。
    既知ギャップ: `( <foo> )`（グループ内 named ルール）の action 未発火（match-tree リーク、深い）。
  - [x] **IO::Blob v0.0.1 — builtin 型サブクラスの user メソッド override を修正（#TBD, 2026-06-22）。**
    `class IO::Blob is IO::Handle` の user override（`.get`/`.lines`/`.getc`/`.word`/`.words`）が、Instance dispatch の
    `is_native_method` フォーク（`vm_call_method_compiled.rs` の &self/&mut 両経路）で継承元 native IO::Handle メソッドに
    shadow され `Expected IO::Handle` で死んでいた。フォーク条件に `&& !has_user_method(class, method)` を追加し、クラスが
    MRO 上に自前メソッドを持つときは native を取らないように。`has_user_method` は `is_native_method` が真のときのみ評価される
    （short-circuit）ので hot-path コストは最小。`get`/`lines`/`getc`/`word`/`words`/constructor が動作、**Text::CSV を
    IO::Blob から読む `t/020-text-csv.t` が PASS**。テスト `t/builtin-subclass-method-override.t`（8 件）。
    ✅ `SeekType` enum も登録済み（#TBD, 2026-06-22）: `SeekFromBeginning`/`SeekFromCurrent`/`SeekFromEnd` を組み込み enum 化
    （`init_seek_type_enum`）し `SeekType:D` デフォルト型チェックが通るように。単独の `$io.seek(n); $io.read(n)` は raku 一致。
    残（別軸・本タスク外）: 010_basic の `subtest {}` 内 `read`/`print`/`say`/`write`/`slurp-rest`/`close`/`nl`/`Supply` が
    なお fail（フルファイル内のみ・分離では PASS）。Buf `is` 比較／`.data is rw` 書き戻し／nl-out／Supply など複数の独立バグ。
  - **HTTP::Status v0.0.5**: user `method sink` がシンクコンテキストで呼ばれず status table が空。
    ⚠️ 注意: 過去に sink 修正は sink.t を回帰させた（メモリ `sink-context-blocked-container-identity` 参照）。
- [ ] **DB アクセス — sqlite3 CLI ラッパ（pure Raku）が現実解。**
  - **DBIish/DBDish**: NativeCall 依存 → **ブロック**（ドライバは `sqlite3_*` C API）。API shape のみ再利用可。
  - **推奨**: `run`/`qqx` で `sqlite3`（`/usr/bin/sqlite3` 3.45.1, インストール済）を呼ぶ薄い pure-Raku ラッパ。
    mutsu の `run`/`qqx`（`:out`/`:err`/`exitcode`）は raku とバイト一致で動作確認済。`sqlite3 -json` で行を JSON 出力可。
    工数 ~1-2日。値エスケープ/1クエリ1プロセスは要注意。
  - **フォールバック**: flat-file `.raku`＋`EVALFILE`（mutsu で round-trip 確認済）は今日すぐ動く MVP。
- [x] **JSON — `to-json`/`from-json` をネイティブ実装（#TBD, 2026-06-22）。** JSON::Fast 0.19 は `use nqp;`＋~50 個の
      nqp op 依存でロード不可のため、`use JSON::Fast` / `use JSON::Tiny` を組み込みモジュールとして認識し（`Test` と同方式・
      `loaded_modules` ゲート）、`to-json`/`from-json` を Rust ネイティブ実装（`src/runtime/json.rs`、ディスパッチは
      `src/vm/vm_native_json.rs`）。raku JSON::Fast とバイト一致（pretty 2-space / `:!pretty` / `:sorted-keys` / `:spacing`、
      Rat→`.0`・Num→`e0`、null→`Any`、decimal→Rat・exp→Num、surrogate escape）。テスト `t/json.t`（34 件）。
  - [x] **hash 要素 cell の pair-value デコンテナ化（#TBD, 2026-06-22）。** `%specs{ .basename } := %data<tests>` 後の
    `%specs.head.value.head<template>` が「Type Array does not support associative indexing」で死んでいた原因を修正。
    hash 要素は `ContainerRef` cell で格納されるが、hash を pairs として反復（`.pairs`/`.head`/`.kv`/`.antipairs`/`.sort`）する
    際に cell を Pair 値へそのまま入れていた（`%h<k>` 読み・`.values` は deref していたのに不整合）→ `+`/`.elems` が cell を
    単一スカラ扱い。`typed_pair`（hash→pair の中心）＋ `.pairs`/`.kv`/`.antipairs` で `deref_container()` 適用。テスト
    `t/bind-hash-value-pairs.t`（14 件）。**これで 91/92-specs のハーネスが実際の spec を走るようになった。**
  - **91/92-specs の残（別軸・本タスク外）**: 実 spec の rendering ギャップ（delimiter 永続化／inheritable partials／lambda）と、
    最初の spec のみ `+$spec.value`=0 になる subtest/Seq-consumption 系バグ。いずれも itemization とは独立。
- [ ] **ユーティリティ:**
  - [x] **File::Temp 0.0.12 — 完動（#3399, 2026-06-22）。** `tempfile`/`tempdir` 実ファイル生成・
    write→read・END cleanup・`File::Directory::Tree` 依存ロードまで raku 一致。ブロッカーだった
    `use`/`unit module` の `:ver<>:auth<>` adverb（version/auth セレクタを import タグ扱いして `no such tag 'ver'`）を
    解消＝parser で dist セレクタとして消費・破棄。`unit module Foo:ver<>:auth<>` も対応。多数のモジュールに効く一般機能。
  - [x] **MIME::Base64 1.2.5 — 完動（#3427, 2026-06-22, session 11）。** 5 つ目の動作モジュール。`Blob:D` 型パラメータの
    blob を `for $d -> $a,$b?,$c?` でバイト反復できるよう修正（`encode`/`decode`/`encode-str`/`decode-str` 全て raku 一致）。
    own test **4/4 ファイル完全パス**（basic 18/18, rfc4648 16/16, oneline 2/2, binary-and-long-line 11/11）。4 つ目は
    pack/unpack（#3429）＋小文字 native buffer 型エイリアス `blob8`/`blob16`（#TBD）で完動。
    Option B 採用＝for ループ materialization（`vm_control_ops.rs` `for_blob_byte_items`）と `.map`/`.grep` で、裸の Blob 値と
    `for $scalar`→`[$scalar]` ラップ（単一要素 `List` 配列内の Blob）の両形をバイト列展開。`value_to_list` は不変。Blob に
    itemization マーカーがないため `for $my_blob`/`($blob,)`（raku: 1 要素）もバイト展開する divergence を受容（roast の
    Buf/Blob 56 ファイルに `for $` は皆無で未踏）。正しい解 Option A（Blob itemize マーカー）は将来回帰時に。`t/blob-for-iteration.t`（11 件）。
  - [x] **File::Directory::Tree 0.2 — 完動（#TBD, 2026-06-22）。** 全 sub が `IO(Cool) $io` 強制型パラメータを
    使う。3 つの一般バグを修正: (1) `IO` ロールを `IO::Path`/`IO::Special` が does するよう `type_matches` に追加
    （`Str.IO` が返す `IO::Path` が `IO` ターゲットにマッチせず coercion 失敗していた）、(2) coercion param `T(S)` が
    既に `T` 型の値を拒否していた（source `S` のみ検査）→ source にも target にもマッチしない場合のみ拒否（binding.rs
    ＋sub-signature 両経路）、(3) 既にターゲット型を満たす Instance の再コアースで存在しない coercion メソッドを
    slow-path 呼び出しして死ぬ問題を `try_coerce_value_with_method` の早期 return で解消。加えて user/main thread の
    スタックを 32MB→256MB に拡大（rmtree が 120 段ネストを相互再帰で削除するため）。テスト `t/io-coercion-param.t`
    （14 件）＋モジュール自身の `t/01-basic.rakutest` 全 9 件 PASS（raku 一致）。
- [ ] バイナリ配布: mise GitHub バックエンドのインストール検証 / GitHub Releases 自動化。

**次の高インパクト順（推奨）:** ✅① `use`/`unit module` の `:ver<>:auth<>` adverb（File::Temp 完動・#3399）→
✅② native `to-json`/`from-json`（JSON 全般・#3402。mustache 91/92 は別の `:=`-hash-itemization バグ待ち）→
✅③ `IO::Socket::Async.Supply(:bin)`→`Buf[uint8]`（#TBD。live server には別途 `whenever Supply`内 `done` teardown 修正が要）
（HTTP server 本体が死ぬ地点）→ ✅④ coercion-type パラメータ `T(S)`（File::Directory::Tree 完動・#TBD。IO ロール
＋coercion-param が既存 target 型を受理＋深い再帰用スタック拡大）→ ⑤ builtin 型サブクラスの user メソッド
override 解決（IO::Blob）。①②④⑤ はいずれも単一モジュールを超える一般機能。

### I. Track C — 並行（共有セル）残

スカラ／state の `start` 間ライブ共有・hash/array 要素 atomic は landed（→ news）。残:

- [ ] **`state @`/`%`・lexical aggregate の真共有**（Track B 要素 cell 基盤に依存）。
- [ ] Semaphore / nonblocking await / lock 競合（S17・hard・別軸）。
- [ ] `unsafe` の single-thread 前提コメント是正（`Arc::as_ptr as *mut` を strong_count ガード前提に・最終的に要素も cell 化）。

### J. 構造リファクタ・将来（独立・中長期）

- [ ] 制御フロー（`return`/`last`/`next`/`take`/`emit`）を `RuntimeError` god-struct から `enum Control` へ分離（ANALYSIS §2.4）。
- [ ] `.^methods`/`.can` を実ディスパッチ表から導出 / roast fudge ロジック分離 / 500 行超ファイル分割。
- [ ] エラーメッセージ品質向上 / エッジケースの panic・crash を 0 に。
- [ ] REPL / Debugger / `zef` 互換 / native binary output / WASM playground 公開。

---

## メトリクス

| 指標 | 現在 | 目標 |
|------|------|------|
| Whitelist | **1285** | 1300+ |
| fib(25) vs raku | **1.0x** | <10x |
| method-call vs raku | **2.7x** | <1.5x |
| bench-class vs raku | **2.3x** | <1.5x |
| bench-fib（型制約付き）vs raku | **3.2x** | <2x |
| 起動時間 vs raku | **0.04x** | 0.04x |
| tree-walk フォールバック（メソッド/関数） | **~1% / ~18.6%（大半 carrier）** | 0%（carrier 除く） |
| 動作モジュール数 | **5（Mustache, File::Temp, File::Directory::Tree, HTTP::Parser, MIME::Base64〔own test PASS〕）** | 5+（ウェブブログスタック） |
| Template::Mustache / HTTP::Server::Tiny | **Mustache ✅** / Tiny ❌ | ✅ |

---

## ✅ 完了した大型キャンペーン（詳細は news/、ここには残さない）

- **VM decoupling / tree-walking struct 統合（CP-1/2/3, #3075〜#3104）** — 単一 struct が bytecode VM。
- **単一ストア化 write-through グラインド（#3219〜#3354, 第12〜40セッション）** — reverse pull 撤去まで完了
  （env_dirty 物理削除は §1-A・§2-E で substrate 待ち）。詳細＝news/2026-06.md ＋ memory `project_dual_store_unification_next`。
- **第一級コンテナ Phase 0/1・Phase 3 Stage 0〜2c** — escape 解析・スカラ cell・インスタンス属性 cell（CAS 含む）。
- **React/Supply 統一ループ（Track C Stage 1〜3）** — whenever/LAST/QUIT/CLOSE 全 native。
- **lazy 配列 L1/L1b/L5/L5b/L2a（#3306〜#3315）** — lazy `.gist`/`.elems`/reify-on-demand（整数レンジ）。
- **panic→`X::` 境界 / 無限 Range クラッシュ撲滅 / roast 90% 突破** — 完了。
- **重複実装カタログ消化（dedup A/B/C・レバー A/B/C）** — 完了。
