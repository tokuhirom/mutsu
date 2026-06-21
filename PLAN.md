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
- 🔴 **残る `env_dirty` の物理削除はブロック中**。`env_dirty` はもはや correctness 機構ではなく、
  precise reconcile（`reconcile_locals_from_env_at_site`）のゲート＝安価な perf 最適化に降格している。
  だが完全削除には壁がある:
  - **multi-frame accumulation の壁（2026-06-21 実証）**: `via(); via()`（A→B→外側変数書込を2回）の累積は
    env_dirty-gated の blanket reconcile が支える唯一の機構。precise な単フレーム drain では届かず、source を
    親へ持ち越す retention 方式も drain 順序に脆弱で**置換不可**（実験で確認・revert 済）。
  - **根本**: 置き場が2つある限り「env を名前書きした→slot が stale かも」という目印は何らかの形で必要。
    `env_dirty` を真に消すには **env を locals の派生ビュー化＝単一ストア化**が要り、それは
    **env↔locals が同一コンテナ cell を共有する**こと（前提①）が前提。
- **✅ env↔locals 純 writeback コヒーレンスは完了**（slice 1〜1.20・#3400 まで）。OFF roast survey の純 writeback
  サーフェスは枯渇。**残る OFF 依存は別軸＝lazy-lists.t の laziness バグ**（`.kv`/`.pairs`/`.antipairs` の eager force・
  下記 §C 参照）。
- **∴ 次の一手 = lazy-lists 真 lazy 化（§C 末尾 ＋ §3-F の L 系）。** これを消化すれば OFF survey が実質クリアし、
  `env_dirty` 削除（§2-E）が射程に入る。それまで env_dirty は perf ゲートとして正当に残す。

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
      **★残る OFF 依存は純 writeback でない別軸 2 のみ**:
      - **lazy-lists.t 24-26 = laziness バグの露出**（`.kv`/`.pairs`/`.antipairs` が gather を eager force・ON は write-loss
        で偶然 raku 一致、OFF が正しく伝播して露出）。**修正＝`.kv`/`.pairs`/`.antipairs` の真 lazy 化**（§3-F の L 系と合流・
        precise-writeback では解けない）。**これが env_dirty 削除の最後の前提。**
      - IO-Socket-Async.t 5,7 = reactive 並行 flaky（決定的 pin 不可・env_dirty 削除の `blanket_reconcile_if_dirty` 空洞化で実挙動確認）。
- [ ] **★次の本丸 = lazy-lists 真 lazy 化（→ §1-A 解禁）**: 上記 lazy-lists.t を消化すれば OFF survey が実質クリアし、
      §2-E（`env_dirty`/`ensure_locals_synced`/`saved_env_dirty` 物理削除）が射程に入る。L 系（§3-F の L2b 系）と合流。
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
  - **HTTP::Server::Tiny v0.0.2**: 最有力ブロッカー＝`IO::Socket::Async.Supply(:bin)` が `Buf[uint8]` でなく
    `Str` を emit（`:bin` adverb 無視）→ ハンドラが `parse-http-request(Str)` で型エラー死。**ここが live server が
    死ぬ正確な地点＝単独最大インパクト。**
  - **HTTP::Parser v0.0.2**: `token SP { "\x20" }`（regex slang 内の**括弧なし** `"\xNN"`）がデコードされず
    grammar 全体が失敗（`\x[20]`/`\x[0d]` 等の括弧付きや非 regex の `"\x20"` は OK）。easy/medium。
  - **IO::Blob v0.0.1**: `class IO::Blob is IO::Handle` の user override（`.get`/`.lines` 等）が builtin native
    IO::Handle メソッドに shadow され `Expected IO::Handle` で死。MRO/dispatch バグ（builtin 型のサブクラスの
    user メソッドが native を優先すべき）。medium・汎用性高。
  - **HTTP::Status v0.0.5**: user `method sink` がシンクコンテキストで呼ばれず status table が空。
    ⚠️ 注意: 過去に sink 修正は sink.t を回帰させた（メモリ `sink-context-blocked-container-identity` 参照）。
- [ ] **DB アクセス — sqlite3 CLI ラッパ（pure Raku）が現実解。**
  - **DBIish/DBDish**: NativeCall 依存 → **ブロック**（ドライバは `sqlite3_*` C API）。API shape のみ再利用可。
  - **推奨**: `run`/`qqx` で `sqlite3`（`/usr/bin/sqlite3` 3.45.1, インストール済）を呼ぶ薄い pure-Raku ラッパ。
    mutsu の `run`/`qqx`（`:out`/`:err`/`exitcode`）は raku とバイト一致で動作確認済。`sqlite3 -json` で行を JSON 出力可。
    工数 ~1-2日。値エスケープ/1クエリ1プロセスは要注意。
  - **フォールバック**: flat-file `.raku`＋`EVALFILE`（mutsu で round-trip 確認済）は今日すぐ動く MVP。
- [ ] **JSON — JSON::Fast はロード不可（NQP 依存）。builtin or shim を推奨。**
      JSON::Fast 0.19 は `use nqp;`＋**~50 個の nqp op**（`list_i`/`findnotcclass`/`strfromcodes`/native int 等）に
      依存。mutsu は nqp op を 2 個（`atkey`/`atpos`）しか実装しておらず、`use JSON::Fast` 時点で `Unknown function:
      list_i` で死。全 op 実装は数週・高リスク。**代替＝mutsu に builtin `to-json`/`from-json` を実装**（or `nqp::`
      非依存の小さな pure-Raku JSON shim）。Template::Mustache 91/92-specs も即解禁。
- [ ] **ユーティリティ:**
  - [x] **File::Temp 0.0.12 — 完動（#TBD, 2026-06-22）。** `tempfile`/`tempdir` 実ファイル生成・
    write→read・END cleanup・`File::Directory::Tree` 依存ロードまで raku 一致。ブロッカーだった
    `use`/`unit module` の `:ver<>:auth<>` adverb（version/auth セレクタを import タグ扱いして `no such tag 'ver'`）を
    解消＝parser で dist セレクタとして消費・破棄。`unit module Foo:ver<>:auth<>` も対応。多数のモジュールに効く一般機能。
  - **MIME::Base64 1.2.5**: `decode-str` は OK、`encode-str` が誤り（`AA==`）。原因＝`Blob:D` 型パラメータに束縛した
    blob を `for $d -> $a,$b?,$c?` で**バイト反復できず** blob 丸ごとが `$a` に入る。medium（VM gap）。
  - **File::Directory::Tree 0.2**: 全 sub が `IO(Cool) $io` 強制型パラメータを使い `Str→IO` 強制不可で死。
    **coercion-type シグネチャパラメータ（`T()`/`T(U)`）の一般対応**が必要。medium・汎用性高。
- [ ] バイナリ配布: mise GitHub バックエンドのインストール検証 / GitHub Releases 自動化。

**次の高インパクト順（推奨）:** ✅① `use`/`unit module` の `:ver<>:auth<>` adverb（File::Temp 完動・#TBD）→
② builtin `to-json`/`from-json`（JSON 全般＋mustache specs）→ ③ `IO::Socket::Async.Supply(:bin)`→Buf
（HTTP server 本体が死ぬ地点）→ ④ coercion-type パラメータ `T()` → ⑤ builtin 型サブクラスの user メソッド
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
| 動作モジュール数 | **2（Mustache, File::Temp+File::Directory::Tree）** | 5+（ウェブブログスタック） |
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
