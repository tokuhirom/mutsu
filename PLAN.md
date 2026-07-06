# PLAN.md — mutsu の実装計画

> このファイルには **未完了の作業だけ** を載せる。完了した作業は [news/](news/) に移す。
> 過去ログは [news/](news/)、性能の詳細は [PERFORMANCE.md](PERFORMANCE.md)、
> roast の失敗分析は [TODO_roast/BLOCKERS.md](TODO_roast/BLOCKERS.md) を参照。
>
> **最終更新 2026-07-05**:
> ゴールを「**Battery included な Raku 処理系**」として再定義し、全面再構成した。
> 旧構成にあった完了済み記述は [news/2026-06.md](news/2026-06.md)（アーカイブ節含む）と
> [news/2026-07.md](news/2026-07.md) に移した。

## ゴール — Battery included な Raku 処理系

**mutsu をインストールするだけで、ドキュメントが整備された標準添付ライブラリが揃い、
すぐに実用コードが書ける Raku 言語インタープリタ**を作る。

bun が JavaScript に対して取ったポジショニング（ランタイム＋パッケージマネージャ＋標準ツールを
単一の高速なバイナリで提供）の Raku 版。公式 Rakudo エコシステムには battery-included な
ディストリビューションが存在せず、そこが mutsu の独自の立ち位置になる。構成要素は 4 つ:

1. **高速起動の互換インタープリタ** — 起動 raku 比 0.04x・roast whitelist 1350。CLI ツールと
   スクリプト実行を主戦場に、raku 互換性を維持・拡大する。→ §3 / §4
2. **標準添付ライブラリ（batteries）** — JSON / HTTP / テンプレート / DB / ファイルユーティリティ等を
   同梱し、インストール直後に `use` するだけで動く。**各ライブラリにドキュメントを付ける**。→ §1
3. **同梱パッケージマネージャ `mzef`**（実 Zef を vendoring）— 標準添付で足りないものは fez
   エコシステムから取得できる。→ §1 B2
4. **土台品質** — GC（table stakes・進行中）・性能・エラーメッセージ。→ §2 / §5

## この文書の読み方

- **§1 Batteries** がゴール直結の本丸: 同梱ライブラリの選定・vendoring・ドキュメント・mzef・配布。
- **§2 GC** が進行中の大型キャンペーン（Phase B）。
- **§3 substrate / §4 roast / §5 perf / §6 並行・構造** は土台品質の残件。
- 具体的な roast 失敗の対応順は [TODO_roast/BLOCKERS.md](TODO_roast/BLOCKERS.md) を参照。

### フェーズ構成（ADR-0001）

性能と互換性で raku に追いついたその先、**GC と JIT が次の大型ジャンプ**になる。
GC のないインタプリタは「欠陥品」とみなされ誰も使わない＝GC は table stakes。順序と方式は
[docs/adr/0001-gc-strategy-and-phasing.md](docs/adr/0001-gc-strategy-and-phasing.md) で決定済み。
**2026-07-03、Phase A 完了（roast 目標達成）を確認し GC 着手を決定**（[ADR-0002](docs/adr/0002-phase-a-gate-reassessment.md)）。要点:

| フェーズ | 内容 | 本書の該当 |
|---|---|---|
| **A. 追いつく** | 互換性＋速度で raku に並ぶ（**完了・ADR-0002**） | §3 / §4 / §5 に残件のみ |
| **B. Value 表現リワーク＋GC** | Track B（要素 cell 化）＋ cycle collector を**統合**（ADR 層3a）（**いまここ**） | §2 |
| **C. JIT** | 独自メリット | §5 Lever 4 |

- **GC は JIT の前**（JIT を GC 前提の上に載せる）。
- **方式 = cycle collector on Arc（non-moving + refcount・レベル1 採用）**。スカラ系は型フィルタで GC 対象外
  ＝数値/文字列 hot path はコスト 0。性能は GC でなく JIT で稼ぐ。
- **Track B は GC と一体（層3a）。単独で先行着手しない。** NaN-boxing は JIT の地ならし（層3b）、
  biased refcount は層3c。
- Batteries（§1）は GC と並行に進められる（互換性・モジュール・配布の作業は Value 表現に依存しない）。

### 🚫 標準ルール: 「1 操作 = 1 実装」を守る（ユーザー方針 2026-06-07）

実行エンジンは単一の `Interpreter` 構造体（= bytecode VM）に統合済み。
同じ Raku 操作を**複数箇所に重複実装しない**:

1. 新規実装・修正は VM/native 層（`src/vm/` ＋ pure native `src/builtins/`）に **1 回だけ**書く。
2. 呼び出し経路（EVAL / 正規表現の埋め込み `{}` ブロック等）が同じ処理を要するときは、単一の native 実装へ**委譲**する。
3. 重複を見つけたら native 実装を正本にして、重複コピーを削除する。

---

## 1. 🔋 Batteries — 標準添付ライブラリと配布（ゴール直結・本丸）

動作実績のあるモジュール（詳細は [news/2026-06.md](news/2026-06.md)）: JSON native
（`to-json`/`from-json` #3402）/ Template::Mustache / File::Temp / File::Directory::Tree /
HTTP::Parser / MIME::Base64 / HTTP::Server::Tiny（end-to-end HTTP 配信）/ Tubu（自作同期
ウェブフレームワーク・`t/lib`）/ DBDishLite（自作 SQLite 層・`t/lib`）/ NativeCall MVP
（実 SQLite CRUD 往復）/ zef CLI。

現状これらは「動く」だけで、**同梱・ドキュメント・継続保証**の 3 点が未整備。battery included を
名乗るにはこの 3 点を揃える必要がある — それがこの節。

### B1. バンドルセットの確定・vendoring・ドキュメント

- [ ] **バンドル対象リストの確定**。第一候補（動作実績ベース）:
      JSON（native 組込）/ Template::Mustache / File::Temp / File::Directory::Tree / HTTP::Parser /
      MIME::Base64 / HTTP::Server::Tiny / Tubu（同期 WAF）/ DBDishLite（SQLite）/ NativeCall。
      「ウェブブログが標準添付だけで書ける」を選定基準にする（HTTP クライアントの穴は要調査）。
- [ ] **vendoring 機構**: 同梱モジュールをソースツリー（例: `modules/`）に vendoring し、
      インストールされた mutsu が追加設定なしで解決できるようにする（`MUTSULIB` の組込デフォルト化、
      ないし `Interpreter::new()` への標準 lib path 登録 — `add_default_site_repo()` と同じパターン）。
      precomp キャッシュとの整合も確認。
- [ ] **`t/lib` 自作ライブラリの昇格**: Tubu / DBDishLite をテスト補助置き場から正式な同梱モジュールへ
      移し、命名・API・独立テストを整える。
- [ ] **ドキュメント整備**: 同梱ライブラリごとに使い方ドキュメント（概要・インストール不要で使える旨・
      API リファレンス・実例コード）。置き場は `docs/batteries/`（ないしトップ `BATTERIES.md` 起点）。
      「ドキュメントが整備された」がゴールの明文要件なので、モジュール追加時はドキュメント必須にする。
- [ ] **動作モジュール回帰 CI**（PR 非ブロック・main push 時検知・ユーザー方針 2026-06-28）:
      一度「動いた」モジュールが動き続けているかを継続検知する。各既知モジュールを `use`＋スモーク
      （ロード・代表 method・`zef --help` 出力一致）するハーネス。外部 dist は CI で `zef fetch` するか
      vendoring。失敗はレポートのみ（赤でも main は止めない運用）。同梱セットが決まればその smoke が
      そのまま battery の品質ゲートになる。

### B2. mzef — 実 Zef を同梱した `mzef` パッケージマネージャ（north-star・ユーザー方針 2026-06-28）

ビジョン: **mutsu をインストールすると `mzef` コマンドが使える**。実装は**reimplement せず Zef そのもの**
（upstream）を使う。zef は巨大な実 Raku プログラムなので、**最強の互換性北極星**でもある（zef 由来の
一般バグ修正を多数 landed 済み — news 参照）。

到達点（詳細は news/2026-06.md・news/2026-07.md）: ✅ CLI ロード＋コマンド dispatch（`zef --help`/
`--version` が動作）/ ✅ CompUnit::Repository の install→use 橋（`repository-for-name` well-known 名・
デフォルト site repo 自動登録・担保 `t/compunit-repository-for-name.t`）。残:

- [ ] **実 zef バイナリの end-to-end 実行を阻む 2 バグ**:
      (a) `Zef::Client` の `%`-sigil 属性に Associative をダックタイピングする非 Hash オブジェクト
      （`has %.hash handles <AT-KEY EXISTS-KEY ...>`）を bind すると `coerce_attr_value_by_sigil`
      （`methods_signature.rs`）の catch-all を素通りして raw Instance のまま格納される。
      (b) 実 `Zef::CLI.rakumod` のコンパイルを止めるパーサエラー（未特定）。
- [ ] 既知の小差異: CLI 数値文字列の `Int $n` への coerce が raku より積極的（`MAIN(Int $n,…)` に `7` が
      マッチ・raku は slurpy fallback）。実用上は mutsu 側のほうが直感的。
- [ ] **network fetch**: fez エコシステム（`https://360.zef.pm/`）への取得。堅牢な async TLS が前提。
- [ ] **実 install＋build/test 実行**、`mzef` バイナリ shim ＋ zef 本体＋依存＋config の vendoring
      （debian の zef は `resources/bin/zef` 欠落・要 known-good vendoring）。

切り分け: **「実 Zef をテスト標的として走らせ続ける」は即効性が高く継続**。同梱 installer としての
実 install は network fetch（TLS）が最大の前提工事。

### B3. 配布・ツール

- [ ] **バイナリ配布**: mise GitHub バックエンドのインストール検証 / GitHub Releases 自動化。
      「インストールするだけで揃う」の入口なので B1 vendoring と同時に設計する
      （バイナリ＋同梱モジュールツリーのパッケージング）。
- [ ] REPL / Debugger / native binary output / WASM playground 公開。

### B4. モジュール互換の残ブロッカー（batteries の裾野）

- [ ] **NativeCall 残**: ① `CArray[uint8]`・`CArray[Str]` ② `is repr('CStruct')` 構造体 ③ callback
      （汎用 C コールバック）。MVP〜実 SQLite CRUD までは完了（news/2026-06.md アーカイブ節）。
- [ ] **Humming-Bird 完全配信の残 2 ブロッカー**（LOAD＋LISTEN＋accept＋decode までは動作・#3549）:
      **B1** = 型付きパラメータ→呼び出し元同名 lexical への `var_type_constraint` 漏れ（グローバル
      name-keyed HashMap を呼び出し境界で scope する正攻法。env-authoritative 化は subset-6e を壊すため
      不可）。**B2** = detach した `start{react{whenever $chan{}}}` が await されない限り駆動されない
      ＝並行スケジューリング campaign。詳細＝memory `session-24-humming-bird-loads`。
- [ ] **HTTP::Server::Tiny 深掘り時の残**: keep-alive 連続リクエスト・chunked request body・
      `whenever $conn.Supply(...)` 内の `done`/`last` 制御シグナル（tap コールバックが worker thread 上で
      走り react の control-flow フレームから切れる）。デフォルト構成では発火しないため基本配信は無影響。
- [ ] **Template::Mustache 残**（91/92-specs）: delimiter 永続化／inheritable partials／lambda ＋
      最初の spec のみ `+$spec.value`=0 になる subtest/Seq-consumption 系バグ。
- [ ] stored Regex `<$var>` lexical capture 喪失（Tubu で発見・別軸）。
- 📌 off-the-shelf `DBDish::SQLite` は `MoarVM::Guts::REPRs`（MoarVM 内部表現の直接エミュレート）依存で
  原理的に動かせない＝事実上の壁。実用 SQLite は DBDishLite + NativeCall が正道（調査結論＝news/2026-06.md）。
  副次発見の一般パースバグ（未修正）: `constant NAME is export = <cond> ?? <Type> !! <Type>` の
  ternary then 枝 greediness。

---

## 2. ★ Phase B: GC（cycle collector on Arc）→ JIT（進行中・2026-07-03 着手）

実装順序は [docs/gc-level1-detailed-design.md](docs/gc-level1-detailed-design.md) §11。
**完了分**（詳細は news/2026-07.md）: §11 step 1〜4（root/child visitor・`MUTSU_VM_STATS` GC カウンタ・
`Gc<T>`/node header/candidate buffer）/ step 5 first wave（`Hash`/`Array`/`ContainerRef` の `Arc→Gc`、
`Set`/`Bag`/`Mix` #4117、dead 化した `arc_contents_mut` 削除）/ **step 6-7（`Promise`/`Channel` の Gc 化＋
supply registry root visitor #4127）** / step 8 trial-deletion collector 本体＋safepoint 配線
（GC-on 出力が GC-off と byte 一致・`tests/gc_stress.rs`）/ step 9 second wave（`Sub`/`Instance`
attributes #4123、`WeakSub`=`WeakGc`）/ **step 10（`LazyList` third wave #4125）** / `gc_trace` wrapper
完全性（#4124/#4134/#4135）/ `WeakGc<T>` 仕上げ / cross-thread 安全化（minimal STW #4130）/
`make_mut`/`get_mut` uniqueness の `header.strong` 基準化 / デバッグ運用（`MUTSU_GC_LOG`/`MUTSU_GC_VERIFY`）/
CI `gc-stress` ジョブ / **`DESTROY`-on-reclaim（`Trace::finalize` フック＝refcount 死・cycle reclaim の
両方で発火、Rust `Drop` から分離）＋ dead sweep（strong=0 candidate は trial deletion を経ずに
worker 稼働中でも解放＝threaded 変異ループの unbounded 蓄積と数秒 pause を解消、pause_max 2.8s→16ms）＋
inner dispatch（`run_range`）backedge safepoint**。

**cross-thread cooperative STW 完成（本スライス・設計メモ §6.1）**: collector が `STOP_REQUESTED` を立て、
他の全 mutator が quiescent（safepoint で park、または blocking wait＝promise/channel/lock/semaphore の
condvar・join・sleep が safe region としてカウント）になってから cycle scan を実行。quiescence が
間に合わない場合（未ラップの blocking site）は timeout→suspects 再キュー＋cooldown で従来動作に
フォールバック（健全性は不変・liveness のみ劣化）。`Value` を扱う raw `std::thread::spawn` は全て
`spawn_user_thread`（mutator 登録）へ統一（signal watcher / proc-async pump / zip / supply throttle）。
併せて collect_white の buffered-skip が「drain 後〜STW 達成前に再 buffer された cycle メンバーを
White のまま取り残す」stranding を修正（VERIFY が検出・毎 run 55-80 件→0）。
= **サーバ型の join しない worker が生きたまま cycle 回収が可能に**（churn 実測: 98 collects で
412 cycles 回収・pause_max ~50ms）。

残:

- ✅ **既定値 `MUTSU_GC` は on**（2026-07-05 切替済み — ADR-0003 §5 に受け入れ実測とゲート改訂を記録。
      bench-class ~+8% はユーザー判断で許容、根本削減は層 3b=NaN-boxing）。GC キャンペーン（層 3a）完了。
      残る GC 関連 perf は NaN-boxing（3b）に統合。
- **Track B（要素 cell 化）** — スライス 1（atomic ストア要素セル #4241）/ 2（state aggregate
  cell 書き戻し #4245）/ 3（state 集約の全モードセル化 — 非スレッドのクロージャ間共有 raku 一致
  #4251）完了。map/array の「構造」は COW スナップショット、要素「値」のみセル — この分割が
  Track B の一般化テンプレート。残スライス（T4 multidim cas / T5 typed-constraint 透過 /
  T6 非 state escaped aggregate probe）と着手条件・ゲートは
  [docs/gc-post-3a-roadmap.md](docs/gc-post-3a-roadmap.md) §2 に精緻化済み。
- **post-3a ロードマップ**: 層3a hardening（H1 継続計測〜H5 background collect の着手トリガ）・
  層3b NaN-boxing のスライス計画（3b-0 API 壁 → 3b-1 表現スイッチ → 3b-2 交通量刈り）・
  層3c 凍結条件は [docs/gc-post-3a-roadmap.md](docs/gc-post-3a-roadmap.md) 参照。
  続いて JIT（層4）= [ADR-0004（Accepted 2026-07-06）](docs/adr/0004-jit-strategy.md)。
- [ ] **層3a 監査性 sweep（ANALYSIS rev8 §2.1・低コスト・1 PR で済む）**:
      ① `gc/mod.rs`/`gc_ptr.rs`/`collect.rs` の stale な「default off / no production caller yet」
      ヘッダを default-on 後の実態に是正 ② `gc_ptr.rs`/`collect.rs` のモジュール全体
      `#![allow(dead_code)]` 撤去（「step 8 で外す」と書かれたまま・step 8 は完了済みで真の dead
      surface を隠しうる） ③ `get_mut`/`make_mut` の uniqueness（`header.strong == 1`）と candidate
      バッファのカウント外 `Arc` clone が共存できる不変条件（buffered clone は collect safepoint
      でのみ deref・`Gc::drop` は `collecting()` 中 early-return）を散文でなく debug assert に
      ④ `Gc::make_mut` の `header.strong` 手動付け替え（全 `Relaxed`）の並行 clone/drop 競合時
      ordering を検討。

---

## 3. 🔴 substrate — multi-dispatch の tree-walk fallback 除去（残件）

単一ストア化・tree-walking interpreter 撤去・第一級コンテナ・状態所有・multi-dispatch VM 化の
主要キャンペーンは全て完了済み（詳細は [news/2026-06.md](news/2026-06.md)）。substrate として
残るのは multi-dispatch の fallback 除去のみ:

- [ ] default-param OTF の builtin-shadow 単一候補（name-cache 汚染リスクがあるため意図的に除外を維持中）。
- [ ] モジュール sub OTF に残る interpreter 結合構文: `state` / `EVAL` / `EVALFILE` / `start` /
      `CATCH` / `CONTROL` / phaser / ネスト宣言 / subtest / sigilless scalar（`\x`）/ 戻り型 coercion
      （ゲート実体 = `def_is_otf_compilable_module_single`、`vm/vm_call_func_ops.rs`）。
      `is rw`/`is raw` は #4091（rw-arg のコンパイル時 caller slot 化）でゲートから外れた。
      本筋は `compiled_fns` の拡充。
- [ ] `@_` slurpy recursive sub（別カテゴリ）。`@a[1..*]` 再帰の immutable-List bug は §4 扱い。
- 完了済み（計測で確認）: bare multi OTF / `state` 候補・caching-proto body（#4047）/ `code_signature`
  param（#3883）/ capture param。signature alternates の `state` 共有のみ interpreter 境界として
  意図的に残す（news/2026-07.md）。

---

## 4. 🟢 roast backlog（[TODO_roast/BLOCKERS.md](TODO_roast/BLOCKERS.md) 駆動・インパクト順）

現状 whitelist は **1350 / 1463**。診断には `./scripts/roast-history.sh` を使う
（出力は `tmp/roast-{panic,timeout,error,fail,pass}.txt`）。

- [ ] **★lazy-seq（残 ④のみ）**: `.List`/`.Array` の laziness 保持・`eqv` lazy ガード・Seq single-pass
      consumption・reify-on-mutation は解決済み（news 参照）。残: 変異後の is-lazy 保持
      （partial-reify＋lazy tail・`docs/lazy-arrays.md` §4.1）／`push`/`pop` が lazy list で `X::…` を
      投げる raku 挙動／slurpy 真 lazy（値は正・is-lazy が False）。BLOCKERS.md §「Real lazy infinite sequences」。
- [ ] typed-exception 残 gap: strict-mode undeclared 変数検出／class 再宣言の cross-EVAL 検出
      （runtime + compilation-unit 追跡）／X::Redeclaration::Outer（compile-time scope 解析）。
      全て非-trivial で、単体では roast ファイルを whitelist 化しない（`S32-exceptions/misc.t` 系は完了済み）。
- [ ] 複数行 feed: 行頭 `==>` をまたぐ複数行 feed（`parse_list_infix_loop` の
      `!ws_before.contains('\n')` ガードが阻む）。`ff`/`fff`・単一行 feed は完了（news/2026-06.md
      アーカイブ節）。`==>>`/`<<==`・`~<`/`~>` は rakudo 自身が未実装/仕様未確定＝着手不可。
- [ ] メタ演算子: generalized negation meta（`!op`）/ hyper assignment（`@a >>+=>> 1`）。
- [ ] Phasers: rvalue caching（INIT/CHECK/BEGIN as rvalues）/ PRE/POST（contract programming）。
- [ ] Signatures: type-check enforcement（X::TypeCheck）/ native int/uint overflow・bounds / 単一 sub の複数シグネチャ。
- [ ] OOP: namespaced construction（`A::B.new`）/ `augment class` / parameterized role mixin。
- [ ] Supply/Concurrency: backpressure / `supply`・`react` block scoping / Tap management（close, drain）。
- [ ] IO/Process: IO::Handle read modes（binary/encodings）/ Proc・Proc::Async 完全化 / file test operators（`-e`/`-f`/`-d`）。
- [ ] 孤立サブシステム（main-track 非衝突・BLOCKERS.md §A）: 残 regex（S05-substitution/match capturing-contexts）・
      Unicode CollationTest・shaped arrays・Pod。

---

## 5. perf — 実行速度（計測駆動・MUTSU_VM_STATS / timed roast）

method-call ホットパスキャンペーン第 1 弾（#3853/#3857/#3859/#3867/#3870: body-fingerprint 撤去・
resolution cache・mro_readonly キャッシュ・FxHashMap・単一候補 memoize）と、単一ストア化による
per-call env deep clone 撤廃は完了（news/2026-06.md）。残レバー:

- [ ] **symbol intern round-trip の残り（大型リファクタ）**: dispatch hot path の `cn = class_sym.resolve()`
      String 確保（perf の malloc ~17% の一角）が downstream の `&str` API（`call_compiled_method`/
      `push_method_dispatch_frame`/`check_method_wrap_chain`＋ registry の String キー lookup）のため残る。
      `with_str` の長期保持はグローバル symbol-table read-lock デッドロックで不可＝registry/dispatch API を
      Symbol キー化する必要。`Symbol::intern(method)` も毎回 ~3.5%（method 名は CompiledCode 定数の
      `Value::Str`・事前 intern は precomp 直列化に波及）。
- [ ] `Value` clone/drop（bench-class の ~50%）＋ `attributes.to_map()` の毎回クローン ＋
      `call_compiled_method` の属性キー `format!` ＋ instance 構築 HashMap ＋ `merge_method_env` —
      Lever 2（NaN-boxing・GC 後）待ち、ないし属性 materialization の作り直し（深い）。
- [ ] **Lever 2: NaN-boxing = ADR-0001 層3b（JIT の地ならし・GC 後）**: `Value` 48→8 bytes。
      int-arith 2x・fib ~30% 狙い。`value_size_guard` テストでサイズ監視中。スライス計画
      （3b-0 API 壁 → 3b-1 表現スイッチ → 3b-2 交通量刈り）とゲートは
      [docs/gc-post-3a-roadmap.md](docs/gc-post-3a-roadmap.md) §3。
      **3b-0 着手済み**: 壁 API（`ValueView`/`view()`/accessor/constructor、
      [docs/nanbox-3b0-api-wall.md](docs/nanbox-3b0-api-wall.md)）と ratchet
      （`scripts/check-value-wall.sh`・`make test` 組込・baseline 17757）が landed。
      残り = 直接 variant 参照の機械的移行（ディレクトリ単位スライス・並列可）→ 0 で封印 → 3b-1。
- **Lever 3: threaded dispatch — 凍結**（2026-07-06 ユーザー承認・[ADR-0004](docs/adr/0004-jit-strategy.md) §2.5 J0）:
      JIT Tier A が dispatch ループ除去で同じ利得をより大きく取るため二重投資を避ける。
      JIT が頓挫した場合のみ復活。
- [ ] **Lever 4: JIT（Cranelift）= ADR-0001 層4（GC の後）/ Lever 5: 型制約チェックの tight-loop 省略**。
      方式・フェーズ（J1 骨組み → J2 Tier A 網羅 → J3 呼び出し規約/IC → J4 Tier B インライン →
      J5 既定 on）とゲートは **[ADR-0004（Accepted 2026-07-06）](docs/adr/0004-jit-strategy.md)**。
      deopt/stack map なしの subroutine-threading 起点、safepoint poll で GC の STW に協調。
      着手条件 = 層3b（Lever 2）のゲート達成。
- [ ] **Lever 6: biased reference counting = ADR-0001 層3c（GC 後の独立 perf）**。凍結 — 着手トリガは
      「JIT J4 完了後の profile で atomic inc/dec が上位に残る」のみ（gc-post-3a-roadmap §4）。
- [ ] **opcode 残件（[docs/opcode-design-review.md](docs/opcode-design-review.md) §2/§5/§6・#4279 の続き）**:
      ラベル等の inline `Option<String>` payload（`Last`/`Next`/`Redo`/loop 系/`SmartMatchExpr.lhs_var`）
      の定数プール `Option<u32>` 化（`OpCode` を 48B 未満へ） / per-instruction 定数コスト
      （`current_code` 生ポインタ store・`trace_log!` チェック）の計測付き削減 / `Jump(i32)` が
      絶対 index を運ぶ encoding の是正 / per-opcode ヒストグラム駆動での特化 op 統合
      （`ContainerEq`×4・`IndexAssign*`×6 — 美学でなくデータで駆動）。
- [ ] 正規表現: 量指定子反復ごとの `RegexCaptures.clone()` 削減。
- 目標: method-call <1.5x、bench-class <1.5x、bench-fib（型制約付き）<2x。

---

## 6. 並行（Track C 残）・構造リファクタ（独立・中長期）

- [ ] **`state @`/`%`・lexical aggregate の真共有** — Track B スライス 2+3 でほぼ解消:
      state 集約は全モードで `ContainerRef` セル（#4245 write-through/cell 経路、#4251 全モード
      セル化 — 非スレッドの同一 sub 由来クロージャ間共有も raku 一致）。
      pin = t/state-aggregate-shared-cell.t（18 本、real raku でも全 pass）。残:
      高競合の並行「構造」挿入の lost-update（real rakudo は同形で MoarVM oops クラッシュ＝
      言語保証外。mutsu は不壊で優位 — 仕様外のまま維持）と、非 state の escaped aggregate
      probe（gc-post-3a-roadmap §2 T6）。
- [ ] Semaphore / nonblocking await / lock 競合（S17・hard・別軸）。
- [ ] 生ポインタ aliased write の撤廃: 旧 `arc_contents_mut` は dead 化済みで、本番経路は
      `gc::gc_contents_mut` / `Gc::{get,make}_mut` に移動（unsoundness は解消でなく移動 —
      ANALYSIS rev8 §2.1）。完全解消 = §2 Track B 残スライス T4-T6 に統合済み（重複着手しない）。
- [ ] **レキシカルスコープ slot キャンペーン完遂（ANALYSIS §1.4・
      [docs/lexical-scope-slot-campaign.md](docs/lexical-scope-slot-campaign.md)）**: writeback IR への
      slot 焼き込みは部分着手済み（`SmartMatchExpr.lhs_slot`・RMW slot 引数・rw-arg `Pair(name, slot)`）。
      残り = by-name フォールバック（`find_local_slot`）の撤廃 → `MUTSU_SHADOW_SLOTS` 既定 ON
      （シャドウ衝突の実修正） → `BlockScope` の locals 全 clone/restore 撤去。
- [ ] エラー/制御のチャネル分離: bool 群の `enum Control` 統合と `RuntimeError` 縮小
      （cold Box 化・`result_large_err` 23→0）は完了。残る「制御を `Result::Err` で運ぶ」構造自体の
      分離は実害が消えたため優先度低（ANALYSIS rev8 §2.2）。
- [ ] Supply detached worker の panic を QUIT へ伝播（現状は握り潰し・ANALYSIS §5）。
- [ ] `.^methods`/`.can` を実ディスパッチ表から導出 / roast fudge ロジック分離 / 500 行超ファイル分割。
- [ ] **衛生トレンドの棚卸し（ANALYSIS rev8 §5/§6）**: `runtime/mod.rs` の再肥大（1932→2118 行）の
      再スリム化 / GC・Track B churn で増えた `.clone()` 9022（+1322）・`unwrap` 系 1643（+167）・
      `#[allow(` 157（+19）の増加分レビュー。
- [ ] エラーメッセージ品質向上 / エッジケースの panic・crash を 0 に。

---

## メトリクス

| 指標 | 現在 | 目標 |
|------|------|------|
| **同梱ライブラリ（vendored＋ドキュメント付き）** | **0**（動作実績 10+ が t/lib・外部取得のまま） | **10+ を同梱・全てドキュメント付き** |
| mzef | CLI 起動＋dispatch ✅／install→use 橋 ✅ | 実 zef バイナリでの実 install（残 2 バグ＋network fetch） |
| バイナリ配布 | なし | mise / GitHub Releases で単一コマンド導入 |
| Whitelist | **1350**（全 .t 1463 中） | 1300+ ✅ 達成済み・現状維持以上 |
| GC | **default on ✅**（2026-07-05・ADR-0003） | 達成（残 perf は層 3b へ） |
| fib(25) vs raku | **1.0x** | <10x ✅ |
| method-call vs raku | **2.7x** | <1.5x |
| bench-class vs raku | **2.3x** | <1.5x |
| bench-fib（型制約付き）vs raku | **3.2x** | <2x |
| 起動時間 vs raku | **0.04x** | 0.04x ✅ 維持 |
| tree-walk フォールバック（メソッド/関数） | **~1% / ~18.6%（大半 carrier）** | 0%（carrier 除く） |
