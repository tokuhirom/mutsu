# PLAN.md — mutsu の実装計画

> このファイルには **未完了の作業だけ** を載せる。完了した作業は [news/](news/) に移す。
> 過去ログは [news/](news/)、性能の詳細は [PERFORMANCE.md](PERFORMANCE.md)、
> roast の失敗分析は [TODO_roast/BLOCKERS.md](TODO_roast/BLOCKERS.md) を参照。
>
> **最終更新 2026-07-12**:
> 完了済み記述を一掃して残タスクだけに再スリム化（完了分の正本は
> [news/2026-07.md](news/2026-07.md)）。ゴール「**Battery included な Raku 処理系**」の
> 再定義と全面再構成は 2026-07-05（旧完了記述は [news/2026-06.md](news/2026-06.md)
> アーカイブ節と news/2026-07.md）。

## ゴール — Battery included な Raku 処理系

**mutsu をインストールするだけで、ドキュメントが整備された標準添付ライブラリが揃い、
すぐに実用コードが書ける Raku 言語インタープリタ**を作る。

bun が JavaScript に対して取ったポジショニング（ランタイム＋パッケージマネージャ＋標準ツールを
単一の高速なバイナリで提供）の Raku 版。公式 Rakudo エコシステムには battery-included な
ディストリビューションが存在せず、そこが mutsu の独自の立ち位置になる。構成要素は 4 つ:

1. **高速起動の互換インタープリタ** — 起動 raku 比 0.04x・roast whitelist 1383。CLI ツールと
   スクリプト実行を主戦場に、raku 互換性を維持・拡大する。→ §3 / §4
2. **標準添付ライブラリ（batteries）** — JSON / HTTP / テンプレート / DB / ファイルユーティリティ等を
   同梱し、インストール直後に `use` するだけで動く。**各ライブラリにドキュメントを付ける**。→ §1
3. **同梱パッケージマネージャ `mzef`**（実 Zef を vendoring）— 標準添付で足りないものは fez
   エコシステムから取得できる。→ §1 B2
4. **土台品質** — GC（table stakes・**完了・既定 on**）・性能・エラーメッセージ。→ §2 / §5

## この文書の読み方

- **§1 Batteries** がゴール直結の本丸: 同梱ライブラリの選定・vendoring・ドキュメント・mzef・配布。
- **§2 Phase B** は層3a（GC）・層3b（NaN-boxing）完了、層4 JIT は J1（骨組み #4471）・
  J2（Tier A 網羅 + 全 call 経路 + jit-stress CI #4474）完了 —
  次スライスは J3（呼び出し規約/inline cache・ADR-0004）。
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
| **B. Value 表現リワーク＋GC** | 層3a（Track B＋cycle collector 統合）・層3b NaN-boxing **共に完了**（2026-07-12 #4469・`Value` 48→8B） | §2 |
| **C. JIT** | 独自メリット（**いまここ** — J1 #4471・J2 #4474 完了・次は J3） | §5 Lever 4 |

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

- [ ] **実 zef バイナリの end-to-end 実行を阻むブロッカー（2026-07-12 セッションで大幅前進）**:
      旧 2 バグは解消済み（(a) %-sigil Associative bind = #4452 / (b) パーサエラー = 再現せず）。
      同日 landed: #4457（classify pair-iteration / hash-init contained-Pair / IO::Path.child 連結）・
      #4460（grammar token 静的 fold — `REQUIRE.parse` が raku 比 ~70x → **1.1x**）・
      #4462（`Version.parts/.plus/.whatever` — 候補 version 照合の真因）・
      **#4466（★旧最大ブロッカー根治: worker thread 上の shared 配列で append/prepend/pop/shift/
      splice が `__mutsu_atomic_arr::` store を迂回し黙って喪失 — 「%-hash 属性 push 喪失」の
      真因は populate の `append @short-names-to-index` 全滅だった。news/2026-07.md 参照）**。
      → **`zef info Zef` が未改変 upstream・hyper 有効・GC 既定 on・実 fez index (7648 dists) で
      Identity/Provides/Depends 出力まで完全動作**（release 2 runs 安定・pin t/hyper-array-mutators.t）。
      現フロンティア:
      1. populate 性能: fold 後 release で fez+rea 全 populate ~3-5 分（raku は数秒）。
         残= plain Named subrule 呼び出しの per-call コスト + Distribution/Identity 構築。
      2. ネスト `.raku` 表示: コレクション内の Instance が `Sp()`（type object 風）に描画される
         （`(C.new,).raku` → raku は `(C.new(...),)`）。実体は正常（semantic には無害・表示のみ）。
      3. `zef list --installed` は exit 0・出力なしまで動作（mutsu 側 site repo が空なら妥当）。
      4. index 名数の微差: fez 7648 metas で raku 9259 keys / mutsu 9256（3 件・name-fail 2-3 dists）。
      5. （監視）旧観測「GC on だと 2 個目の Ecosystems で `$!name` 空読み」は #4466 後の release
         2 runs で再現せず。再発したら GC×thread の状態破壊として独立調査。
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

## 2. ★ Phase B: GC → NaN-boxing → JIT（層3a・層3b 完了 — 次は層4 JIT）

**GC（cycle collector on Arc・層3a）は完了・既定 on**（2026-07-05 ADR-0003）。
**NaN-boxing（層3b・= §5 Lever 2）も完了**（2026-07-12 #4467 B-guards / #4469 B-flip・
`size_of::<Value>()` 48→8B・GC カウンタ main 一致・全ベンチ 5〜9% 高速化＝ゲート達成）。
経緯と詳細は [news/2026-07.md](news/2026-07.md)。残:

- [ ] **層4 JIT（Cranelift・= §5 Lever 4）**: **J1 骨組み（#4471）・J2 Tier A 網羅（#4474）
      完了（2026-07-12・詳細 = news/2026-07.md）** — 全 6 call 経路にエントリフック・
      generic step shim で straight-line opcode ~60 種対応・jit-stress CI green・
      t/ + roast 全 whitelist が JIT on で green。実測: fib +7.3% / bench-fib +4.2%
      （= dispatch 除去分）、method-call/bench-class は flat（hot opcode が generic step
      経由で per-op dispatch が残るため — その除去が J3/J4 の本体）。残フェーズ =
      **J3 呼び出し規約/IC（JIT→JIT 直接呼び出し・method dispatch inline cache・
      型制約チェックの JIT 内 fast path。CallFunc/CallMethod の専用 shim 化から入るのが素直）**
      → J4 Tier B インライン（int-arith 級の top-level/compound ループはここの hot-loop entry 圏）→
      J5 既定 on = [ADR-0004（Accepted 2026-07-06）](docs/adr/0004-jit-strategy.md)。
- [ ] **3b-2 traffic pruning**（[docs/gc-post-3a-roadmap.md](docs/gc-post-3a-roadmap.md) §3.3）:
      clone/drop が 8B copy になった今、無駄な clone 自体の削減（`.clone()` 9022 の棚卸しと重なる）。
      JIT より優先度低・profile 駆動で。
- [ ] 層3a hardening（H1 継続計測〜H5 background collect の着手トリガ）=
      [docs/gc-post-3a-roadmap.md](docs/gc-post-3a-roadmap.md) 参照。grammar パースの候補 push
      自体（~510k/200-parse）の抑制は未着手（実害＝メモリ保持は `Weak` 化で解消済み）。
- 層3c biased refcount = 凍結（着手トリガは gc-post-3a-roadmap §4）。
  層4 JIT = [ADR-0004（Accepted 2026-07-06）](docs/adr/0004-jit-strategy.md)・
  着手条件 = 層3b ゲート達成（§5 Lever 4）。

---

## 3. 🔴 substrate — multi-dispatch の tree-walk fallback 除去（残件）

主要キャンペーン（単一ストア化・tree-walk interpreter 撤去・第一級コンテナ・状態所有・
multi-dispatch VM 化・**モジュール sub OTF ゲート緩和 #4427→#4429→#4431→#4437**）は完了済み
（[news/2026-06.md](news/2026-06.md) / [news/2026-07.md](news/2026-07.md)）。ゲート実体 =
`def_is_otf_compilable_module_single`（`vm/vm_call_func_ops.rs`）。「ゲートを外して実験すれば
済む」フロンティアは枯渇し、残タスクはいずれも**機構実装**が必要:

- [ ] **`start` の OTF 化 = per-call 捕捉セル化**: 再帰 sub の start クロージャが param を捕捉すると
      再帰呼び出しの re-bind が捕捉値を clobber するため全体除外中
      （退行 pin = `t/start-block-return-value.t` test 3・不可の実証と経緯 = news/2026-07.md）。
- [ ] **sigilless scalar（`\x`）param の OTF 化**: raw エイリアスの EVAL 越し caller writeback に
      #4091（`is rw` compile-time caller slot）相当の機構が必要
      （FAIL pin = `t/sigilless-params.t`「sigilless aliases are writable through EVAL calls」）。
- [ ] **`once` の cross-thread 重複発火（tree-walk でも同挙動の実バグ）**: `once_values` が thread
      クローン時に値コピーされ各 thread で再発火（raku は全体 1 回）。修正 = `once_values` の
      共有セル化（#4312 の state セルと同型パターン）。
- 意図的除外（やらない判断済み）: default-param builtin-shadow 単一候補（name-cache 汚染リスク・
  ユーザー方針）/ `is encoded(...)`（NativeCall・実害ゼロ）/ signature alternates の `state` 共有
  （interpreter 境界として残す）。

---

## 4. 🟢 roast backlog（[TODO_roast/BLOCKERS.md](TODO_roast/BLOCKERS.md) 駆動・インパクト順）

現状 whitelist は **1383 / 1463**（2026-07-12）。診断には `./scripts/roast-history.sh` を使う
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

- [ ] `Value` clone/drop ＋ `attributes.to_map()` の毎回クローン ＋
      `call_compiled_method` の属性キー `format!` ＋ instance 構築 HashMap —
      Lever 2（NaN-boxing・GC 後）待ち、ないし属性 materialization の作り直し（深い）。
      **2026-07-12 スライス実績**: ① for ループ topic writeback O(n²) 修正（#4447 —
      旧記述「bench-class の ~50% が Value clone/drop」の実体）② native ctor plan の
      per-class キャッシュ＋`is default` ループのゲート（#4451）③ `merge_method_env` の
      `method_local_keys` HashSet を allocation-free 述語化 — 合計で bench-class
      1.06s→0.23s（raku 比 2.3x→**1.15x** ✅）・method-call 2.7x→**1.31x** ✅。
      残: `dispatch_compiled_method` 入口の `to_map()` 除去（中規模リファクタ）と
      attrs の `HashMap<String,Value>`/SipHash 起因の malloc 群（＝作り直し本体）。
- **Lever 2: NaN-boxing = ADR-0001 層3b — 完了**（2026-07-12 #4467/#4469・`Value` 48→8B・
      全ベンチ 5〜9% 高速化・詳細 = §2 / news/2026-07.md）。
- **Lever 3: threaded dispatch — 凍結**（2026-07-06 ユーザー承認・[ADR-0004](docs/adr/0004-jit-strategy.md) §2.5 J0）:
      JIT Tier A が dispatch ループ除去で同じ利得をより大きく取るため二重投資を避ける。
      JIT が頓挫した場合のみ復活。
- [ ] **Lever 4: JIT（Cranelift）= ADR-0001 層4 — J1（#4471）・J2（#4474）完了、
      次 = J3 呼び出し規約/IC** / Lever 5: 型制約チェックの tight-loop 省略。
      残フェーズ（J2 → J3 呼び出し規約/IC → J4 Tier B インライン → J5 既定 on）とゲートは
      **[ADR-0004（Accepted 2026-07-06）](docs/adr/0004-jit-strategy.md)**（詳細 = §2 / news/2026-07.md）。
- [ ] **Lever 6: biased reference counting = ADR-0001 層3c（GC 後の独立 perf）**。凍結 — 着手トリガは
      「JIT J4 完了後の profile で atomic inc/dec が上位に残る」のみ（gc-post-3a-roadmap §4）。
- [ ] **opcode 残件（[docs/opcode-design-review.md](docs/opcode-design-review.md) §2/§5/§6・#4279 の続き）**:
      ラベル等の inline `Option<String>` payload（`Last`/`Next`/`Redo`/loop 系/`SmartMatchExpr.lhs_var`）
      の定数プール `Option<u32>` 化（`OpCode` を 48B 未満へ） / per-instruction 定数コスト
      （`current_code` 生ポインタ store・`trace_log!` チェック）の計測付き削減 / `Jump(i32)` が
      絶対 index を運ぶ encoding の是正 / per-opcode ヒストグラム駆動での特化 op 統合
      （`ContainerEq`×4・`IndexAssign*`×6 — 美学でなくデータで駆動）。
- [ ] 正規表現: 量指定子反復ごとの `RegexCaptures.clone()` 削減。
- 目標: method-call <1.5x（✅ 1.14x・2026-07-12）、bench-class <1.5x（✅ 0.93x・2026-07-12）、
  fib <10x（✅ **0.91x** — ?LINE-field＋empty-overlay-tier 修正で絶対 2.3x 回帰ごと解消・
  経緯は PERFORMANCE.md「RESOLVED」節）、bench-fib（型制約付き）<2x
  （✅ **1.92x** — `<=`/`>`/`>=` の Int/Num fast path で達成・2026-07-12）。

---

## 6. 並行（Track C 残）・構造リファクタ（独立・中長期）

- [ ] **state/lexical aggregate 真共有の残**: 高競合の並行「構造」挿入の lost-update のみ
      （real rakudo は同形で MoarVM oops クラッシュ＝言語保証外。mutsu は不壊で優位 —
      仕様外のまま維持＝実質やらない判断）。セル化本体は Track B スライス 2+3・T6 で完了
      （news/2026-07.md・pin = t/state-aggregate-shared-cell.t 18 本）。
- [ ] Semaphore / nonblocking await / lock 競合（S17・hard・別軸）。
- [ ] **再帰 start/await sub を 2 連続実行すると 2 つ目がハング（2026-07-11 発見・main ef5cd62e で再現）**:
      `sub f($n){ start { $n<=0 ?? "b" !! await(f($n-1)) ~ "|$n" } }; await(f(3));` を実行した後に
      2 分岐再帰（`await(fib($n-2)) + await(fib($n-1))`）の start sub を await すると決定的にハング
      （fib 単独・単発再帰 2 連続は OK）。先行 start チェーンが thread-pool worker を解放しない疑い。
      raku は両方 4/8 を返す。
- [ ] 生ポインタ aliased write の撤廃: 旧 `arc_contents_mut` は dead 化済みで、本番経路は
      `gc::gc_contents_mut` / `Gc::{get,make}_mut` に移動（unsoundness は解消でなく移動 —
      ANALYSIS rev8 §2.1）。Track B T4–T6 完了（news/2026-07.md）を受けて残実態の棚卸しから。
- [ ] **`BlockScope` の locals 全 clone/restore 撤去**（レキシカルスコープ slot キャンペーン
      [docs/lexical-scope-slot-campaign.md](docs/lexical-scope-slot-campaign.md) の最終手・perf 本丸）:
      `exec_block_scope_op` の `self.locals.clone()` を撤去する。`$OUTER::` 実行時 snapshot・
      GC roots・env 再同期と絡む load-bearing refactor で専用セッション向き。
      前段（S1–S17 slot 焼き込み＋shadow-slot 既定 ON 化）は完了済み（news/2026-07.md）。
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
| Whitelist | **1373**（全 .t 1463 中） | 1300+ ✅ 達成済み・現状維持以上 |
| GC | **default on ✅**（2026-07-05・ADR-0003） | 達成（残 perf は層 3b へ） |
| fib(25) vs raku | **0.91x**（2026-07-12・?LINE/overlay 修正で回帰解消） | <10x ✅ |
| method-call vs raku | **1.14x**（2026-07-12） | <1.5x ✅ |
| bench-class vs raku | **0.93x**（2026-07-12） | <1.5x ✅ |
| bench-fib（型制約付き）vs raku | **1.92x**（2026-07-12・`<=`/`>`/`>=` Int fast path） | <2x ✅ |
| 起動時間 vs raku | **0.04x** | 0.04x ✅ 維持 |
| tree-walk フォールバック（メソッド/関数） | **~1% / ~18.6%（大半 carrier）** | 0%（carrier 除く） |
