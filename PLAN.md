# PLAN.md — mutsu 今後の実装計画

> このファイルは**これからやる作業だけ**を載せる。完了したものは [news/](news/)（月別）へ移す。
> 過去の実装状況は [news/](news/)、パフォーマンス詳細は [PERFORMANCE.md](PERFORMANCE.md)、
> roast ブロッカー分析は [TODO_roast/BLOCKERS.md](TODO_roast/BLOCKERS.md) を参照。

## 方針

**実用的な Raku インタープリタ**として使ってもらえる品質を目指す。
起動 25 倍速い強みを活かし、CLI ツール・スクリプティング用途をメインターゲットとし、
最終的には **mutsu でウェブブログが作れる** レベルのライブラリ互換性を実現する。

### 🚫 標準ルール: 「1 操作 = 1 実装」を崩さない（ユーザー方針 2026-06-07）

VM decoupling 完結（下記）で実行エンジンは単一 struct `Interpreter`（＝ bytecode VM）に一本化された。
今後も同じ Raku 操作（builtin 関数・メソッド・演算子・coercion）を**二度書かない**:

1. 新規実装・修正は VM/native 層（`src/vm/` ＋ pure native `src/builtins/`）に **1 回だけ**書く。
2. carrier（EVAL / 正規表現の埋め込み `{}` ブロック等）が同じ処理を要するときは、単一 native 実装へ**委譲**する（再実装しない）。
3. 重複を見つけたら native を authoritative にして重複コピーを削除する。
4. レビュー観点: PR が既存 native と重なるロジックを足していないか確認する。

### ✅ 完了した大型キャンペーン（詳細は news/、ここには残さない）

- **VM decoupling / tree-walking Interpreter 撤去（CP-1/CP-2/CP-3, #3075〜#3104）** — 完結。`struct VM` は
  `Interpreter` へ溶け、単一 struct が bytecode VM そのもの。全状態 VM 所有、`env_dirty` dual-store のみ内部最適化として存続。
- **重複実装カタログの消化（dedup A/B/C・レバー A/B/C）** — 完了。
- **第一級コンテナ 戦略フェーズ**（escape 解析 #2758・Phase 0/1・型メタの Arc-ptr keying 全廃 #2952〜#2985）— 完了。
- **無限 Range クラッシュ撲滅 + 遅延リスト pull モデル統一**、**panic→`X::` 変換境界**、**roast 90% 突破（1218）** — 完了。

---

## 🟢 並列実装可能（独立・互いにブロックしない）

> 各項目は別ブランチで並行に進められる（critical path を共有しない）。着手時に該当 BLOCKERS/メモリを確認。

### A. Track C — 並行（共有セル）

スライス 1〜5 landed（共有スカラ / state / compound-assign / hash-elem / array-elem index 代入 #3063）。
スカラ `$counter`・`state $n` の `start` 間ライブ共有は **landed・実機確認済み**（mutsu 4/3＝raku 一致、ANALYSIS rev2 §2.2/§8.3）。残り:

- [ ] **`state @`/`%`・lexical aggregate の真共有** — `start` 間で配列/ハッシュ集約変数を共有。Track B 要素セル基盤に依存。
- [ ] **`unsafe` の single-thread 前提コメント是正**（ANALYSIS rev2 §2.3）— `Arc::as_ptr as *mut` のエイリアス書き換えは
      11→4 箇所に減り `strong_count` ガード＋`Arc::make_mut` フォールバックで UB の実発生は回避済み。だが SAFETY コメントは
      "mutsu is single-threaded" のまま陳腐化（`vm/vm_var_assign_ops.rs:2098,2559,2587,2908,3514`）。コメントを実態（strong_count
      ガード前提）に是正し、最終的には配列/ハッシュ要素も `ContainerRef` 化して生ポインタ改竄を撤廃（Phase 2 依存）。

### B. perf — 起動 / 実行速度（計測駆動・MUTSU_VM_STATS / timed roast）

- [ ] 正規表現: 量指定子反復ごとの `RegexCaptures.clone()` 削減（コンパイルキャッシュ #3064 / 単一マッチ早期終了 #3065 は landed）。
- [ ] closure captures as indexed slots（env サイズ自体の削減。method-call/​bench-class の env deep clone ~9μs/call が残ボトルネック）。
- [ ] **NaN-boxing**: `Value` を 72→8 bytes に（Int/Num/Bool/Nil）。セルも安価化。
- [ ] JIT compilation (Cranelift) の検討 / Cycle collector（循環参照）。
- 目標: method-call <1.5x、bench-class <1.5x、bench-fib（型制約付き）<2x。

### C. roast backlog（BLOCKERS.md 駆動・インパクト順）

- [ ] **【クラッシュ】無限 Range の `.Supply` 化が worker thread で abort**（ANALYSIS rev2 §8.7）—
      `my $s = (1..Inf).Supply; $s.tap({...})` が `capacity overflow` でプロセス abort（exit 0 に偽装）。二重の穴:
      ① `builtins/methods_0arg/coercion.rs:536-537` の Supply coercion が無限 Range を `i64::MAX` 無ガードで
      `(a..=b).map(Value::Int).collect()`（§8.2 の掃討漏れ）→ `materialize_capped` 経由へ。
      ② `.tap`/`start{}` のワーカースレッド本体に panic→`X::` 変換境界が無い（#3045 はメインスレッドのみ）→
      worker の panic を親 Promise/Supply の失敗として伝播させる。
- [ ] **残りの型付き例外**（X::Str::Numeric / X::Method::NotFound / X::Undeclared / X::Cannot::Lazy /
      X::EXPORTHOW::InvalidDirective 等）。詳細は BLOCKERS.md "throws-like / Exception Types"。
- [ ] **Match キャプチャ番号付け / コンテナ kind**: (1) `$<x>=(...)` が positional スロットにも重複格納され番号がずれる
      （`/$<x>=(\w)(\d)/`）、(2) `m:g//` を `my @m` 代入後 `@m.gist` が `(…)` を返す（receiver の List-kind dual-store ナンス）。
- [ ] Lookbehind assertions (`<!after>`) / `:Perl5` modifier edge cases。
- [ ] 未実装演算子: `ff`/`fff`（flipflop 8 variants）/ `==>`・`<==`（feed の precedence 残: `==>` が `=` より強く結合する差）/
      `~<`・`~>`（string bitwise shift・優先度低）。
- [ ] メタ演算子: generalized negation meta (`!op`) / hyper assignment (`@a >>+=>> 1`)。
- [ ] Phasers: rvalue caching (INIT/CHECK/BEGIN as rvalues) / PRE/POST (contract programming)。
- [ ] Signatures: type-check enforcement (X::TypeCheck) / native int/uint overflow・bounds / 単一 sub の複数シグネチャ。
- [ ] OOP: namespaced construction (`A::B.new`) / `augment class`（新規属性追加）/ parameterized role mixin。
- [ ] Supply/Concurrency: backpressure / `supply`・`react` block scoping / Tap management (close, drain)。
- [ ] IO/Process: IO::Handle read modes (binary/encodings) / Proc・Proc::Async 完全化 / file test operators (`-e`/`-f`/`-d`)。

### D. 第一級コンテナ — 機会的バックログ（戦略フェーズ完了済・単発で直す）

特定 whitelist 候補が残り 1–2 subtest でその原因がピンポイントで該当バグのときだけ着手。実装台帳 = [docs/container-identity.md](docs/container-identity.md)。

- [ ] **Track B 残 niche**（詳細 = メモリ `project_track_b_phase2_element_cells`）: ① `for %h<k>.values{$_*=2}` の element-source
      rw writeback（最ホット path・多機構絡み高リスク）、② `.push(@var)` 参照格納（COW detach するので cell 昇格が必要・hot push path）。
- [~] **object-hash（`%h{KeyType}`）残**: `S09-hashes/objecthash.t`（非 whitelist）の ① `%h{Any}` の Mu キー拒否（根は
      `Mu.new` が `Any` インスタンス生成→smartmatch 波及・高リスク）、② `Hash.push` 型チェック、③ list→hash flatten の itemization 追跡。
- [ ] **属性セル + 属性束縛 = Phase 3 の機会的部分**（`$!x :=` / per-attribute container template、S03-binding/attributes・S14-traits/attributes 5-8）。
      ※ 本格 Phase 3 は下記「順序依存」参照。

### E. モジュール互換（Q3 — ウェブブログスタック）

目標: **mutsu でウェブブログシステムが構築できる**。

- [ ] **Template::Mustache** — grammar action dispatch（proto regex in alternation の action 呼び出し）がブロッカー。`.meta` 等を修正。
- [ ] **HTTP::Server::Tiny** の依存（HTTP::Parser / IO::Blob / HTTP::Status）→ 本体。
- [ ] DB アクセス — pure Raku 簡易実装 or qqx ベースの SQLite wrapper（NativeCall 不可）。
- [ ] File::Temp / MIME::Base64 (pure Raku) / File::Directory::Tree。
- [ ] バイナリ配布: mise GitHub バックエンドのインストール検証 / GitHub Releases 自動化。

### F. 構造リファクタ（独立・中長期）

- [ ] 制御フロー（`return`/`last`/`next`/`take`/`emit`）を `RuntimeError` god-struct から `enum Control` へ分離
      （ANALYSIS §2.4・`result_large_err` 負債の解消）。
- [ ] `.^methods`/`.can` の型別メソッド一覧を実ディスパッチ表から導出（ANALYSIS §4）。
- [ ] roast fudge ロジックを核から分離 / テスト一時ファイルを `tmp/` へ / 500 行超ファイルの分割（ANALYSIS §6）。
- [ ] エラーメッセージの品質向上 / エッジケースの panic・crash を 0 に。

### G. Practicality（将来）

- [ ] REPL / Debugger / `zef` package manager compatibility / native binary output。
- [ ] 「mutsu でウェブブログを作る」チュートリアル / raku 互換性マトリクス公開 / WASM playground 公開。

---

## 🔴 順序依存・並列不可（前提あり）

> 内部に着手順序があり、前段が終わるまで後段に着手できないもの。

### 第一級コンテナ Phase 2 → Phase 3（要素セル → 属性セル）

最ホットな表現に触るので段階導入（big-bang 回帰を避ける）。設計の鍵・撤回した試行は [docs/container-identity.md](docs/container-identity.md)。

- [ ] **Phase 0.5 第2段（実挙動変化）**: `GetArrayVar`/`Index` の auto-decont（＝値スタック「常に decont 済み」不変条件）+
      新 lvalue opcode（`GetLocalContainer`/`IndexContainer` 等）の本配線。push 内容が変わるため **Phase 2 と同一 PR** で入れる。
- [ ] **Phase 2 — 配列/ハッシュ要素のコンテナ化（COW セル）**: take-rw（gather.t 38）/ `@a[0] :=` / 深い `>>++`・`deepmap(++*)`
      （hyper.t 330-333）/ object-hash / S12 accessors・instance。`HashSlotRef`/`ArraySlotRef` の場当たり + name-based writeback reconcile を削除。
- [ ] **Phase 3 — 属性コンテナ + 属性束縛**: `$!x :=` / per-attribute container template（S03-binding/attributes・S14-traits/attributes 5-8）。

速度の担保（設計内蔵）: (a) escape 解析でセル省略（捕捉/エイリアス/`.VAR` されないローカルは裸値）、(b) 配列は COW、
(c) decont は単一分岐、(d) 中期の NaN-boxing で payload 8byte 化。各 Phase は重量級 roast を timed 確認（#2746 の教訓: perf 回帰は
`make test` で検出不能、CI release roast の timeout で初めて顕在化）。

---

## メトリクス

| 指標 | 現在 | 目標 |
|------|------|------|
| Whitelist | **1270** | 1280+ |
| fib(25) vs raku | **1.0x** | <10x |
| method-call vs raku | **2.7x** | <1.5x |
| bench-class vs raku | **2.3x** | <1.5x |
| bench-fib（型制約付き）vs raku | — | <2x |
| 起動時間 vs raku | **0.04x** | 0.04x |
| 動作モジュール数 | 1 | 5+（ウェブブログスタック） |
| Template::Mustache / HTTP::Server::Tiny | ❌ | ✅ |
