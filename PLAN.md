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

- [ ] **残りの型付き例外**（X::Str::Numeric / X::Method::NotFound / X::Undeclared / X::Cannot::Lazy /
      X::EXPORTHOW::InvalidDirective 等）。詳細は BLOCKERS.md "throws-like / Exception Types"。
- [ ] **真の lazy 無限配列**（`my @a = 1..*` の reify-on-demand）。設計＝[docs/lazy-arrays.md](docs/lazy-arrays.md)。
      reify 基盤（scalar/gather coroutine 経由の index pull）は既存。残＝① 無限 Range/`...` 列を `@` 代入時に
      capped Array へ潰さず reify LazyList のまま保持（L2）② mutation chokepoint で reify-on-write（L3・~10箇所・回帰注意）
      ③ lazy `.gist`/`.is-lazy`（L1/L5）④ slurpy 真 lazy 化（L4）。**slurpy `*@`/`+@` 無限 Range ハングは cap で解消済**
      （#3302 予定・docs L4 の暫定）。unblock: slurpy-params.t / slice.t / eqv.t lazy。
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

### ★ 二重ストア統合 ＋ 第一級コンテナ — **Slice F で収束する 1 本のキャンペーン**（2026-06-18 再編）

ユーザー本命。設計＝[docs/vm-single-store.md](docs/vm-single-store.md)（履歴・撤回試行は [docs/vm-dual-store.md](docs/vm-dual-store.md)）+
[docs/container-identity.md](docs/container-identity.md)。

**2026-06-18 の深掘りで判明した収束**: 当初は「二重ストア統合」と「第一級コンテナ」を別キャンペーンとして並べていたが、
**両者は single-store の最終 Slice F（`env_dirty` 削除＝`locals` を真の単一権威化）で同一の壁に収束する**:

> Slice F の真の前提は **`env` と `locals` がコンテナで乖離しないこと**（cell-linked container で env と locals が別 Arc を
> 持たないこと）。これは第一級コンテナ campaign が解いている問題そのもの。ContainerRef cell（Phase 2・`:=` bind は大幅 landed・
> `element-bind-cell.t` 31ケース）は **1 ストア内の COW 跨ぎ生存**を解決済みだが、**dual-store の env↔locals 乖離は別レイヤ**で残る。
> pairs/slip carrier-drop が `element-bind-cell` を壊すのは正にこの dual-store 乖離（cell があっても carrier が env_dirty を
> 落とすと locals 側コンテナが stale 経路で読まれる）。**∴ blanket 削除/carrier-drop の小細工では Slice F に到達不可。**

**現状（blanket-removal / carrier-drop 路線は価値枯渇）**:
- [x] **Slice A** — invariant guard + 計測（#3219）。
- [x] **Slice B** — `EVAL` 精密 writeback（#3222）+ EVAL carrier env_dirty drop（#3227）。
- [~] **Slice C** — R1 reverse write-through。**実測で SUPERSEDED**（our/dynamic/push/call は既に pull 1–2・effective=0）。
- [~] **Slice C′** — open-q#2 の regex 経路 + bareword carrier 一般化（#3231）+ EVAL内 `$x ~~ s///` writeback 穴修正（#3237）。
      `pairs`/`slip` 一般化は下記収束（env↔locals コンテナ coherence）が前提＝延期。
- [x] **Slice D** — R3 blanket-mark 撲滅 = **監査で完了確認（#3238）**。安全に削除できる冗長 blanket は無し（残る ≈140 の
      `env_dirty=true` は精密ゲート本体／正当 mutation／carrier-block net のいずれか・spurious は全ベンチ 1〜8 pull）。

**ここから先（収束点へ向かう・残り 1 本柱）**:
- [x] **Slice E — closure upvalue 化 = 完遂（Part1 #3245 + Part2 #3247, 2026-06-18）**: closure を whole-env でなく
      **upvalue snapshot**（free vars + `__mutsu_*` meta + system 名）で捕捉し、`compute_needs_env_sync` の closure flush
      （branch #2）を撤去＝closure を forward-flush 機構から完全分離。**実測で判明: Slice E は perf 中立**（closure 毎の
      env deep copy は GLOBAL_BASE 退避 + chain-aware `entry_or_insert` で既に ~1–2 entries）＝目的はアーキ/保守性。
      **素朴な「free-var だけ捕捉」は不可と実証**: `free_var_syms`（GetGlobal scan）が ①専用 op の system 名（`self`/属性・
      `$_`・`$?FILE`・dynamic var）②`stmt_pool` に隠れる nested body（`whenever`/`gather`）を取りこぼす → system 名 keep-rule
      （`env::is_plain_user_lexical`）+ 不完全フレーム whole-env fallback（`captures_env_by_name` cc flag・`WheneverScope` 追加）
      が正攻法。pin=`t/single-store-slice-e.t`。詳細＝docs/vm-single-store.md Slice E 節。
- [ ] **第一級コンテナ Phase 2 残 → Phase 3（Slice F の substrate・残る唯一の前提）**: env↔locals がコンテナ Arc を共有する終状態へ。
      Phase 3（instance cell）は registry 撤去・CAS まで完遂、Phase 2（要素 cell）も element-element/LHS/cyclic 束縛まで landed
      （nested.t 43/43・#2922/#2925）。**残る具体作業**（実装台帳 docs/container-identity.md「残スライス」＋ docs/slotref-removal-plan.md）:
      - **残 `HashSlotRef` 生成サイトの cell 化**: `hash_autovivify`（junction-key bind / `is raw` reduce lvalue-read / autoviv-op
        fallback）— 呼び出し後に entry が存在＝phantom 不要なので cell 化可（hot path 非該当）。deferred-token 用途以外の `HashSlotRef` を枯らす。
      - **grep-rw-view 撤去**: 最後の ptr-keyed グローバルの一つ。matched 要素を cell 昇格し view registry を全廃（for-loop rw topic が消費者）。
      - [~] **env↔locals コンテナ coherence の本丸（Slice F の真の前提）**: cell-linked container が env と locals で別 Arc を持つ
        dual-store 乖離を解消（pairs/slip carrier-drop が `element-bind-cell.t` を壊すのが証左）。**設計済＝[docs/env-locals-coherence.md](docs/env-locals-coherence.md)**
        （推奨＝outer コンテナを env でも `ContainerRef` cell として持ち env↔locals が同 cell 共有＝instance attr Phase 3 と同型。
        escape-aware で perf 崖回避。Stage 0 チョークポイント→Stage 1 昇格→Stage 2 計測→Stage 3 Slice F+pairs/slip 一般化）。
        進捗（2026-06-18）:
        - [x] **Stage 0**（read/write チョークポイント棚卸し）= read は `into_deref` で単一化済みと確認、write gap の bind バグ
              （`our %g:=%h` #3252・bound hash whole-reassign #3255・constant 要素書込 + closure whole-reassign #3256）を補完。
        - [x] **Stage 1**（escape-aware outer cell 化・bound container write チョークポイント）= 完了。audit（#3258）で
              blast radius を ~14 write サイトに特定 → **for-rw writeback "site A" を array（#3259）/ hash（#3260）化** →
              監査で「残る生 match の write ギャップは junction だけ」と確定 → **junction + from-end `@a[*-1]` 代入（#3279）/
              `%h.push`・`.append`（#3281）を bound-cell-aware 化**。bound-container 操作（nested/deep/reference push・splice・
              whole-slice）は全 raku 一致。
        - [x] **bug ②（scalar-array 参照共有）= 完了（Slice 2a–2d 全着地）**: `my $n=@z` の push/要素伝播（2a #3264 +
              chained #3267）→ `@aoa[i]=@row`/`%h<k>=@row`（2b #3274）→ `my @a:=@$n`/`my %h:=%$m`（2c #3268/#3277）→
              **headline `sub f($n){ my @a:=@$n; @a.push }; f(@z)`（2d #3283）**。2d は call 境界で **`@`/`%` 変数を `$`-param に
              渡すとき共有 `ContainerRef` cell へ昇格**（binding.rs）+ slot-only fast path を gate で slow path へ迂回
              （詳細・follow-up = [docs/scalar-array-sharing.md](docs/scalar-array-sharing.md) §5 Slice 2d）。
              - **残 follow-up（pre-existing・2d では未カバー）**: ① **method param**（`method m($n){ $n.push }`）=
                `vm_method_dispatch.rs:79` の fast-path gate に scalar-container-share 条件を追加（slow path は同 :424 で
                bind_function_args_values を使う→promotion 適用。注意=invocant の index alignment）。② **`is copy` $-param**
                （rebind 許可と cell 共有の両立）。
      - Phase 0.5 第2段（任意・実挙動変化）: `GetArrayVar`/`Index` の auto-decont + 新 lvalue opcode の本配線。
      - Phase 3 機会的残: 属性束縛（`$!x :=` / per-attribute container template、S03-binding/attributes・S14-traits 5-8）。
- [ ] **Slice F（収束点）** — coarse 機構削除（`env_dirty`/`ensure_locals_synced`/`sync_locals_from_env`/`saved_env_dirty`）。
      **前提 = Slice E（upvalue）+ 第一級コンテナ Phase 2/3（env↔locals コンテナ coherence）の両方。** ここで初めて pairs/slip
      carrier-drop も安全化し、`locals` が単一権威・`env` は派生ビューになる。
- [ ] **Slice G**（後続・任意）— env の on-demand materialization（per-call `clone_env` を lazy overlay 化）。

速度の担保（設計内蔵）: (a) escape 解析でセル省略（捕捉/エイリアス/`.VAR` されないローカルは裸値）、(b) 配列は COW、
(c) decont は単一分岐、(d) 中期の NaN-boxing で payload 8byte 化。各 Phase は重量級 roast を timed 確認（#2746 の教訓: perf 回帰は
`make test` で検出不能、CI release roast の timeout で初めて顕在化）。

---

## メトリクス

| 指標 | 現在 | 目標 |
|------|------|------|
| Whitelist | **1282** | 1300+ |
| fib(25) vs raku | **1.0x** | <10x |
| method-call vs raku | **2.7x** | <1.5x |
| bench-class vs raku | **2.3x** | <1.5x |
| bench-fib（型制約付き）vs raku | — | <2x |
| 起動時間 vs raku | **0.04x** | 0.04x |
| 動作モジュール数 | 1 | 5+（ウェブブログスタック） |
| Template::Mustache / HTTP::Server::Tiny | ❌ | ✅ |
