# ADR-0001: GC 導入の方式選定とフェーズ計画

- **Status**: Accepted（2026-06-27 — レベル1採用。収集方式 §4.2・A' 範囲 §4.3 は未決として残す）
- **Date**: 2026-06-27
- **Deciders**: tokuhirom, Claude
- **関連**: [ANALYSIS.md](../../ANALYSIS.md) §2.1 / §1.3 / §5、[PLAN.md](../../PLAN.md) §G(perf) / §I(Track C)

> このファイルは「mutsu に GC をいつ・どの方式で導入するか」を巡る設計判断を記録する。
> 最終決定が確定していない論点は「未決」節に分け、合意済みの方向性と区別する。
> ADR は今後この決定に触れる作業の起点とし、判断が変わったら新しい ADR で supersede する。

---

## 1. Context（背景）

### 1.1 現状

- mutsu のメモリ管理は **`Arc` 参照カウントのみ**。tracing GC も cycle collector も存在しない。
  → **循環参照はリークする**（`value/mod.rs`）。`Weak` は `CONSUMED_SEQS` / `WeakSub` /
  `ptr_keyed.rs` 等の局所対策に留まり、系統的ではない。
- 循環は Raku で普通に書ける：再帰クロージャの env 自己捕捉、相互参照オブジェクト（親子・グラフ・
  双方向リスト）、`%h<k> := %h` 系の自己束縛（`element-element-bind-plan.md` の MAX_DESCENT は
  「無限ループ防止」であって**メモリは回収していない**）、Promise/Supply の channel 相互参照。
  → **実害あり**。

### 1.2 プロジェクト方針との関係

- gain 定義（CLAUDE.md）：gain = 正しいアーキテクチャ（Raku 互換 + 速度 + flaky なし）。
- ユーザー方針：**まず raku に追いついて十分な速度を出す → その上で明確なメリットのある実装を作る**。
- そのフェーズに到達したとき、**GC のないインタプリタは「欠陥品」とみなされ誰も使わない**。
  → GC は「やるか否か」ではなく **table stakes（プロダクト品質の最低ライン）**。論点は *いつ・どの方式で*。

### 1.3 決定を縛る技術的制約

- **mutsu は既にマルチスレッド実行する**：`start{}` / `Promise` / `hyper` / `race`。
  `Value` は `Arc` ベースで **Send + Sync 前提**。cross-thread 共有の実体は `clone_for_thread` →
  `shared_vars: RwLock<HashMap>`（`runtime/mod.rs`、ANALYSIS §2.1）。
- **mutsu は Rust の所有権の上に乗っている**：`Value` は Rust の enum で、Rust のスタック・ローカル変数・
  一時値・`Vec<Value>` のあちこちに**生で**存在する。VM が全 Value の居場所を把握しているわけではない。

---

## 2. 参考：リファレンス実装（MoarVM）の GC

Rakudo → NQP → **MoarVM**。GC は MoarVM が持ち、構成は：

- **Generational（世代別）**：
  - **Nursery（若い世代）= semi-space copying**。各スレッドが自分の nursery を持ち、**alloc = bump
    pointer（ポインタを進めるだけ）**で激速。collection 時に tospace/fromspace でコピー。
  - **Gen2（古い世代）**：nursery collection を生き残ったオブジェクトを昇格（tenure）。サイズ別
    free-list pools ＋ large object、回収は full collection で mark-sweep 的。
- **Moving（コピーする）**：nursery オブジェクトはアドレスが動く → forwarding ポインタで付け替え。
- **Parallel / stop-the-world**：collection 時に全スレッドを safepoint で止め、回収を複数スレッドで分担。
  世代間参照は **write barrier** で remembered set に記録。
- **refcount は一切使わない。**

**重要な含意**：tracing GC は mutate ごとの atomic inc/dec が無く alloc が bump pointer で激速。
つまり「シングルスレッドの速さ」という観点では、**tracing GC の方が refcount/cycle collector より速い**。

**なぜ MoarVM はそれができるか**：VM が世界を完全に所有しているから。全オブジェクトが `MVMObject`
ヘッダを持ち、値は `MVMThreadContext` 管理下のフレーム経由でアクセスし、C 拡張ですら `MVMROOT` で
root を明示登録する。→ GC が全 root と全ポインタの居場所を正確に把握でき、安全に動かせる。

**mutsu との根本差**：mutsu は Rust スタックに生 Value が散らばっているため、precise moving GC が必要と
する「全 root の把握」が不可能。実現するには Value を必ずハンドル経由でしか触らない設計への
**全面再設計**が要る（後述レベル2）。

---

## 3. Decision（合意した方向性）

> 以下は 2026-06-27 の議論で合意した方向性。**最終承認待ちの分岐は §4 に分離**。

1. **GC は最終的に必須**（プロダクト品質要件）。論点は導入時期と方式のみ。

2. **フェーズ順序を固定する**：

   | フェーズ | 内容 | 現 PLAN.md の該当 | シングルスレッド速度への影響 |
   |---|---|---|---|
   | **A. 追いつく** | 互換性＋速度で raku に並ぶ | §F roast, §2 multi-dispatch, §H module, perf Lever 1/3 | — |
   | **A'. 地ならし** | root を集約し GC 実装を楽にする | §1.4 レキシカルスコープ, §1.3 upvalue index 化 | — |
   | **B. Value 表現リワーク＋GC** | 下記 §3 層3a | §2.1 Track B, GC（新規） | 中立（型フィルタで hot path 0） |
   | **C. JIT** | 独自メリット | perf Lever 4 | 改善 |

   - **GC は JIT の前**：JIT が生成するネイティブコード上のオブジェクト参照を後から GC の root 走査／
     セーフポイントに組み込むのは困難。先に GC を入れてから JIT なら最初から GC 前提の root を吐ける。
   - **GC は Phase A 完了後に着手**：追いついていない段階で GC を入れても「遅くてリークしない
     インタプリタ」にしかならない。GC は当面 PLAN.md の「将来」節に置く。

3. **moving GC（MoarVM 式 nursery copying）は却下**：Rust 所有 + Arc 直接保持と非互換。
   root の完全把握が不可能で、アドレスが動くと `arc_contents_mut`（生ポインタ書き込み）が全壊する。

4. **第一候補は cycle collector on Arc（Bacon-Rajan trial-deletion / concurrent cycle collection）**：
   - `Arc` の Send + Sync を維持 → 既存の cross-thread モデル（start/Promise/hyper/race・`shared_vars`）を
     壊さない。Bacon-Rajan は元々 concurrent 環境向け設計。
   - Value 表現の全置換が不要 → blast radius が tracing より桁違いに小さい。
   - 「循環リークという実害そのもの」だけを狙い撃つ。gain 定義（互換性・正しさ）に直結。

5. **型フィルタで hot path を守る**：`Gc<T>` 管理は**循環を作り得るコンテナ系 variant だけ**に導入する。
   - 対象（コンテナ系）：`Array` / `Hash` / `Set` / `Bag` / `Mix` / `Pair` / `Instance` / `Sub`(closure) /
     `ContainerRef`。
   - 非対象（スカラ系・循環不可能）：`Int` / `Num` / `Rat` / `Complex` / `Str` / `Bool` / `Nil`。
   - → fib 等の**数値演算ループ・文字列処理は candidate 登録ゼロ**。mutsu のベンチ律速（数値・
     メソッド呼び出し）が GC の影響を受けない。「Bacon-Rajan は遅い」は naïve に全オブジェクトへ
     均一適用した場合の話で、型で切れば回避できる。

6. **層分割（Value 表現リワークの内訳）**：

   | 層 | 内容 | 関係 |
   |---|---|---|
   | **3a** | Track B（要素セル化）＋ cycle collector ＝ コンテナ系の `Arc → Gc<T>` 一斉置換 | **不可分。1キャンペーンで実施** |
   | **3b** | NaN-boxing（Value 48→8B） | **JIT の地ならし**（8B 固定・タグ単純・レジスタに乗る）。GC 後・JIT 前。順序 3a→3b→4 |
   | **3c** | biased reference counting（所有スレッドからの操作は非 atomic） | GC 後の独立 perf 課題。**JIT が intループをネイティブ化すれば優先度低下**（hot path から refcount が消える） |

   - **3a が不可分な理由**：Track B も GC も同じ `Arc<ArrayData>` / `Arc<HashData>` 群（ANALYSIS §2.1、
     79 箇所）を触る。別々にやると 79 箇所を 2 回触り、相互に作り直しになる。さらに Track B の最大の
     地雷「`arc_contents_mut` 借用保持中の VM 再入デッドロック」と、cycle collector の「いつ collect を
     走らせるか（セーフポイント）」は**同じ再入境界の問題**で、一緒に設計すれば一本化できる。
   - **3b を分離できる理由**：cycle collector 案では Arc/Gc ポインタが残るので、NaN-boxing は
     「スカラ=payload / コンテナ=Gc ポインタタグ」になるだけ。tracing GC の mark ビット配置問題が
     起きず、GC と同時必須ではなくなる。

7. **検証手段**：`MUTSU_VM_STATS` に `gc_candidate_pushes` / `gc_collections` 等のカウンタを追加し、
   **fib ベンチで candidate push = 0 を実測で保証**する（counters は最適化レベル非依存・debug build で回す
   = CLAUDE.md 方針に合致）。「GC を入れたら hot path が遅くなった」を推測でなく数字で監視する。

8. **レベル1（cycle collector on Arc）を採用する**（2026-06-27 決定）。方針は「cyclic reference が
   解決すればよく、性能は JIT で稼ぐ」。**GC に性能を求めない＝GC を単純に保てる＝JIT も載せやすい**、
   という連鎖で一貫する。
   - **JIT 観点でもレベル1 が載せやすい**：レベル1 は **non-moving + refcount**。JIT＋GC 協調で地獄に
     なる 3 点（① セーフポイントごとのスタックマップ生成 ② moving 時の forwarding ③ write barrier emit）が
     ほぼ消える。non-moving ゆえポインタが動かず forwarding 不要、refcount が生存を保証するので root
     スキャンに JIT スタックマップが原理的に不要。**JIT がやるのは「Arc inc/dec を emit する」だけ**。
   - **レベル2 は JIT の上限性能は最高だが JIT 実装そのものが本格 VM 級**（MoarVM/V8/JVM 相当の
     スタックマップ・forwarding・barrier）。「やりやすさ」では逆に最難関。
   - **mutsu の律速と噛み合う**：数値・fib・メソッド呼び出しが律速で、Int/Num/Bool は型フィルタで
     Gc 対象外。JIT が intループをネイティブ int 化すれば内側は **GC も refcount もゼロの完全ネイティブ
     コード**になり、hot path はレベル2 とほぼ同性能。GC を重く作って性能を取りにいく必要が消える。
   - **レベル2 は長期オプションとして保留**：レベル1 JIT が refcost の天井（オブジェクト多用コード）に
     当たり、かつそれが**実測で**ボトルネックと判明した場合にのみ、新しい ADR で再評価する。

---

## 4. Open questions（未決 — 4.2 / 4.3 のみ。4.1 は §3-8 で決定済み）

### 4.1 レベル1 か レベル2 か → **レベル1で決定（§3-8）**

2026-06-27、**レベル1（cycle collector on Arc）採用で決定**。根拠は §3-8（特に JIT 観点：
non-moving + refcount が JIT のスタックマップ/forwarding/barrier を回避し、型フィルタで数値 hot path は
JIT ネイティブ化で GC/refcost ゼロ）。レベル2（VM 全面再設計で MoarVM 式 precise moving tracing）は、
レベル1 JIT が refcost の天井に**実測で**当たった場合にのみ新しい ADR で再評価する長期オプションとして保留。

参考（却下したレベル2 の特性）：全 Value をハンドル/ヒープ管理し Rust スタックに生 Value を置かない
全面再設計。性能の上限は最も高い（bump alloc・refcost ゼロ）が、事実上インタプリタの再設計で年単位、
Rust で書く安全性の旨味を相当捨てる。

### 4.2 cycle collection の起動方式

- 同期（candidate buffer が閾値超過時にトリガ、その場で trial-deletion scan）
- 非同期（background thread で concurrent collection、シングルスレッド実行は push のみ）

→ 4.1 がレベル1で確定した後、3a 設計時に決める。

### 4.3 A'（地ならし）の範囲

§1.4 レキシカルスコープ導入・§1.3 upvalue index 化のどこまでを「GC の前提条件」とするか。
root を「フレームの `Vec<Value>` ＋ upvalue 配列」に集約できれば cycle collector の root 列挙が単純化する。
env HashMap が root 経路に残る限り走査が複雑化する点が判断材料。

---

## 5. Consequences（決定の帰結）

- **Phase A 完了までは GC 本体に着手しない。** PLAN.md の GC は「将来」節に置く。
- **Track B を単独で先行着手しない。** GC と統合（層3a）する前提に変更する。ANALYSIS §2.1 / §7-6 に
  「Track B は GC と一体（Arc→Gc 一斉置換）」を追記する。
- **perf Lever 2（NaN-boxing）は GC と同時必須ではない**（層3b・GC 後でも可）と PLAN.md §G に反映する。
- **biased refcount を新規 perf 課題（層3c）として PLAN.md §G に追加する**（NaN-boxing・JIT と並ぶ箱）。
- 層3a 着手時は、コンテナ系 variant の `Arc → Gc` 置換と Track B 要素セル化を **1 キャンペーン**で行う。
- 層3a の実装詳細（cooperative STW / root 列挙 / async node の first-wave 切り方）は
  [gc-level1-detailed-design.md](../gc-level1-detailed-design.md) で管理する。

- **CLAUDE.md にエージェント向けルールを追記する**：ADR 慣習（`docs/adr/`）と本 ADR の GC/JIT
  ロードマップ要約（フェーズ順序・moving 却下・cycle collector・型フィルタ・Track B 統合・level-1/2 未決）。

> 注：本 ADR は方向性の記録であり、上記の PLAN.md / ANALYSIS.md への反映（文書再整理）は
> 別作業として実施する（このセッションでは「まず議論を詰める」を選択したため未反映）。CLAUDE.md への
> ルール反映は本 PR で実施済み。

---

## 6. Alternatives considered（検討した代替案）

| 案 | alloc/mutate 速度 | mutsu への実装現実性 | Rust 所有権との関係 | 判定 |
|---|---|---|---|---|
| **MoarVM 式 generational moving tracing** | 最速（bump alloc・refcost 0） | ✕ 全 Value ハンドル化＝VM 全面再設計 | 捨てる | レベル2 として長期保留 |
| **conservative non-moving mark-sweep（Boehm 式）** | 中（bump 無し・Arc 併存問題） | △ Arc/GC 二重管理・誤検出・MT スタック走査が地雷、cycle collector より難しくなりがち | 半分捨てる | 却下寄り |
| **既存 GC ライブラリ（gc-arena / rust-gc / broom）** | — | ✕ ほぼ single-thread（`Gc<T>: !Send`）、cross-thread 不可 | — | 却下（MT 非対応） |
| **cycle collector on Arc（Bacon-Rajan）** | refcost 残るが型フィルタで hot path 0 | ○ 最小侵襲・既存 Arc 活用 | 維持 | **第一候補（レベル1）** |

---

*この ADR は 2026-06-27 の設計議論を記録したもの。判断が変わった場合は新しい ADR で supersede する。*
