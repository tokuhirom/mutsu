/**
 * Japanese text for the Internals hub (internals.html). Same sections, same
 * ids, same order as internals.en.js — keep the two in step. See that file's
 * header for what belongs here and what belongs on the generated pages.
 */

const SRC = 'https://github.com/tokuhirom/mutsu/blob/main';
const src = (path, text) => `<a href="${SRC}/${path}" rel="noopener"><code>${text ?? path}</code></a>`;
const adr = (file, text) => `<a href="${SRC}/docs/adr/${file}" rel="noopener">${text}</a>`;

/* HTML turns a source line break into a space, which is right between English
   words and wrong between Japanese characters (「バイトコード インタプリタ」).
   Drop a line break that touches a non-ASCII character on either side, except
   inside <pre>, where every break is content. */
function joinLines(html) {
  return html.split(/(<pre>[\s\S]*?<\/pre>)/).map((part, i) => i % 2 ? part
    : part.replace(/([^\x00-\x7F])\n\s*/g, '$1').replace(/\n\s*([^\x00-\x7F])/g, '$1'))
    .join('');
}

const TEXT = {
  title: 'mutsu の内部構造',
  intro: 'mutsu が内部でどう動いているかの解説です。ソースコードが実行されるまでの流れ、' +
    'Raku の値の格納方法、バイトコード VM、そしてガベージコレクタを扱います。' +
    'インタプリタを読んだり手を入れたりしたい人向けです。ここからリンクしている 2 つの' +
    'リファレンス（<a href="types.html">値と型</a>、<a href="opcodes.html">VM オペコード</a>）は' +
    'サイトをデプロイするたびにソースから生成しているので、古い mutsu の説明になることは' +
    'ありません。',
  tocIntro: '言語の裏側にある機械: パイプライン、値、VM、GC。',
  tocTitle: '目次',

  sections: [
    {
      id: 'pipeline',
      title: 'ソースから実行まで',
      body: `
        <p>mutsu は Rust で書かれた<strong>バイトコードインタプリタ</strong>です。
        プログラムは 3 つの段階を経て、最後に実行エンジンがちょうど 1 つあります。
        かつてのツリーウォーク型インタプリタは取り除かれ、<code>Interpreter</code>
        という構造体がそのまま VM です。</p>
        <pre><code>ソースコード
   │  パーサ        src/parser/     Raku の文法 → AST (Expr / Stmt)
   ▼
  AST
   │  コンパイラ    src/compiler/   AST → CompiledCode (命令列 + 定数プール)
   ▼
CompiledCode
   │  VM            src/vm/         オペランドスタック上で命令を実行
   ▼                                 ├─ ホットなチャンク → Cranelift JIT (ネイティブコード)
出力                               └─ 型の決まったルーチン → TRIR (型付き IR)</code></pre>
        <h3>パーサ</h3>
        <p>${src('src/parser/')} はソースコードを AST に変換します。式が約 50 種類
        （${src('src/ast.rs', 'Expr')}）、文が約 30 種類（<code>Stmt</code>）です。
        Raku はパースの途中で副言語を切り替えます（正規表現・クォート・Pod は
        それぞれメインの文法の中に独自の文法を持ちます）。パーサはその切り替えに
        従って動きます。パースエラーは構造化されたエラーコードと行・桁を持ちます。</p>
        <h3>コンパイラ</h3>
        <p>${src('src/compiler/')} は AST を 1 度たどって、ユニット・ルーチン・
        クロージャ本体ごとに <code>CompiledCode</code> チャンクを出力します。
        中身は、命令の配列、リテラルや名前を集約する<strong>定数プール</strong>
        （10 か所で使われる同じ文字列も 1 スロットで済みます）、そして各命令を
        ソースの行に対応づける表です。コンパイラは前もって解決できるものは
        解決しておきます。特に、名前がどのレキシカル変数を指すかを解決しておくので、
        変数アクセスのほとんどは名前の検索ではなくスロット番号で済みます。</p>
        <h3>VM</h3>
        <p>${src('src/vm/')} がチャンクを実行します。詳しくは下の
        <a href="#vm">バイトコード VM</a> を参照してください。</p>
        <h3>より速い 2 つの層</h3>
        <ul>
          <li><strong>JIT。</strong>十分に何度も実行されたチャンクは
          <a href="https://cranelift.dev/" rel="noopener">Cranelift</a> でネイティブコードに
          コンパイルされます。デフォルトで有効です（<code>MUTSU_JIT=off</code> で
          無効化）。</li>
          <li><strong>TRIR。</strong>変数・型・呼び出し先がすべて静的にわかるルーチンは、
          型付きで解決済みの IR（${src('src/trir/')}）にもう一度コンパイルされます。
          そこではネイティブの <code>int</code> は箱に入った値ではなく、専用のレジスタ
          バンクに置かれたマシンワードです。VM からは
          <a href="opcodes.html#op-CallTrir"><code>CallTrir</code></a> 命令で呼び出します。</li>
        </ul>`,
    },
    {
      id: 'values',
      title: '値の格納方法',
      body: `
        <p>Raku のあらゆる値（<code>Int</code>、文字列、配列、オブジェクト、
        クロージャ）は Rust の ${src('src/value/mod.rs', 'Value')} で表され、
        <code>Value</code> は <strong>8 バイト</strong>、つまり NaN-boxing された
        64 ビットのワード 1 つです。Perl 5 の <code>SV</code> にあたる役割を担い、
        ペイロードの種類が Perl 5 の SV ボディ型にあたります。</p>
        <pre><code>63            48 47                                           0
┌──────────────┬──────────────────────────────────────────────┐
│  page (16)   │                payload (48)                  │
└──────────────┴──────────────────────────────────────────────┘
page 0x0001          小さな Int。ペイロードがその数値
page 0x0002..0xFFF2  倍精度浮動小数点数 (0x0002 ページ分ずらしたもの)
page 0xFFF3..0xFFFF  kind タグ + インラインの値またはポインタ</code></pre>
        <p>小さな整数と倍精度浮動小数点数はメモリを確保しません。それ以外はすべて
        <strong>kind</strong> で、kind のペイロードの置き場所は次の 4 通りです。</p>
        <ul>
          <li>ワードの中に<strong>インライン</strong>で: <code>Nil</code>、
          <code>Bool</code>、型オブジェクトなど。</li>
          <li><strong><code>Arc&lt;T&gt;</code></strong> の後ろに: 文字列、多倍長整数、
          有理数、Range、<code>Seq</code> など。普通の参照カウントです。</li>
          <li><strong><code>Gc&lt;T&gt;</code></strong> の後ろに: 配列、ハッシュ、
          オブジェクト、クロージャ、コンテナ。参照循環に加わり得るものすべてで、
          <a href="#gc">循環コレクタ</a>が見るのはこれらです。</li>
          <li><strong><code>WeakGc&lt;T&gt;</code></strong> の後ろに: ブロックが
          自分自身を指す弱参照。</li>
        </ul>
        <p>ビット配置を知っているのは
        ${src('src/value/nanbox/mod.rs', 'src/value/nanbox/')} だけです。インタプリタの
        他の部分はワードからデコードした借用 enum である
        ${src('src/value/view.rs', 'ValueView')} を通して値を読むので、エンコーディングを
        変えても呼び出し側に手を入れる必要はありません。</p>
        <h3>コンテナ</h3>
        <p>Raku は値と、それを入れるコンテナを区別します。束縛した変数
        （<code>$a := $b</code>）、クロージャが捕捉した変数、<code>is rw</code> の
        引数はどれも 1 つの <code>Gc&lt;ContainerCell&gt;</code> を共有するので、
        ある名前を通した書き込みは他のすべての名前から見えます。型制約は名前ではなく
        セルが持ちます。配列やハッシュの要素は裸の値として格納され、エイリアスが
        必要になったときに初めて専用のセルに昇格します。</p>
        <p class="manual-note">全 kind とペイロード型の一覧、組み込み型の木
        （<code>Mu</code> → <code>Any</code> → <code>Cool</code> → …）は
        <a href="types.html">値と型</a> にあります。</p>`,
    },
    {
      id: 'vm',
      title: 'バイトコード VM',
      body: `
        <p>VM は<strong>スタックマシン</strong>です。命令はオペランドをスタックから
        取り出し、結果を積みます。<code>1 + $x</code> はおおよそ
        <code>LoadConst</code>、<code>GetLocal</code>、<code>Add</code> に
        コンパイルされます。ディスパッチループは
        ${src('src/vm/vm_exec_dispatch.rs')} にある、現在の命令に対する大きな
        <code>match</code> 1 つです。</p>
        <h3>フレームとローカル変数</h3>
        <p>コンパイラが解決できたレキシカル変数は番号付きの<strong>スロット</strong>に
        置かれ、<code>GetLocal(3)</code> のような命令がスロット 3 を直接読みます。
        呼び出しのスロットは、毎回確保するベクタではなく、1 本の連続したスタックの
        一部を切り出した窓です。そのためルーチンに入るときにメモリ確保が起きません。
        コンパイル時に解決できない名前（動的変数、<code>our</code> 変数、
        <code>EVAL</code> から届く名前など）は名前で引く環境を通ります。こちらは
        遅い経路です。</p>
        <h3>命令セット</h3>
        <ul>
          <li>命令は数百種類あります。ほとんどは小さな命令（ロード、ストア、比較、
          呼び出し）ですが、構文をまるごと実行する<strong>複合命令</strong>もあります。
          <code>ForLoop</code> や <code>WhileLoop</code> などは 1 命令の中でループを
          回すので、反復ごとにディスパッチャを経由しません。</li>
          <li>命令 1 つは <strong>48 バイト</strong>以下に抑えています（ユニットテストで
          固定）。それを超える variant はペイロードを箱に入れるので、命令列が
          コンパクトに保たれます。</li>
          <li>各命令のディスパッチ部には <code>// Cost:</code> コメントがあり、
          計算量と、それが何に比例するかを書いています。Rakudo より悪い場合はそう明記し、
          追跡用の issue を示しています。</li>
        </ul>
        <h3>中身を見る</h3>
        <ul>
          <li><code>mutsu --dump-bytecode file.raku</code> で、プログラムがコンパイル
          されたチャンクを表示できます。</li>
          <li><code>mutsu --dump-ast file.raku</code> で AST を表示できます。</li>
        </ul>
        <h3>JIT</h3>
        <p>JIT はホットになったチャンクをまるごとコンパイルします。デフォルトでは
        100 回呼ばれた時点です（<code>MUTSU_JIT_THRESHOLD</code> で変更可能）。
        2 つの層で動きます。</p>
        <ul>
          <li><strong>Tier A</strong> は命令列を、インタプリタ自身のヘルパー関数を
          順に呼ぶネイティブ関数に変換し、分岐はネイティブの制御フローにします。
          これでディスパッチループがなくなります。</li>
          <li><strong>Tier B</strong> は最も頻度の高い命令（整数・浮動小数点の算術、
          比較、ローカル変数アクセス）をインラインの機械語に展開します。NaN-box の
          タグを確認し、確認に失敗したときはヘルパーに任せます。</li>
        </ul>
        <p>JIT が対応していない命令を含むチャンクは、そのままインタプリタで
        実行されます。ガードも脱最適化もオンスタック置換もないので、JIT の振る舞いは
        構造上インタプリタと同じになります。</p>
        <p class="manual-note">全命令のオペランド・説明・コストは
        <a href="opcodes.html">VM オペコード</a> にあります。</p>`,
    },
    {
      id: 'gc',
      title: 'メモリとガベージコレクション',
      body: `
        <p>mutsu のメモリ管理は<strong>参照カウントと循環コレクタ</strong>の
        組み合わせです。参照カウントは、最後の参照がなくなった瞬間にほぼすべてを
        解放します。解放できないのは循環です。自分自身を捕捉するクロージャ、
        子が親を指し返すオブジェクト、自分自身を含むハッシュなどで、普通の Raku でも
        簡単にできます。循環コレクタはそのためにあります。</p>
        <h3>Gc&lt;T&gt;</h3>
        <p>循環し得るペイロードは ${src('src/gc/gc_ptr.rs', 'Gc<T>')} です。これは
        <code>GcBox</code> を包む <code>Arc</code> で、<code>GcBox</code> のヘッダには
        GC から見える強参照カウント、色、そして「バッファ済み」フラグがあります。
        循環し得ない型（数値、文字列、<code>Seq</code>）は普通の <code>Arc</code> の
        ままで、コレクタに何のコストもかけません。数値計算のベンチマークは GC の
        仕事を一切登録しません。</p>
        <h3>アルゴリズム: Bacon–Rajan の試験的削除</h3>
        <ol>
          <li><strong>候補。</strong><code>Gc</code> への参照が 1 つ drop されても
          まだ他の参照が残っているとき、そのノードは循環だけで生き延びているかも
          しれません。紫に塗られて候補バッファに記録されます。バッファには弱参照を
          入れるので、候補になったことで何かが生き延びることはありません。また、
          スレッド同士が取り合わないよう 64 分割されています。</li>
          <li><strong>灰色マーク。</strong>各候補から部分グラフをたどり、内部の辺の
          数だけカウントを引きます。</li>
          <li><strong>スキャン。</strong>それでもカウントが 0 より大きいノードは、
          部分グラフの外から参照されています。そのノードと、そこからたどれるもの
          すべてを元に戻します（黒）。残りは白です。</li>
          <li><strong>白の回収。</strong>白いノードは互いにしか参照し合っていない
          ゴミです。まずファイナライザを走らせるので、Raku の <code>DESTROY</code> は
          まだオブジェクトの属性を見られます。その後に辺を切り、あとは普通の参照
          カウントが解放します。</li>
        </ol>
        <p>このコレクタはプログラムのルートを探す必要がありません。参照カウント
        だけで動くので、Rust のローカル変数や VM のスタックにある値はスキャンされなくても
        安全です。オブジェクトは決して移動しません。JIT がスタックマップやライト
        バリアなしに生ポインタを持てるのもそのためです。</p>
        <h3>いつ動くか</h3>
        <p>回収のきっかけは<strong>候補バッファの大きさ</strong>です。最初は 16,384 件で、
        回収のたびに生き残った数の 2 倍（上限は約 100 万）に調整されます。回収は
        同期的に、<strong>セーフポイント</strong>でだけ行われます。セーフポイントとは、
        ループの後方ジャンプ、呼び出しや戻り、<code>await</code> など、回収によって
        無効になるような借用をインタプリタが持っていない瞬間です。</p>
        <h3>スレッド</h3>
        <p>複数のスレッドが動いているとき、コレクタは協調的な stop-the-world を
        要求します。他のスレッドは、次のセーフポイントで停止するか、すでにブロッキング
        待ちの中にいます。50 ms 以内に全体が止まらなければ、コレクタは候補を
        バッファに戻して後で再挑戦します。安全でない状態でスキャンすることは
        ありません。</p>
        <h3>調整用の環境変数</h3>
        <table class="opt-table"><tbody>
          <tr><td><code>MUTSU_GC=off</code></td><td>循環コレクタを無効にする
          （循環しないデータは参照カウントで引き続き解放される）</td></tr>
          <tr><td><code>MUTSU_GC_THRESHOLD=N</code></td><td>回収のきっかけになる
          候補バッファの初期サイズ</td></tr>
          <tr><td><code>MUTSU_GC_LOG=summary</code></td><td>回収ごとにログを出す
          （<code>detail</code>、<code>trace</code> でより詳しく）</td></tr>
          <tr><td><code>MUTSU_GC_VERIFY=1</code></td><td>回収の前後でコレクタの
          不変条件を検査する</td></tr>
          <tr><td><code>MUTSU_GC_EVERY_CANDIDATE=N</code></td><td>ストレスモード:
          候補 N 件ごとに回収する</td></tr>
        </tbody></table>
        <p>CI では、コレクタを通常よりはるかに頻繁に動かし、不変条件をすべて検査する
        設定で、テストスイート全体と仕様テスト全体をもう一度走らせています。そのため
        コレクタのバグは、誰かのプログラムのデータ破壊ではなく、テストの失敗として
        現れます。</p>
        <h3>採用しなかったもの</h3>
        <p>MoarVM のような、正確で移動する世代別コレクタは採用しませんでした。
        すべての <code>Value</code> を GC が書き換えられるハンドルにする必要があり、
        事実上 VM の書き直しになるためです。その理由と未解決の課題は
        ${adr('0001-gc-strategy-and-phasing.md', 'ADR-0001')} と
        ${adr('0003-default-on-gc-trigger.md', 'ADR-0003')} に記録されています。</p>`,
    },
    {
      id: 'reading',
      title: 'さらに読む',
      body: `
        <p>大きな設計判断は ADR として
        <a href="https://github.com/tokuhirom/mutsu/tree/main/docs/adr" rel="noopener"><code>docs/adr/</code></a>
        に記録されています。このページの背景にあるものは次のとおりです（英語）。</p>
        <ul>
          <li>${adr('0001-gc-strategy-and-phasing.md', 'ADR-0001')} — GC の戦略と、
          GC → NaN-boxing → JIT の順序</li>
          <li>${adr('0003-default-on-gc-trigger.md', 'ADR-0003')} — 回収のきっかけ</li>
          <li>${adr('0004-jit-strategy.md', 'ADR-0004')} — Cranelift JIT とその層</li>
          <li>${adr('0005-nanbox-representation-encoding.md', 'ADR-0005')} — NaN-box の
          エンコーディング</li>
          <li>${adr('0013-container-interior-mutability-cellvalue.md', 'ADR-0013')} —
          共有コンテナをその場で書き換える仕組み</li>
          <li>${adr('0077-locals-are-a-window-into-a-contiguous-stack.md', 'ADR-0077')} —
          呼び出しのローカル変数を 1 本のスタックの窓にする</li>
          <li>${adr('0110-typed-resolved-ir-for-statically-typed-routines.md', 'ADR-0110')}
          — 型付き IR (TRIR)</li>
        </ul>
        <p>開発者向けのガイドはリポジトリの
        <a href="https://github.com/tokuhirom/mutsu/blob/main/AGENTS.md" rel="noopener"><code>AGENTS.md</code></a>
        です。</p>`,
    },
  ],
};

TEXT.sections = TEXT.sections.map(sec => ({ ...sec, body: joinLines(sec.body) }));

export default TEXT;
