/**
 * Japanese prose for the tutorial.  Code and expected output live in
 * content/lessons.txt; the keys here must match its "<chapter>/<lesson>" ids.
 *
 * The explanations follow the official Raku documentation (Raku/doc), vendored
 * in this repository under raku-doc/ and used under the Artistic License 2.0.
 */

export default {
  title: 'Raku の歩き方',
  intro: 'どのレッスンも実際に動くプログラムです。書き換えて「実行」を押すと、' +
    'ブラウザの中のインタプリタがその場で実行します（サーバには何も送りません）。',
  source: '公式 Raku ドキュメント（Raku/doc）に基づいて書かれています。',

  chapters: {
    basics: 'はじめの一歩',
    operators: '演算子',
    control: '制御構造',
    lists: 'リストとハッシュ',
    subs: 'サブルーチンとシグネチャ',
    objects: 'オブジェクト',
    regex: '正規表現と Grammar',
    concurrency: '並行処理',
    'wrapping-up': 'まとめ',
  },

  lessons: {
    'basics/hello': {
      title: 'Hello, World',
      body: `
        <p>Raku のプログラムは文の並びで、各文はセミコロンで終わります。
        <code>say</code> は引数を出力して改行します。</p>
        <p>Raku ではほとんどすべてがメソッドを持つオブジェクトです。リテラルも例外ではなく、
        <code>"Hello".uc</code> は文字列リテラルに対して <code>uc</code>（大文字化）
        メソッドを呼んでいます。</p>
        <p>コメントは <code>#</code> から行末までです。</p>`,
    },
    'basics/variables': {
      title: '変数とシジル',
      body: `
        <p><code>my</code> はレキシカル変数を宣言します。宣言した位置から、それを囲むブロックの
        終わりまでが有効範囲です。</p>
        <p>先頭の記号は<strong>シジル</strong>と呼ばれ、中身の「かたち」を表します。</p>
        <ul>
          <li><code>$</code> — 単一の値（リストを 1 個のものとして扱う場合も含め、任意のオブジェクト）</li>
          <li><code>@</code> — 位置で引くコンテナ（配列）</li>
          <li><code>%</code> — キーで引くコンテナ（ハッシュ）</li>
          <li><code>&amp;</code> — コード</li>
        </ul>
        <p><code>&lt;Raku Perl Rust&gt;</code> は quote-word リストで、
        クォートもカンマも書かずに空白区切りの文字列リストを作ります。</p>`,
    },
    'basics/types': {
      title: '型',
      body: `
        <p>Raku は漸進的型付けです。型注釈は省略できますが、実行時には型が存在します。
        <code>.^name</code> はオブジェクトのメタオブジェクトに型名を尋ねます。
        <code>.^</code> はオブジェクトそのものではなく<em>メタクラス</em>へのメソッド呼び出しです。</p>
        <p><code>~~</code> はスマートマッチ演算子で、型に対しては「この値はその型に適合するか」を
        答えます。型は木構造になっているので、<code>Int</code> は同時に <code>Cool</code>
        （数値と文字列をよしなに変換してくれる型群）でもあり、<code>Any</code> でも
        <code>Mu</code> でもあります。</p>`,
    },
    'basics/interpolation': {
      title: '文字列の補間',
      body: `
        <p>ダブルクォート文字列は変数を補間します。さらに波括弧の中には<strong>任意の式</strong>
        を書けます。整形処理の多くはこれで済むので、テンプレート専用の記法は要りません。</p>
        <p>配列は添字を続けたときに要素が展開されるので、<code>"@xs[]"</code> は
        「<code>@xs</code> の全要素を空白区切りで」という意味になります。裸の <code>@xs</code>
        は文字列中ではそのままなので、メールアドレスを書いても事故になりません。</p>
        <p>シングルクォート文字列は何も補間しません。</p>`,
    },

    'operators/numbers': {
      title: '数値の扱い',
      body: `
        <p>整数どうしの除算は <code>Rat</code>（分子と分母を持つ正確な有理数）になります。
        だから Raku では <code>0.1 + 0.2 == 0.3</code> が <code>True</code> です。
        多くの言語で偽になるのは二進浮動小数点を経由するからで、ここではそれが起きません。</p>
        <p><code>div</code> は整数除算、<code>%</code> は剰余、そして <code>%%</code> は
        「割り切れるか」を直接尋ねる演算子です。<code>x % n == 0</code> と書く必要はもうありません。</p>
        <p><code>Int</code> は多倍長なので、<code>2 ** 100</code> はオーバーフローせず正確です。</p>`,
    },
    'operators/strings': {
      title: '文字列と比較',
      body: `
        <p>Raku は数値用と文字列用の演算子を分けているので、演算子の意味がオペランドによって
        変わることがありません。<code>~</code> は連結、<code>+</code> は加算、
        <code>x</code> は文字列の繰り返し、<code>eq lt gt</code> は文字列比較、
        <code>== &lt; &gt;</code> は数値比較です。</p>
        <p>三方比較は数値ではなく列挙値を返します。<code>&lt;=&gt;</code> は数値順、
        <code>cmp</code> は型に応じた順序で比較し、どちらも <code>Less</code> /
        <code>Same</code> / <code>More</code> のいずれかになります。</p>`,
    },
    'operators/ranges': {
      title: '範囲と数列',
      body: `
        <p><code>..</code> は <code>Range</code> を作ります。どちらかの側に <code>^</code>
        を付けるとその端点を除外するので、<code>1..^5</code> は 1 から 4 です。
        文字列にも使え、桁上がりのように増加します。</p>
        <p><code>...</code> は数列演算子です。最初の数項を書けば規則（等差・等比、あるいは
        自分で書いたクロージャ）を推論します。<code>* + *</code> は 2 引数のクロージャなので、
        <code>1, 1, * + * ... *</code> は遅延評価で無限に続くフィボナッチ数列になります。</p>`,
    },
    'operators/meta': {
      title: 'メタ演算子',
      body: `
        <p>メタ演算子は既存の演算子から新しい演算子を組み立てます。関数名を 100 個覚える代わりに、
        規則を 1 つ覚えれば済みます。</p>
        <ul>
          <li><code>[op]</code> — リストを <code>op</code> で畳み込みます。
              <code>[+]</code> は合計、<code>[*]</code> は総乗、<code>[~]</code> は連結です。</li>
          <li><code>&gt;&gt;op&lt;&lt;</code> — 要素ごとに <code>op</code> を適用します（ハイパー演算子）。
              <code>&gt;&gt;.method</code> は全要素にメソッドを呼びます。</li>
          <li><code>Z</code> は 2 つのリストを綴じ合わせ、<code>X</code> は直積を取ります。</li>
        </ul>
        <p>これらは自分で定義した演算子も含め、どんな演算子とも組み合わせられます。</p>`,
    },
    'operators/junctions': {
      title: 'ジャンクション',
      body: `
        <p>ジャンクションは「複数の値であると同時に 1 つの値」です。比較するとすべての要素に対して
        比較が分配され、ジャンクションの種類（<code>any</code> / <code>all</code> /
        <code>one</code> / <code>none</code>）に従って結果がまとめられます。</p>
        <p><code>|</code> は <code>any</code> の略記です。ジャンクションは真偽値文脈で普通の
        <code>Bool</code> に潰れます。<code>so</code> はその変換を明示的に行うので、
        そのまま出力できます。</p>`,
    },

    'control/conditionals': {
      title: '条件分岐',
      body: `
        <p><code>if</code> / <code>elsif</code> / <code>else</code> は見た目どおりに動き、
        条件を括弧で囲む必要はありません。<code>unless</code> はその否定形です。</p>
        <p><code>with</code> は「定義されているか」を見る <code>if</code> の兄弟です。値が
        <em>定義済み</em>（単に真であるかではなく）ならブロックを実行し、その値をトピック変数
        <code>$_</code> に束縛します。否定形は <code>without</code> です。</p>
        <p>三項演算子は <code>?? !!</code> と書きます。<code>?</code> と <code>:</code>
        は他の用途のために空けてあります。</p>`,
    },
    'control/loops': {
      title: 'ループ',
      body: `
        <p><code>for</code> はリストを反復します。ポインティブロック
        <code>-&gt; $i { ... }</code> は現在の要素に名前を付けます。書かなければ要素はトピック変数
        <code>$_</code> に入ります。</p>
        <p>ポインティブロックは複数の引数を取れて、<code>for</code> はその個数ずつ要素を取り出します。
        <code>.kv</code>（添字と値の組）が自然に読めるのはこのためです。</p>
        <p>どの文にも <code>for</code> / <code>if</code> / <code>while</code> /
        <code>unless</code> を後置修飾子として付けられます。ブロックを書くほどでもないときに便利です。</p>`,
    },
    'control/given-when': {
      title: 'given / when',
      body: `
        <p><code>given</code> はトピック <code>$_</code> を設定し、各 <code>when</code>
        がそれとスマートマッチします。スマートマッチは多相なので、1 つの <code>given</code>
        ブロックで型・厳密な値・正規表現・述語のどれに対しても分岐できます
        （<code>when * &gt; 100</code> はクロージャとのマッチです）。</p>
        <p>マッチした <code>when</code> はその値を持ってブロックを抜けるので、
        <code>break</code> は不要です。残りは <code>default</code> が受けます。</p>`,
    },
    'control/loop-control': {
      title: 'ループ制御',
      body: `
        <p><code>next</code> は次の反復へ進み、<code>last</code> はループを抜け、
        <code>redo</code> はリストを再評価せずに現在の反復をやり直します。</p>
        <p>ループも式です。値が必要な文脈に置かれた <code>for</code> ループは各反復の値の
        リストを返すので、結果を集めるために変数を用意する必要はあまりありません。</p>`,
    },

    'lists/arrays': {
      title: '配列とスライス',
      body: `
        <p><code>@a[$i]</code> は添字アクセスです。添字に<em>リスト</em>を渡すとスライスになり、
        <code>Range</code> もそのようなリストの一種です。</p>
        <p>添字の中の <code>*</code> は要素数を表すので、<code>@a[*-1]</code> は最後の要素、
        <code>@a[*-2]</code> はその 1 つ前です。</p>
        <p>配列はオブジェクトです。<code>.push</code>、<code>.elems</code>、
        <code>.reverse</code>、<code>.rotate</code>、<code>.pick</code> などはすべてメソッドです。</p>`,
    },
    'lists/map-grep-sort': {
      title: 'map・grep・sort',
      body: `
        <p><code>.map</code> は全要素を変換し、<code>.grep</code> は条件に合う要素だけを残し、
        <code>.sort</code> は並べ替えます。</p>
        <p>ブロックの書き方は 3 通りあり、どれも等価です。トピックを使う
        <code>{ $_ * 2 }</code>、自己宣言プレースホルダ引数を使う
        <code>{ $^a &lt;=&gt; $^b }</code>（アルファベット順に引数へ割り当てられます）、
        そして式をクロージャに変える Whatever スター <code>* ** 2</code> です。</p>
        <p><code>.sort</code> に 1 引数のブロックを渡すと、要素そのものではなくその値をキーとして
        並べ替えます。<code>.sort(*.chars)</code> は「短い順」という意味です。</p>`,
    },
    'lists/hashes': {
      title: 'ハッシュ',
      body: `
        <p>ハッシュはキーと値の対応です。<code>%h&lt;key&gt;</code> はリテラルキー用の添字
        （クォート不要）で、<code>%h{$k}</code> は式を取ります。</p>
        <p>ハッシュを反復すると <code>.key</code> と <code>.value</code> を持つ
        <code>Pair</code> が得られます。ハッシュの順序は意図的にランダム化されているので、
        出力するときは意味のある順に明示的に並べ替えてください。</p>
        <p>添字の副詞は構造についての問い合わせです。<code>:exists</code>、
        <code>:delete</code>、<code>:k</code>（キー）、<code>:v</code>（値）、
        <code>:p</code>（ペア）。</p>`,
    },
    'lists/lazy': {
      title: '遅延評価',
      body: `
        <p>数列は遅延評価されます。要素は誰かが要求したときにだけ作られるので、無限リストも
        普通の値として持ち回せます。<code>(1..Inf).grep(*.is-prime)</code> は「すべての素数」で、
        <code>.head(5)</code> はそのうち 5 個だけを計算します。</p>
        <p><code>gather</code> / <code>take</code> は任意の制御構造から遅延リストを作ります。
        ブロックを実行し、<code>take</code> のたびに要素が 1 つ足されます。ループの形をした
        コルーチンだと思ってください。</p>`,
    },

    'subs/basics': {
      title: 'サブルーチン',
      body: `
        <p><code>sub</code> はサブルーチンを宣言します。最後に評価した式が戻り値になるので、
        明示的な <code>return</code> は省略できます。</p>
        <p>引数にはデフォルト値を与えられ、デフォルト値から前の引数を参照できます
        （<code>$h = $w</code> なので <code>area(5)</code> は正方形になります）。</p>
        <p><code>*@rest</code> は<em>スラーピー</em>引数で、残りの位置引数をすべて受け取ります。</p>`,
    },
    'subs/signatures': {
      title: 'シグネチャ',
      body: `
        <p>シグネチャでは型を制約でき（<code>Str $host</code>）、名前付き引数を宣言でき
        （<code>:$port</code>、呼び出し側は <code>:port(8080)</code> または
        <code>port =&gt; 8080</code>）、<code>--&gt;</code> の後ろに戻り値型を書けます。</p>
        <p>名前付き引数は順序が自由で既定では省略可能、位置引数は既定では必須です。
        <code>!</code> や <code>?</code> を付けるとこれを反転できます。</p>
        <p>シグネチャはサブルーチン専用ではありません。ブロック、<code>for</code> ループ、
        例外ハンドラ、分配代入でも同じ記法を使います。</p>`,
    },
    'subs/multi': {
      title: 'マルチディスパッチ',
      body: `
        <p><code>multi</code> は同じ名前を共有する候補の 1 つを宣言します。呼び出しのたびに
        Raku は引数を受け付ける候補のうち<em>最も狭い</em>ものを選びます。判定材料は引数の個数、
        型、厳密なリテラル値、そして <code>where</code> 制約です。</p>
        <p>おかげで再帰の基底条件は宣言そのものになり
        （<code>multi fact(0) { 1 }</code>）、長い型チェックの連鎖は独立して読める複数の定義に
        置き換わります。</p>`,
    },
    'subs/closures': {
      title: 'ブロックとクロージャ',
      body: `
        <p>ブロックは第一級の値です。<code>-&gt; $a, $b { ... }</code> はシグネチャ付きの
        ブロック、<code>{ ... }</code> は <code>$_</code> を受け取るブロックです。</p>
        <p>すべてのブロックは見えているレキシカル変数を捕捉します。下の <code>counter</code>
        は、呼び出しが終わっても生き続ける自分専用の <code>$n</code> を持った関数を返します。</p>
        <p>Whatever スターは式からクロージャを作ります。<code>* * 2</code> は
        <code>{ $_ * 2 }</code> の意味です。引数に名前を付けるほうがかえって邪魔なくらい
        短い式のときに使ってください。</p>`,
    },

    'objects/classes': {
      title: 'クラス',
      body: `
        <p><code>class</code> は型を宣言します。<code>has $.x</code> は公開読み取りアクセサ
        <em>付き</em>の属性、<code>has $!x</code> は非公開の属性です。クラスの内側からは
        どちらも <code>$!x</code> で参照します。</p>
        <p>すべてのクラスには公開属性を名前付き引数で受け取る <code>.new</code> が備わります。
        属性にはデフォルト値を書け、<code>is required</code> を付けると必須になります。</p>
        <p><code>Str</code> メソッドを定義すると、<code>~$p</code> や文字列補間が
        そのオブジェクトの表示方法を知るようになります。</p>`,
    },
    'objects/inheritance': {
      title: '継承',
      body: `
        <p><code>is</code> は親クラスを指定します。メソッドはメソッド解決順序に沿って探索されるので、
        サブクラスの <code>speak</code> が優先され、継承した <code>intro</code> からも
        <em>正しい方</em>が呼ばれます。これが動的ディスパッチです。</p>
        <p><code>self</code> は起動対象（invocant）です。<code>$.name</code> は
        <code>self.name</code> の略記なので、アクセサメソッドを経由し、オーバーライドを尊重します。</p>`,
    },
    'objects/roles': {
      title: 'ロール',
      body: `
        <p>ロールは振る舞いのまとまりで、<code>does</code> によってクラスに<em>合成</em>されます。
        合成は平坦です。ロールのメソッドはクラス自身のメソッドになり、衝突は暗黙に勝者が決まるのではなく
        コンパイル時エラーになります。</p>
        <p>is-a 関係を主張せずに振る舞いだけを共有したいときはロールを選びましょう。クラスは
        自分が does しているすべてのロールに <code>~~</code> でマッチするので、
        インタフェースとしても機能します。</p>`,
    },
    'objects/subsets': {
      title: 'サブセットと列挙型',
      body: `
        <p><code>subset</code> は「型 + 述語」に名前を付けます。これは本物の型で、
        シグネチャでも <code>where</code> 節でもスマートマッチでも使え、値が束縛される
        タイミングで検査されます。</p>
        <p><code>enum</code> は名前付き定数の集合を宣言します。それらは新しい型の値でもあり、
        数値によって順序付けられます。</p>
        <p><code>try</code> はブロックを実行し、失敗したら例外を投げる代わりに <code>Nil</code>
        を返すので、<code>//</code>（定義済みor）で代替値を用意できます。</p>`,
    },

    'regex/matching': {
      title: 'マッチング',
      body: `
        <p>Raku の正規表現は継承ではなく再設計された独自のサブ言語です。パターン中の空白は
        意味を持たないので、読みやすく空けて書けます。リテラルの文字列はクォートで囲みます。</p>
        <p><code>**</code> は繰り返し量指定子で、<code>\\d ** 4</code> はちょうど 4 桁を意味します。
        文字クラスは <code>&lt;[a..z]&gt;</code> と書きます。</p>
        <p>マッチが成功すると <code>$/</code>（マッチオブジェクト）が設定されます。これは
        文字列化するとマッチした部分になります。</p>`,
    },
    'regex/captures': {
      title: 'キャプチャ',
      body: `
        <p>丸括弧は位置キャプチャで、<code>$0</code>、<code>$1</code>… に入ります
        （0 始まりです）。<code>$&lt;name&gt;=[...]</code> は名前付きキャプチャで、
        <code>$&lt;name&gt;</code> で取り出せます。</p>
        <p>名前付き正規表現は <code>my regex name { ... }</code> で宣言し、別のパターンの中から
        <code>&lt;name&gt;</code> として呼び出します。「パターンが他のパターンを呼ぶ」——
        これが Grammar の考え方そのものです。</p>`,
    },
    'regex/substitution': {
      title: '置換',
      body: `
        <p><code>.subst</code> は新しい文字列を返します。置換先にはクロージャを書け、マッチが
        渡されます（<code>*.tc</code> は各マッチを先頭大文字にします）。<code>:g</code>
        副詞を付けると最初の 1 個ではなく全部を置換します。</p>
        <p><code>s///</code> は変数をその場で書き換え、<code>.trans</code> は文字単位の
        変換を行います。</p>`,
    },
    'regex/grammars': {
      title: 'Grammar',
      body: `
        <p>Grammar は「メソッドが名前付きパターンであるクラス」です。解析は <code>TOP</code>
        から始まり、各パターンは他のパターンを名前で呼び出せます。</p>
        <p><code>token</code> はバックトラックを無効にしたパターンで、字句的な部品にはこれが
        適切な既定です。<code>rule</code> はそれに加えて、パターン中の空白を
        「ここに空白が入ってよい」の意味として扱います。</p>
        <p><code>.parse</code> はマッチしたサブパターンで引けるマッチオブジェクトを返し、
        入力全体が解析できなければ <code>Nil</code> を返します。</p>`,
    },

    'concurrency/promises': {
      title: 'Promise',
      body: `
        <p><code>start</code> はブロックをスレッドプール上で実行し、ただちに
        <code>Promise</code> を返します。<code>await</code> は 1 つ、またはリストの完了を待ち、
        結果を順番どおりに返します。</p>
        <p><code>Promise</code> は手動で作って keep（成功）や break（失敗）させることもできます。
        コールバック形式の API をこのモデルに橋渡しするときに使います。</p>`,
    },
    'concurrency/channels': {
      title: 'Channel',
      body: `
        <p><code>Channel</code> はスレッド安全なキューです。生産側が <code>.send</code>、
        消費側が <code>.receive</code> し、<code>.close</code> でこれ以上値が来ないことを
        伝えます。</p>
        <p><code>.list</code> はチャネルが閉じるまで読み続けるので、生産者・消費者のパイプラインが
        普通のリスト処理のように書けます。</p>`,
    },
    'concurrency/supplies': {
      title: 'Supply',
      body: `
        <p><code>Supply</code> は値の非同期ストリームです。<code>Channel</code> が pull 型なのに対し、
        Supply は購読している側へ push します。</p>
        <p><code>react</code> は、<code>whenever</code> ハンドラが値を待っているあいだ生き続け、
        ストリームが終わると終了するブロックを作ります。Supply はリストと同じように
        map・grep・merge できます。</p>`,
    },

    'wrapping-up/exceptions': {
      title: '例外',
      body: `
        <p><code>die</code> は例外を投げます。<code>try</code> ブロックは失敗時に
        <code>Nil</code> を返し、例外を <code>$!</code> に入れます。</p>
        <p>任意のブロックの中に置いた <code>CATCH</code> ブロックは、そのブロックで投げられた
        例外を処理します。型による振り分けには <code>when</code> を使います。処理された例外は
        本当に処理済みで、制御は囲みブロックの後ろから続きます。</p>
        <p>独自の例外型は <code>Exception</code> を継承し、<code>message</code> メソッドを
        用意すれば作れます。</p>`,
    },
    'wrapping-up/phasers': {
      title: 'フェイザ',
      body: `
        <p>フェイザは「書かれた場所」ではなく「決められたタイミング」で実行されるブロックです。
        おかげで前処理・後処理を、関係するコードのすぐ隣に書けます。</p>
        <p><code>ENTER</code> / <code>LEAVE</code> はブロックの出入りで発火します
        （<code>LEAVE</code> は例外で巻き戻る場合にも実行されます）。
        <code>FIRST</code> / <code>LAST</code> はループの最初と最後の反復で、
        <code>BEGIN</code> はコンパイル時に実行されます。</p>`,
    },
    'wrapping-up/introspection': {
      title: 'イントロスペクション',
      body: `
        <p><code>.^</code> はオブジェクトの<em>メタオブジェクト</em>——その型を記述している
        オブジェクト——に対してメソッドを呼びます。これがメタオブジェクトプロトコルであり、
        実行時に利用できます。</p>
        <p><code>.^name</code>、<code>.^attributes</code>、<code>.^methods</code>、
        <code>.^mro</code>（メソッド解決順序）、<code>.^can</code>
        を使うと、想定していなかった型もプログラムから
        調べられます。シリアライザやテストフレームワークはこの仕組みの上に作られています。</p>`,
    },
    'wrapping-up/putting-it-together': {
      title: '総仕上げ',
      body: `
        <p>各章の道具を使った単語頻度の集計です。後置 <code>for</code> 修飾子、
        自動生成されるハッシュ要素、計算したキーによる並べ替え（降順にするための符号反転と、
        同点を解決するためのキー比較）、そして補間の中のメソッド呼び出しを使っています。</p>
        <p>ツアーはこれで終わりです。ここから先は
        <a href="https://docs.raku.org/" rel="noopener">公式ドキュメント</a>
        が各トピックをさらに詳しく解説しています。
        <a href="playground.html">プレイグラウンド</a>ではプログラムをまるごと実行でき、
        <a href="repl.html">REPL</a> では状態の残るセッションで 1 行ずつ試せます。</p>`,
    },
  },
};
