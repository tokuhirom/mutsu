/**
 * Japanese prose for the landing page.  Snippet code lives in
 * content/highlights.txt; keys must match its "<group>/<id>" ids.
 */

export default {
  hero: {
    title: 'Raku を、このタブの中で',
    tagline: '漸進的型付け、本物の Grammar、遅延リスト、マルチディスパッチ——' +
      '表現力のために設計された言語を、ブラウザに配れるほど小さなインタプリタで。',
    sub: 'mutsu は Rust で書かれた Raku 実装です。このサイトの実行はすべて ' +
      'WebAssembly としてローカルで行われます。サーバとの往復はありません。',
    startTutorial: 'チュートリアルを始める',
    openPlayground: 'プレイグラウンドを開く',
  },

  why: {
    heading: 'Raku の何が面白いのか',
    lede: 'Raku では当たり前で、他の多くの言語では面倒なこと 8 つ。' +
      'どれもこの場で動きます。書き換えて確かめてください。',
  },

  next: {
    heading: '次に読むもの',
    lede: '進み方を選んでください。',
    cards: [
      {
        title: 'ツアーに出る',
        body: '<code>say "hello"</code> から Grammar と並行処理まで全 9 章。' +
          'どのレッスンも編集して実行できるプログラムです。',
        href: 'tutorial.html',
        cta: 'チュートリアルへ →',
      },
      {
        title: 'とにかく触る',
        body: '実行のあいだ状態が保たれる REPL 付きのプレイグラウンド。' +
          '書いたコードは共有用のパーマリンクにできます。',
        href: 'playground.html',
        cta: 'プレイグラウンドへ →',
      },
      {
        title: '本家のドキュメントを読む',
        body: '言語について最終的な権威は公式 Raku ドキュメントで、内容も非常に充実しています。' +
          'このサイトは入り口であって、その代わりではありません。',
        href: 'https://docs.raku.org/',
        cta: 'docs.raku.org →',
      },
    ],
  },

  snippets: {
    'why/rationals': {
      title: '意味どおりに動く算術',
      body: '整数どうしの除算は浮動小数点ではなく正確な有理数になるので、' +
        '<code>0.1 + 0.2 == 0.3</code> はそのまま真です。整数は多倍長なので、' +
        '黙ってオーバーフローすることもありません。',
    },
    'why/junctions': {
      title: 'ジャンクション',
      body: '「複数の値であると同時に 1 つの値」。比較は全要素に分配され、' +
        '1 つの答えにまとまります。<code>||</code> の連鎖が <code>3 | 7 | 9</code> になります。',
    },
    'why/lazy': {
      title: '無限のリスト、有限の計算',
      body: '数列は遅延評価なので「すべての素数」も普通の値として <code>.grep</code> や ' +
        '<code>.head</code> に渡せます。実際に見た要素だけが計算されます。',
    },
    'why/multi': {
      title: 'マルチディスパッチ',
      body: '同名の候補のうち最も狭いものが選ばれます。型でも引数の個数でも、' +
        '任意の <code>where</code> 制約でも分岐できます。FizzBuzz は条件分岐なしの 4 つの宣言になります。',
    },
    'why/grammars': {
      title: '言語に組み込まれた Grammar',
      body: 'パーサは「メソッドが名前付きパターンであるクラス」で、互いに呼び出せます。' +
        '構文解析は後付けのライブラリではなく第一級の機能です。',
    },
    'why/operators': {
      title: '組み立てられる演算子',
      body: 'メタ演算子は任意の演算子を畳み込み（<code>[+]</code>）、' +
        '要素ごとの適用（<code>&gt;&gt;*&gt;&gt;</code>）、zip などに変えます。' +
        '自分で定義した演算子にも同じことができます。',
    },
    'why/concurrency': {
      title: 'コアにある並行処理',
      body: '<code>start</code> は Promise を返し、<code>await</code> が結果を集め、' +
        'Supply がリアクティブなストリームを与えます。フレームワークではなく言語そのものの機能です。',
    },
    'why/unicode': {
      title: '隅々まで Unicode',
      body: 'ソース、識別子、文字列、演算子のすべてが Unicode です。文字列は書記素の列なので、' +
        '数えた結果が読み手の見え方と一致します。',
    },
  },
};
