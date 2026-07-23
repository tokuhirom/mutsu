/**
 * Japanese prose for the landing page.  Snippet code lives in
 * content/highlights.txt; keys must match its "<group>/<id>" ids.
 */

export default {
  hero: {
    title: 'mutsu — Rust で書かれた Raku 実装',
    tagline: 'バイトコード VM、ベースライン JIT、循環参照を回収する GC を備えた Raku ' +
      'インタプリタ。パッケージマネージャ同梱の単一バイナリとして配布しています。',
    sub: 'WebAssembly にもビルドでき、このページを動かしているのがそれです。' +
      '以下の例はすべてブラウザ内でローカルに実行されます。サーバはありません。',
    install: 'インストール',
    openPlayground: 'ブラウザで試す',
    repo: 'GitHub',
  },

  stats: {
    roastLabel: '完全にパスする Roast 仕様テストファイル',
    roastNote: '公式 Raku テストスイート {total} ファイル中 {pass} ファイル',
    binaryValue: '単一バイナリ',
    binaryLabel: '別途ランタイム不要',
    binaryNote: '<code>mutsu</code> と、同梱のパッケージマネージャ <code>mzef</code>',
    wasmValue: 'WebAssembly',
    wasmLabel: '同じインタプリタがタブの中で',
    wasmNote: 'このページがそれです。サーバには何も送っていません',
  },

  install: {
    heading: 'インストール',
    lede: 'Linux と macOS 向けのビルド済みバイナリをリリースごとに公開しています。',
    copy: 'コピー',
    copied: 'コピーしました',
    notes: {
      mise: 'リリースアーカイブは <code>bin/mutsu</code>、<code>bin/mzef</code>、' +
        '同梱の Zef ツリーで完結しているので、<code>mzef install &lt;dist&gt;</code> が' +
        '追加設定なしで動きます。',
      docker: 'イメージには両方のバイナリが入っています。<code>mzef</code> で入れた' +
        'モジュールを残したい場合は <code>$HOME</code> に名前付きボリュームをマウントしてください。',
      source: 'Rust 1.92 以降と C コンパイラが必要です。<code>make test</code> でローカルの' +
        'テスト、<code>make roast</code> で公式仕様テストが走ります。',
    },
  },

  features: {
    heading: '中身',
    lede: '機能を絞ったサブセット実装ではありません。公式仕様スイートを基準に開発し、' +
      '言語全体の実装を目指しています。',
    cards: [
      {
        title: '木を辿るのではなくバイトコード VM',
        body: 'ソースを AST に解析し、バイトコードにコンパイルして、約 340 命令の VM で' +
          '実行します。フォールバック先の別インタプリタは存在せず、実行エンジンは VM だけです。',
      },
      {
        title: 'ベースライン JIT',
        body: 'バイトコードがホットになると Cranelift でネイティブコードにコンパイルします。' +
          '既定で有効で、インタプリタのみの経路もコミットごとに並べて計測し続けています。',
      },
      {
        title: '本物のガベージコレクタ',
        body: '参照カウントだけでは循環参照が漏れるため、循環回収器を備えています。' +
          '対象はコンテナ系の値だけなので、数値や文字列の処理は GC のコストを一切払いません。',
      },
      {
        title: 'パッケージマネージャ mzef を同梱',
        body: '本家の <a href="https://github.com/ugexe/zef">Zef</a> を同梱し、mutsu 上で' +
          '動かしています。エコシステムの配布物をダウンロードしてサイトリポジトリに' +
          'インストールし、<code>use</code> で解決できます。別途 Raku を入れる必要はありません。',
      },
      {
        title: 'NativeCall',
        body: 'Raku から C ライブラリを呼べます。ネイティブ型、<code>CArray[T]</code> ' +
          'バッファ、<code>is rw</code> の出力引数を含むポインタに対応しており、' +
          'SQLite の C API を叩ける程度には揃っています。構造体とコールバックは今後の課題です。',
      },
      {
        title: '公式テストスイート駆動',
        body: '仕様は Roast であり、進捗はファイル単位の完全パス数で測ります。' +
          'テストを個別に特別扱いすることはしません。パスしていたファイルが落ちたら' +
          'それはトレードオフではなくバグです。',
      },
    ],
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
