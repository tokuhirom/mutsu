/**
 * Japanese text for the manual. Mirrors content/manual.en.js section for
 * section — the `id`s must match, since the table of contents and the URL
 * fragments are shared across languages.
 *
 * NOTE: a newline inside a text run renders as a SPACE, which is wrong in
 * Japanese (English wraps on spaces anyway, so the English file may wrap
 * freely). Keep every run of Japanese text on one source line, however long;
 * break only between HTML tags.
 *
 * Section bodies are trusted HTML from this repository (never user input).
 * `{roastPass}`, `{roastTotal}` and `{roastPct}` are substituted at render time
 * from content/stats.json.
 */

export default {
  title: 'mutsu マニュアル',
  intro: 'mutsu そのものの使い方をまとめたページです。インストール、コマンドライン、' +
    'モジュールの探索パス、同梱パッケージマネージャ、そして Raku 互換性の現状。' +
    '言語そのものについては<a href="tutorial.html">チュートリアル</a>か、' +
    '<a href="https://docs.raku.org/" rel="noopener">公式 Raku ドキュメント</a>をどうぞ。',
  tocIntro: 'mutsu 本体のインストールと使い方。Raku 言語そのものはチュートリアルの担当です。',
  tocTitle: '目次',

  sections: [
    {
      id: 'install',
      title: 'インストール',
      body: `
        <h3>mise を使う（推奨）</h3>
        <p>ビルド済みバイナリを GitHub Releases で配布しています（Linux / macOS、x86-64 / arm64 の 4 種）。<a href="https://mise.jdx.dev/" rel="noopener">mise</a> を使うと、インタプリタ <code>mutsu</code> と同梱パッケージマネージャ <code>mzef</code> の<strong>両方</strong>が PATH に入ります。</p>
        <pre><code>mise use -g github:tokuhirom/mutsu        # 最新リリース
mise use -g github:tokuhirom/mutsu@0.18.0 # バージョン固定

mutsu -e 'say "Hello, World!"'
mzef --version</code></pre>
        <p>アーカイブは自己完結していて、追加の設定は要りません。</p>
        <pre><code>bin/mutsu                  インタプリタ
bin/mzef                   パッケージマネージャ（zef を mutsu で動かすシム）
share/mutsu/zef            mzef が動かす、同梱された Zef
share/mutsu/modules        同梱ライブラリ。素の "use" で見つかる</code></pre>

        <h3>Docker を使う</h3>
        <p>両方のコマンドを含むイメージを GHCR で配布しています。タグはリリースに対応し、<code>:latest</code> が最新リリース、<code>:main</code> が開発ブランチです。</p>
        <pre><code>docker run --rm -it ghcr.io/tokuhirom/mutsu              # REPL
docker run --rm ghcr.io/tokuhirom/mutsu mutsu -e 'say (^10).sum'
docker run --rm -v "$PWD:/work:ro" ghcr.io/tokuhirom/mutsu mutsu hello.raku</code></pre>
        <p><code>mzef install</code> は <code>$HOME</code>（イメージ内では <code>/root</code>）以下に書き込みます。インストール結果を実行間で保持するには、そこに名前付きボリュームをマウントしてください。</p>

        <h3>ソースからビルドする</h3>
        <p>Rust 1.94 以降（edition 2024）と、<code>pcre2-sys</code> のビルドに使う C コンパイラが必要です。</p>
        <pre><code>git clone https://github.com/tokuhirom/mutsu.git
cd mutsu
cargo build --release
./target/release/mutsu --version</code></pre>
        <p>バイナリは 2 つできます: <code>target/release/mutsu</code> と <code>target/release/mzef</code>。</p>`,
    },

    {
      id: 'running',
      title: 'プログラムを実行する',
      body: `
        <p>ファイル、文字列、あるいは何も渡さずに実行できます。</p>
        <pre><code>mutsu script.raku              # ファイルを実行
mutsu -e 'say 42'              # ワンライナー
echo 'say 42' | mutsu          # 標準入力から
mutsu -                        # 同じことを明示的に書いた形
mutsu --repl                   # 対話セッション</code></pre>
        <p>シェバンも期待どおり効くので、スクリプト単体を実行可能にできます。</p>
        <pre><code>#!/usr/bin/env mutsu
say "hello from a script";</code></pre>

        <h3>引数</h3>
        <p>スクリプト名より後ろはすべてスクリプトのもので、そのまま <code>@*ARGS</code> に入ります。mutsu 側が解釈することはありません。</p>
        <pre><code>$ mutsu -e 'say @*ARGS' foo --bar=1
[foo --bar=1]</code></pre>
        <p><code>MAIN</code> サブを定義した場合は、引数がそのシグネチャに対してマッチされ、合わない呼び出しには使い方が表示されます。</p>
        <pre><code>$ cat greet.raku
sub MAIN(Str $name) { say "Hello, $name!" }
$ mutsu greet.raku World
Hello, World!
$ mutsu greet.raku
Usage:
  greet.raku &lt;name&gt;</code></pre>

        <h3>終了ステータスとエラー</h3>
        <p><code>exit 3</code> は 3 で終了します。捕捉されなかった例外は、メッセージとバックトレースを標準エラーに出して 1 で終了します。</p>
        <pre><code>$ mutsu boom.raku
boom
  in sub f at boom.raku line 1
  in sub g at boom.raku line 2
  in block &lt;unit&gt; at boom.raku line 3</code></pre>`,
    },

    {
      id: 'options',
      title: 'コマンドラインオプション',
      body: `
        <table class="opt-table">
          <tbody>
          <tr><td><code>-e CODE</code></td><td>ファイルの代わりに <code>CODE</code> を実行する。</td></tr>
          <tr><td><code>-n</code></td><td>プログラムを入力行のループで包む。各行が <code>$_</code> に入る。</td></tr>
          <tr><td><code>-p</code></td><td><code>-n</code> と同じで、各繰り返しの後に <code>$_</code> を出力する。</td></tr>
          <tr><td><code>-ne CODE</code>, <code>-pe CODE</code></td><td>結合形。例: <code>mutsu -ne 'say .uc'</code>。</td></tr>
          <tr><td><code>-I PATH</code></td><td>モジュール探索パスに <code>PATH</code> を追加する。複数指定可、<code>-IPATH</code> とも書ける。</td></tr>
          <tr><td><code>-M MODULE</code></td><td>プログラムの前に <code>use MODULE</code> する。複数指定可。</td></tr>
          <tr><td><code>--repl</code></td><td>対話 REPL を起動する。</td></tr>
          <tr><td><code>--doc</code></td><td>実行せず、ソース中の Pod ドキュメントを整形して出力する。</td></tr>
          <tr><td><code>--dump-ast</code></td><td>実行せず、パースした AST を出力する。</td></tr>
          <tr><td><code>--dump-bytecode</code></td><td>実行せず、コンパイルしたバイトコードを出力する。</td></tr>
          <tr><td><code>--no-precomp</code></td><td>プリコンパイルキャッシュを読まず、書かない。</td></tr>
          <tr><td><code>-v</code>, <code>--version</code></td><td>バージョンを表示する。</td></tr>
          <tr><td><code>-h</code>, <code>--help</code></td><td>オプション一覧を表示する。</td></tr>
          </tbody>
        </table>
        <p>行ループ系のオプションは標準入力を読みます。</p>
        <pre><code>$ printf 'a\\nb\\n' | mutsu -ne 'say .uc'
A
B</code></pre>`,
    },

    {
      id: 'modules',
      title: 'モジュールと探索パス',
      body: `
        <p>モジュール名はパスに対応します。<code>use Foo::Bar</code> は各探索ディレクトリと、そのディレクトリ直下の <code>lib/</code> から <code>Foo/Bar.rakumod</code>（<code>.pm6</code> と <code>.pm</code> も）を探します。つまり通常のプロジェクト構成なら <code>-I .</code> で <code>lib/Foo/Bar.rakumod</code> が見つかります。</p>
        <p>ディレクトリを追加する方法は 3 つあり、mutsu 自身が用意する 2 つの供給源と合わせて、次の順に探索されます。</p>
        <ol>
          <li><code>use lib</code> のパス（後から追加したものが先）</li>
          <li><code>-I</code> のパス（指定順）</li>
          <li><code>MUTSULIB</code> のパス（指定順）</li>
          <li>インストール済みモジュール（<code>mzef</code> が書き込む site リポジトリ）</li>
          <li>mutsu に同梱されたライブラリ</li>
        </ol>
        <p>最初に見つかったものが勝ちます。したがって <code>-I</code> は、同名のインストール済みモジュールを<strong>そのバージョンに関係なく</strong>必ず覆い隠します。このフラグが固定するのはディレクトリであって、バージョンではありません。同じ理屈で、同梱ライブラリは天井ではなく床です。<code>mzef</code> で新しい版を入れれば、それが同梱版を覆い隠します。</p>
        <pre><code>mutsu -I lib script.raku
MUTSULIB=/opt/raku/lib:/srv/lib mutsu script.raku</code></pre>
        <pre><code>use lib 'lib';          # プログラム内で。リストも渡せる
use Foo::Bar;</code></pre>

        <h3>インストール済みモジュール</h3>
        <p>インストールされたディストリビューションは <em>site リポジトリ</em>、つまり <code>$XDG_DATA_HOME/mutsu/repo/site</code>（既定では <code>~/.local/share/mutsu/repo/site</code>）に置かれます。素の <code>use</code> で見つけるための設定は何も要りません。</p>
        <p>同じモジュール名を複数のディストリビューションが提供している場合は最も高いバージョンが選ばれ、<code>use</code> 文で絞り込めます。</p>
        <pre><code>use JSON::Class:auth&lt;zef:jonathanstowe&gt;;
use Cro::HTTP:ver&lt;0.8.7+&gt;;
use Some::Module:api&lt;2&gt;;</code></pre>`,
    },

    {
      id: 'packages',
      title: 'mzef でパッケージを入れる',
      body: `
        <p><code>mzef</code> は本物の <a href="https://github.com/ugexe/zef" rel="noopener">Zef</a>（Raku エコシステムのパッケージマネージャ）を同梱し、それを mutsu 上で動かしたものです。インタプリタと同じアーカイブに入っているので、ブートストラップは不要です。</p>
        <pre><code>mzef install JSON::Fast      # ディストリビューションと依存を入れる
mzef list --installed        # 入っているものを見る
mzef info JSON::Fast         # エコシステムのメタデータ
mzef search json             # 探す
mzef uninstall JSON::Fast
mzef update                  # エコシステムのインデックスを更新</code></pre>
        <p>インストール先は上で説明した site リポジトリで、素の <code>use</code> がすぐに拾います。</p>
        <p><strong>ここはまだ荒削りです。</strong>CLI もインストール経路も動きますが、エコシステムは広く、すべてのディストリビューションが mutsu 上できれいにビルド・テストできるわけではありません。失敗したらそれは報告する価値のある mutsu のバグです。ライブラリに手を入れるのではなくインタプリタを直す、というのがこのプロジェクトの方針です。</p>`,
    },

    {
      id: 'bundled',
      title: '同梱ライブラリ',
      body: `
        <p>mutsu はコミュニティのライブラリ一式をバイナリの隣、<code>share/mutsu/modules</code> に同梱しています。いずれも上流のモジュールを無改変で採用したもので、インストールした瞬間から探索パスに載っています。<code>mzef install</code> もネットワークも要らず、素の <code>use</code> が動きます。</p>
        <pre><code>use HTTP::UserAgent;
my $ua = HTTP::UserAgent.new;
say $ua.get('https://example.com').content.chars;</code></pre>
        <p>TLS、HTTP、URI、MIME::Base64、テンプレート、一時ファイル操作などが最初から入っています。<a href="batteries.html">同梱ライブラリのページ</a>に、バージョン・ライセンス・提供モジュール・上流ドキュメントを一覧しています。</p>
        <p>同梱は最も優先度の低い供給源なので、同梱ライブラリの新しい版を <code>mzef</code> で入れればそれが単に優先されます。セキュリティ修正が mutsu のリリースを待たずに届くのはこの仕組みのおかげです。</p>`,
    },

    {
      id: 'env',
      title: '環境変数',
      body: `
        <table class="opt-table">
          <tbody>
          <tr><td><code>MUTSULIB</code></td><td>コロン区切りのモジュール探索パス。<code>-I</code> の後に探索される。</td></tr>
          <tr><td><code>MUTSU_BUNDLE_DIR</code></td><td>同梱ライブラリの場所。既定はバイナリの隣の <code>share/mutsu/modules</code>。</td></tr>
          <tr><td><code>XDG_DATA_HOME</code></td><td>site リポジトリの親。実際の場所は <code>$XDG_DATA_HOME/mutsu/repo/site</code>。既定は <code>~/.local/share</code>。</td></tr>
          <tr><td><code>XDG_CACHE_HOME</code></td><td>プリコンパイルキャッシュの親。実際の場所は <code>$XDG_CACHE_HOME/mutsu/precomp</code>。既定は <code>~/.cache</code>。</td></tr>
          <tr><td><code>MZEF_ZEF_HOME</code></td><td><code>mzef</code> が動かす Zef ツリー。通常はバイナリの隣から自動で見つかる。</td></tr>
          <tr><td><code>MZEF_MUTSU_BIN</code></td><td><code>mzef</code> が Zef を動かすときのインタプリタ。通常は隣の <code>mutsu</code>。</td></tr>
          </tbody>
        </table>
        <p>JIT・GC・トレースなど実行エンジンの挙動を変える変数は<a href="#debugging">中を覗く</a>にまとめてあります。</p>`,
    },

    {
      id: 'cache',
      title: 'プリコンパイルキャッシュ',
      body: `
        <p>モジュール読み込みで高くつくのはパースなので、mutsu はパース結果を <code>$XDG_CACHE_HOME/mutsu/precomp</code>（既定では <code>~/.cache/mutsu/precomp</code>）にキャッシュします。エントリはソースのパスで引かれ、更新時刻と内容のハッシュで検証されます。モジュールを編集すれば自動で無効になるので、変更を反映させるために手でキャッシュを消す必要はありません。</p>
        <p>実行単位で止めるなら <code>--no-precomp</code>、モジュール単位ならソースに <code>no precompilation;</code> と書きます。ディレクトリごと削除しても安全で、必要になれば作り直されます。</p>`,
    },

    {
      id: 'debugging',
      title: '中を覗く',
      body: `
        <p>捕捉されなかった例外は、すでにファイル名と行番号つきのバックトレースを出します。それで足りないときは:</p>
        <pre><code>mutsu --dump-ast -e 'say 1 + 2'        # パーサが何を作ったか
mutsu --dump-bytecode -e 'say 1 + 2'   # コンパイラが何を吐いたか</code></pre>
        <table class="opt-table">
          <tbody>
          <tr><td><code>MUTSU_TRACE=1</code></td><td>すべてを標準エラーにトレースする。カンマ区切りで絞れる: <code>MUTSU_TRACE=parse,vm</code>。</td></tr>
          <tr><td><code>MUTSU_VM_STATS=1</code></td><td>終了時に VM のカウンタ要約を 1 行で標準エラーに出す。</td></tr>
          <tr><td><code>MUTSU_JIT=off</code></td><td>JIT を使わずバイトコードインタプリタだけで動かす（既定は on）。</td></tr>
          <tr><td><code>MUTSU_GC=off</code></td><td>循環参照コレクタを止める（既定は on）。循環はリークするが、切り分けには役に立つ。</td></tr>
          </tbody>
        </table>
        <p>おかしな挙動に出会ったら、最短で役に立つバグ報告は「プログラム」「mutsu の出力」「同じプログラムを <code>raku</code> で動かした出力」の 3 点です。報告先は <a href="https://github.com/tokuhirom/mutsu/issues" rel="noopener">Issue トラッカー</a>。</p>`,
    },

    {
      id: 'compat',
      title: 'どこまで互換なのか',
      body: `
        <p>mutsu は Raku 公式仕様テストスイート <a href="https://github.com/Raku/roast" rel="noopener">roast</a> の <strong>{roastTotal} ファイル中 {roastPass} ファイル</strong>に通っています（{roastPct}%）。数え方はファイル単位で、そのファイル中のすべての表明が通った場合だけ 1 と数えます。参照実装はあくまで Rakudo で、mutsu は毎日それに対して計測されています。</p>

        <h3>「まだ無いだろう」と思われがちだが、ある機能</h3>
        <ul>
          <li><strong>本物のスレッド。</strong><code>start</code>/<code>await</code>、<code>Thread</code>、<code>Lock</code>、<code>Promise</code> は OS スレッド上で動きます（協調的な擬似実装ではありません）。</li>
          <li><strong>Supply と react。</strong><code>supply</code>/<code>react</code>/<code>whenever</code> が動きます。</li>
          <li><strong>NativeCall。</strong>同梱の <code>OpenSSL</code> バインディングを動かせる程度には実装済みで、HTTPS はこれで通っています。</li>
          <li><strong>グラマー。</strong>アクションクラスと <code>make</code>/<code>.made</code> も含みます。</li>
          <li><strong>コンテナ。</strong><code>Proxy</code>、バインディング、<code>is rw</code> は仕様どおりに動きます。</li>
        </ul>

        <h3>既知の穴</h3>
        <ul>
          <li>コンパイル時診断がいくつか足りません。いちばん目につくのは、未宣言変数が strict モードの要求どおりにはコンパイル時に弾かれないことです。</li>
          <li><code>X::</code> 例外型がすべて揃ってはいないので、珍しい型でマッチする <code>CATCH</code> が発火しないことがあります。</li>
          <li>複数行にまたがるフィード演算子はまだパースできません（1 行のフィードは動きます）。</li>
          <li><code>RakuAST</code> はありますが、完成には程遠い状態です。</li>
          <li>エコシステムの任意のディストリビューションを入れるのは、まだ安定してできるとは言えません。上の <a href="#packages">mzef</a> を参照。</li>
        </ul>

        <h3>速度</h3>
        <p>起動は Rakudo のおよそ 25 倍速く、ワンライナーやスクリプトで mutsu が快適なのはこれが理由です。定常状態の性能もプロジェクトのベンチマークでは Rakudo の近くまで来ていて、既定で有効な JIT を入れると再帰やメソッド呼び出しのベンチマークは <code>raku</code> と同等かやや上回ります。形容詞よりも実測値を、ということで、<a href="bench-trend.html">ベンチマークの推移</a>は main への push ごとに計測されています。</p>

        <p class="manual-note">mutsu は活発に開発中で、まだプロダクション利用は推奨しません。とはいえスクリプト、CLI ツール、Raku の学習には十分使えます。</p>`,
    },
  ],
};
