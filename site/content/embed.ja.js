export default {
  title: 'mutsu を Web ページに組み込む',
  intro: 'この GitHub Pages サイトは、npm で公開されている <code>@tokuhirom/mutsu</code> パッケージを使っています。下の実行可能な例は、アプリケーションからインストールできるものと同じ Web Component です。',
  cdnTitle: 'CDN から直接使う',
  cdnBody: '静的なページでは、コンポーネントを配置して jsDelivr からモジュールを読み込みます。本番環境ではリリースバージョンを固定すると、将来のリリースで既存ページの動作が変わるのを防げます。',
  npmTitle: 'npm でインストールする',
  npmBody: 'バンドラを使うアプリケーションでは、パッケージからコンポーネントを import できます。WebAssembly バイナリも含まれているため、利用者側に Rust や wasm-pack は必要ありません。',
  apiTitle: 'JavaScript API を使う',
  apiBody: '独自のエディタや出力 UI を用意する場合は、低レベル API を使います。',
  reference: 'コンポーネントは <code>readonly</code> と <code>session</code> 属性にも対応し、実行完了時には <code>mutsu-run</code> イベントを発行します。各オプションとブラウザ版の制約については、<a href="https://github.com/tokuhirom/mutsu/blob/main/docs/browser-embedding.md">組み込みリファレンス</a>を参照してください。',
};
