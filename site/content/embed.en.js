export default {
  title: 'Embed mutsu in a web page',
  intro: 'The GitHub Pages site uses the published <code>@tokuhirom/mutsu</code> npm package. The runnable component below is the same Web Component that applications install.',
  cdnTitle: 'Use it directly from a CDN',
  cdnBody: 'For a static page, add the component and load its module from jsDelivr. Pin a released version in production so an existing page does not change unexpectedly.',
  npmTitle: 'Install it with npm',
  npmBody: 'Bundled applications can import the component from the package. Its WebAssembly binary is included; users do not need Rust or wasm-pack.',
  apiTitle: 'Use the JavaScript API',
  apiBody: 'Use the lower-level API when the application provides its own editor or output UI.',
  reference: 'The component also supports <code>readonly</code> and <code>session</code> attributes and emits a <code>mutsu-run</code> event. See the <a href="https://github.com/tokuhirom/mutsu/blob/main/docs/browser-embedding.md">complete embedding reference</a> for those options and the browser limitations.',
};
