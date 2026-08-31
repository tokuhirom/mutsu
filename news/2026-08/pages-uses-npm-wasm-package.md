# GitHub Pages uses the published npm package

The GitHub Pages deployment now installs `@tokuhirom/mutsu` from npm instead
of rebuilding the WebAssembly package with wasm-pack. The site therefore
exercises the same published artifact that downstream browser applications use.
Successful release workflows automatically redeploy Pages after npm publication.

The runnable embedding demo is now linked from the shared navigation and also
documents CDN, npm/bundler, and lower-level JavaScript API integration paths.
