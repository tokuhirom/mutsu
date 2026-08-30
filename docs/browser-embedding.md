# Run mutsu in a browser

mutsu is available as a WebAssembly npm package. It runs Raku entirely in the
browser: source code and output do not leave the visitor's page.

Browser-enabled mutsu releases are published under the `mutsu` package name.

## Drop-in runnable examples

Load the component from a CDN and put Raku source in a `<mutsu-code>` element:

```html
<mutsu-code autorun>
  <script type="text/raku">
say "Hello from mutsu!";
say (^10).grep(* %% 2).sum;
  </script>
</mutsu-code>

<script type="module"
  src="https://cdn.jsdelivr.net/npm/mutsu@latest/embed.js"></script>
```

Once the package is published, replace `latest` with the version shown on npm
(for example, `mutsu@0.23.0`) so a future release cannot change an existing
example unexpectedly. The component includes an
editor, Run and Reset buttons, and an output pane. Ctrl+Enter (Command+Enter on
macOS) also runs the code.

The `<script type="text/raku">` wrapper is recommended because it keeps Raku
operators such as `<` from being interpreted as HTML. The browser does not
execute this script; `<mutsu-code>` reads it as source text.

Available attributes:

| Attribute | Effect |
| --- | --- |
| `autorun` | Run the example after the WASM module loads. |
| `readonly` | Show source without allowing edits. |
| `session` | Keep declarations between runs. Reset starts a fresh session. |

Every completed run dispatches a bubbling `mutsu-run` event. Its
`event.detail.output` property contains the displayed output:

```js
document.querySelector('mutsu-code').addEventListener('mutsu-run', event => {
  console.log(event.detail.output);
});
```

## Install with npm

Applications using a bundler can install the same component instead of loading
it from a CDN:

```sh
npm install mutsu
```

```js
import 'mutsu/element';
```

The HTML remains the same. The package includes its WASM binary; no Rust tools
or separate server-side interpreter are needed.

## JavaScript API

Use the lower-level API when the page supplies its own editor or UI:

```js
import init, { evaluate, Repl } from 'mutsu';

await init();

// A new interpreter is used for every call.
console.log(evaluate('say "Hello"'));

// One interpreter is retained across calls.
const repl = new Repl();
console.log(JSON.parse(repl.evalLine('my $answer = 40')));
console.log(JSON.parse(repl.evalLine('$answer + 2')));
console.log(JSON.parse(repl.evalBlock('say $answer')));
repl.reset();
```

`evalLine()` and `evalBlock()` return a JSON string with `output` and
`incomplete` fields. `incomplete` is useful for a REPL prompt: it is true when
an input line has an unclosed bracket and needs another line.

## Building locally

Install `wasm-pack`, then run:

```sh
scripts/build-npm-package.sh
```

The publishable package is written to `pkg/`. Serve files over HTTP rather than
opening an HTML file through `file://`, because browsers load WebAssembly with
`fetch`. To test the complete project site locally:

```sh
python3 -m http.server --directory site 8000
```

Then open <http://localhost:8000/embed-demo.html>.

## Browser limitations

- The initial WASM download is large, so load it only on pages that contain
  runnable examples. All components on a page share the downloaded module.
- Execution currently occupies the page's main JavaScript thread. Avoid
  running untrusted or potentially non-terminating programs.
- Native-only facilities, including NativeCall and the native JIT, are not
  available in the browser build.
- mutsu is under active development and does not yet implement all of Raku.
