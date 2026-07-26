# User documentation

The user-facing manual is **published on the site**, in English and Japanese:

- **<https://tokuhirom.github.io/mutsu/manual.html>** — installing mutsu, running
  programs, the command-line options, the module search path, the bundled `mzef`
  package manager, the bundled libraries, environment variables, the
  precompilation cache, and an honest account of Rakudo compatibility.
- <https://tokuhirom.github.io/mutsu/tutorial.html> — a tour of the Raku language
  itself, every lesson runnable in the browser.
- <https://tokuhirom.github.io/mutsu/batteries.html> — the bundled libraries, with
  their versions, licenses and upstream documentation.

The manual's source is `wasm-demo/content/manual.en.js` and
`wasm-demo/content/manual.ja.js`; the page that renders it is
`wasm-demo/manual.html`. Edit those — this file is a pointer, not a copy.

There used to be a full user guide here. It had drifted badly out of date (it
claimed NativeCall was unsupported, that there was no package manager, that
`start` ran on a single thread, and that runtime errors carried no line numbers —
none of which is true), which is exactly the failure mode a second copy invites.
The published manual is now the single source; keep it that way.

Everything else under `docs/` is developer documentation: architecture notes,
design docs and [ADRs](adr/). Start from [CLAUDE.md](../CLAUDE.md) and
[PLAN.md](../PLAN.md) for those.
