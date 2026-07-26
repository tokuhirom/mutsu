# mutsu's own documentation is finally published

The site taught the Raku *language* — a tutorial, a playground, a REPL — and said
nothing about the tool. There was no page describing how to install mutsu, what
its command-line options are, where it looks for modules, what `mzef` does, or
how far its Raku support actually goes. The nav had no entry for it because no
such page existed.

`manual.html` is that page, in English and Japanese, in ten sections: installing,
running programs, the command-line options, modules and the search path,
installing packages with `mzef`, the bundled libraries, environment variables, the
precompilation cache, looking inside (dumps, tracing, the JIT and GC switches),
and how compatible mutsu is. A sticky table of contents tracks the section you
are reading, section headings are self-links, and the two languages share their
section ids so a link survives the language switch.

The roast figure in the compatibility section is substituted at render time from
`content/stats.json`, the same file the landing page's headline number comes from
and which `pages.yml` writes at deploy time by counting `roast-whitelist.txt`
against `roast/`. A hand-written percentage in a manual goes stale within a week;
this one cannot.

## The old guide was wrong, so it is gone

`docs/user-guide.md` had drifted into actively misleading territory. It told
readers that NativeCall was not supported (it runs the bundled OpenSSL binding),
that there was no package manager or ecosystem integration (mzef ships in the
box), that `supply`/`react`/`whenever` were unimplemented (they work), that
`start`/`await` ran on a single thread (they run on real OS threads), and that
runtime errors carried no line numbers or stack traces (they carry both). It also
asked for a Rust version two releases behind the actual MSRV.

Every one of those claims was checked against the built interpreter before being
rewritten, not edited from memory. The file is now a pointer to the published
manual, and README's "Known Limitations" was corrected the same way: the entries
that remain — missing compile-time diagnostics, incomplete `X::` coverage, an
immature install path, partial `RakuAST`, multi-line feeds — are ones that
reproduce today.

Keeping a second copy of the manual in the repository is what produced the drift
in the first place, so there is exactly one now: `wasm-demo/content/manual.en.js`
and its Japanese twin, rendered by `wasm-demo/manual.html`.

Covered by twelve new assertions in the site's e2e suite (`wasm-demo/e2e.test.mjs`),
including a structural check that the two languages declare the same sections in
the same order — the thing that silently breaks a shared deep link.
