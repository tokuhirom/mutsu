# The clippy 0.1.97 findings are fixed

A container with stable Rust 1.97.0 failed `make lint` at 38 sites that the 1.96.0 toolchain CI
builds with accepts. They are fixed here ahead of the toolchain move itself, so the move lands as a
change to the pins alone and `make lint` is already clean on the newer clippy when it does.

The rewrites are identical in all four clippy configurations (default, `jit` off, wasm32,
`alloc-stats`); rustdoc was already clean. Each one is also accepted by clippy 0.1.96, so this
changes nothing about what CI gates on today:

- `clippy::question_mark` — an `if let`/`match` whose other arm only returns `None` is now `?`
  (25 sites, mostly the `strip_prefix` chains in the parser and the hyper-operator spellings);
- `clippy::for_kv_map` — `for (_, v) in map.iter()` is `for v in map.values()`;
- `clippy::manual_filter`, a redundant reference in a `format!` argument, `unnecessary_to_owned`
  (`std::slice::from_ref(key)` instead of `&[key.to_string()]`), and an unneeded `attributes: _`
  beside `..`.

No behaviour change. The full gate (checks, fmt, all lint configurations, `make test`,
`make roast`) passed on 1.97.0 before the last rebase; the lint configurations were re-run on both
1.96.0 and 1.97.0 after it.
