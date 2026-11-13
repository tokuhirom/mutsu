# The ecosystem sweep measures the distributions its harness used to skip

Seventeen ecosystem records were `skipped` or `blocked_dep` because of the
sweep harness, not mutsu:

- The newest release was chosen by comparing version strings as text, so
  `v0.1.1`, `*` and `readline.0.1.6` outranked every numbered release and the
  sweep fetched years-old tarballs without a META6.json (IRC::TextColor,
  Readline, HTML::Template, SQL::NamedPlaceholder). Versions are now ordered as
  Raku's `Version` orders them.
- A tarball holding a symlink to an absolute path aborted extraction
  (Gnome::Gtk3, GOTO, App::Urls, Sway::PreviewKeys,
  Collection-Plugin-Development). Such links, dangling on every machine but
  their author's, are now dropped.
- Two workers fetching one shared dependency raced on a fixed `.part` file
  name (L10N::EN).
- `CORE` (rakudo's core distribution), `v6` (the language pragma) and the
  modules of rakudo's core library vendored in `modules/Rakudo-Core` (such as
  RakuAST::Deparse::Highlight) were treated as distributions to resolve
  (Learn::Raku::With, Touch, Rakuast::RakuDoc::Render, Elucid8::Build,
  Air-Plugin-RakuDoc).

Re-measured: IRC::TextColor, HTML::Template, SQL::NamedPlaceholder, L10N::EN,
GOTO, App::Urls, Touch and Rakuast::RakuDoc::Render are green; Readline is
partial (fixed separately), Gnome::Gtk3 is `blocked_load` (#17672), and the
rest have no rakudo baseline.

Rakudo::Options `t/02-with.rakutest`, which needs MoarVM's `MVM_SPESH_LOG`,
is recorded as an accepted divergence (#16953).
