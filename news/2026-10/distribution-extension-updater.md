# Distribution::Extension::Updater: open(:create), alias-default scoping, `*%_ ()`, gather truthiness

Taking the `Distribution::Extension::Updater` suite from failing to its first assertion to passing
all 18 assertions of `t/01-basic.rakutest` exposed four interpreter gaps, each now fixed and pinned:

- `IO::Path.open(:create)` with no write mode now creates the file and returns a read handle, as
  rakudo does, instead of dying on Rust's "create requires write access".
- A named alias (`:d(:$dir)`) no longer shadows a sibling positional `$d` while its default is
  evaluated, so `:d(:$dir) = $d || '.'` sees the positional.
- A slurpy hash with a sub-signature (`*%_ ()`) is unpacked against it, so the empty form rejects
  any named argument.
- `so`/`if` on a `gather` Seq pulls its first element, so an empty gather (what `File::Find`
  returns for no matches) is false rather than always true.
