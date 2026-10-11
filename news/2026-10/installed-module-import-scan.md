# The parse-time import scan now finds modules in installed repositories

`use Compress::Bzip2;` failed after `mzef install` with "Your !! was gobbled by
the expression in the middle", although the same source parsed through `-I`.

The parser's import scan (`find_module_file`) skipped every `inst#` entry of the
module search path, because it could not map a module name to a source file
there. The runtime does that through `<prefix>/dist/*.json`. So an installed
module's dependencies (`Compress::Bzip2::Raw` and its `constant`s, types, enum
values and exports) were invisible to the importer's parse, and
`($x) ?? BZ_RUN !! BZ_FLUSH` read `BZ_RUN` as a listop head that swallowed the
`!!`.

The scan now resolves `inst#` entries through the dist metadata's `provides`
map (highest version wins), so installed and `-I` modules parse identically.
Pinned by `t/modules/compunit/installed-repo-constant-ternary.t`.
