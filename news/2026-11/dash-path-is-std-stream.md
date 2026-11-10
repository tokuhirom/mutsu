# `"-".IO` opens, reads and slurps the standard streams

`'-'.IO.open` now returns `$*IN` (read) or `$*OUT` (write/append) itself with `:bin` / `:enc`
applied, opening a closed `$*IN`/`$*OUT` first. Opening an `IO::Handle` whose path is `-`
attaches STDIN/STDOUT, and `'-'.IO.slurp` slurps `$*IN`, as in Rakudo. This moves
`roast/6.c/APPENDICES/A03-older-specs/01-misc.t` forward (the remaining failures there are the
deprecated `subst-mutate`).
