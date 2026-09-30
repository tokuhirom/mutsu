# `.comb` with a `:m` regex honors ignoremark

`"résumé resume".comb(/ :m resume /)` returned one match instead of two. The
position-only scan behind `.comb(Regex)` (`regex_find_all_limited`) never
applied `:m`, while `.match(:g)` did. The scan now matches on the mark-stripped
subject and pattern and maps every span back to the original text, the same way
`regex_find_first` and the capture walk do.
