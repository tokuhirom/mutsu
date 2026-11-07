# `Q :q $...$` and `q :q !...!` honour an escaped delimiter

After `q`/`Q` with colon adverbs and a space, a non-bracket delimiter closed at
its first occurrence even when a backslash had escaped it, so
`Q :q $There are no backslashes here, only lots of \$\$\$!$` (Language/quoting)
stopped at the escaped `$` and failed with "Two terms in a row". The closing
delimiter is now the first unescaped one. Found by the raku-doc sweep.
