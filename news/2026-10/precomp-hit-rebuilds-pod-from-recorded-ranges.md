# A precompiled module records where its Pod is, and a hit rebuilds `$=pod` from just those ranges

ADR-12026 §2.4. A module load used to scan every line of the source for Pod on each
hit (about 1.1M instructions for `Test.rakumod`). The load facts now carry the byte
ranges that hold Pod blocks, and a hit builds `$=pod` from those ranges alone. When
the ranges cannot be isolated (a heredoc body inside one) the whole source is scanned
as before. Verify mode (`MUTSU_PRECOMP_VERIFY=1`) recomputes the ranges.
