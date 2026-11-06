# A failed `chdir` / `indir` names the resolved absolute path

`chdir 'bar'` on a missing directory said `... to 'bar': does not exist`; rakudo
resolves the argument against `$*CWD` and names `'/abs/path/bar'`. The
does-not-exist, not-a-directory and permission-denied messages of `chdir` and
`indir` now do the same (#16857; found by the raku-doc sweep,
`Language/io-guide.rakudoc`).
