# `IO::Path.rename`/`copy`/`move` resolve a destination `IO::Path` against its own `CWD`

IO::Maildir moves a mail from `new/` to `cur/` with
`.rename(.dirname.IO.add: "../cur/" ~ $uniq) given $.IO`. mutsu took the
destination as a string and resolved it against the process cwd, so a maildir
reached through `chdir` never received the file and `walk(agent => USER)` left
every mail in `new/`. An `IO::Path` destination now resolves against its own
`CWD` attribute, as the receiver already did; a plain string still resolves
against the cwd.

Pinned by `t/io/io-path-copy-move-dest-keeps-cwd.t`. IO::Maildir `t/maildir.t`
now matches raku (19 ok).
