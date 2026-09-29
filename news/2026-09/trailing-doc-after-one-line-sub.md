# Trailing `#=` doc after a multi-line sub no longer lost behind an earlier one-line sub

`collect_doc_comments` remembered every declaration that "opens a body" so a closing brace
could restore it for a trailing `#=`. A one-line `sub x {}` opens and closes on one line, so it
was pushed but never popped, and the later closing brace restored it instead of the sub that
really ended there. A declaration is now remembered only when its body is still open after its
line. Fixes #9862.
