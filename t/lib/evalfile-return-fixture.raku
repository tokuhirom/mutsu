# Fixture for t/eval-context-slice5-residue.t: EVALFILE has no `context`
# argument in its signature (raku-doc/doc/Type/independent-routines.rakudoc:
# `EVALFILE($filename where Blob|Cool, :$lang = 'Raku', :$check)`), so a
# `return` in the file's own mainline must keep its plain (uncontextualized)
# semantics -- unwind to whichever routine dynamically encloses the
# EVALFILE() call, same as an uncontextualized `EVAL 'return'`.
return 5;
