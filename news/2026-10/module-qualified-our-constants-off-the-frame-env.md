# Module `our constant` package symbols leave the frame env

A loaded module's top-level qualified `our constant` values now live in its
per-interpreter package-symbol table when the loading env has no binding under
the same name. Direct and indirect lookup, the module's own routines and the
package stash continue to resolve them.

In a small 20-iteration closure-call probe, copied env entries fell from 31
to 30 with the same 41 deep copies. This is slice 7 of ADR-0084 (#7817).
