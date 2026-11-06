# A `sub EXPORT` dispatcher's `.candidates` is the union with the name's other candidates

`&trait_mod:<is>.candidates` on a dispatcher a module's `sub EXPORT` handed out
returned only the registry's candidates of that name whenever there were at
least as many of them as captured ones. `EVAL 'use Test; use Hook; ...'` therefore
listed Test's `:test-assertion!` candidate and lost the hook's own `:xdtrait!`
one (raku lists both). The result is now the name's candidates plus every
captured candidate the name does not reach (#16695).
