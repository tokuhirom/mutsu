use Test;

# §D module-sub OTF: a non-builtin sub imported into (or defined in) an inner
# lexical scope must NOT remain callable after that scope exits, even though an
# in-scope call OTF-compiled and name-cached it. pop_import_scope bumps
# fn_resolve_gen so the name-keyed otf_call_cache is invalidated together with
# the registry entry. Regression pin for roast/S11-modules/lexical.t.

plan 3;

{
    use lib 't/lib';
    use OtfModuleSub;
    is greet("Al"), "Hello, Al", 'imported sub callable in its lexical scope (OTF + name-cached)';
}

# After the import scope exits, the bare name must be gone: EVAL resolves names
# at run time against the live scope, so this must die rather than hit a stale
# OTF cache entry.
my $escaped = False;
try { EVAL 'greet("X")'; $escaped = True }
nok $escaped, 'imported sub is not callable after its lexical scope exits';

# A plain block-local `my sub` behaves the same.
{
    my sub blocal($n) { $n * 3 }
    is blocal(4), 12, 'block-local my sub works in scope';
}
