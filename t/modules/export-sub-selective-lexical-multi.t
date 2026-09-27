use v6;
use Test;
use lib 't/lib';

# Reduced from Identity::Utils' t/02-selective-importing.rakutest, which
# imports each routine in its own bare block through a `sub EXPORT` that looks
# names up in `UNIT::`.

plan 11;

# A package-less module's lexical `multi` family lives on `GLOBAL::name/<sig>`
# registry keys. The block holding the module's FIRST `use` used to drop them
# on exit (a `GLOBAL::` key there looked like an importer alias), so the next
# `use` ran EXPORT against a `UNIT::` without `&build`, and the module's own
# subs could no longer call it.
{
    use SelectiveExportLexicalMulti 'plain';
    ok MY::<&plain>:exists, 'first block imports a plain sub';
}
{
    use SelectiveExportLexicalMulti 'build';
    ok MY::<&build>:exists, 'a later block still imports the lexical multi';
    is build(7), 'int 7', 'the imported multi dispatches (Int)';
    is build('x'), 'str x', 'the imported multi dispatches (Str)';
}
is GLOBAL::probe(), 'int 42', "the module's own sub still reaches its multi";

# `if $out && UNIT::{"&$in"} -> &code` nested in the `else` of an
# `if ... -> &code` read the OUTER `&code` binding (the untaken one).
{
    use SelectiveExportLexicalMulti <long-name:ln>;
    ok MY::<&ln>:exists, 'renamed import exists';
    is MY::<&ln>.name, 'long-name', 'renamed import keeps its original name';
    is ln(1), 'long 1', 'renamed import is callable';
}

# An EXPORT map built from `UNIT::{"&name"}:p` pairs carries each Sub in its
# stash container; a bare call of the imported name died with
# "No such method 'CALL-ME' for invocant of type 'Sub'".
{
    use SelectiveExportPairs <between>;
    is between('Foo:ver<1.0>', '<', '>'), '1.0', 'a :p-exported sub is callable by name';
    is &between('a(b)', '(', ')'), 'b', 'and through its code variable';
}
{
    my sub twice($x) { $x * 2 }
    my %h = f => &twice;
    is (%h<f>:p).value.(21), 42, "a Sub held in a :p pair's container is invocable";
}
