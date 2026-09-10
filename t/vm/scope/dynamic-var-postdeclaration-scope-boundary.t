use v6;
use Test;

# X::Dynamic::Postdeclaration must only fire when a dynamic variable is read
# and *then* `my`-declared again in the exact SAME lexical block. A read in an
# OUTER/earlier scope, followed by an unrelated `my $*x := ...` declared in a
# later, differently-scoped (sibling or nested) block, is legal Raku: the
# earlier read predates that inner binding and never sees it.
#
# Regression: mutsu tracked "has $*x ever been read anywhere in this routine"
# as one unscoped set, so ANY later `my $*x` declaration anywhere in the same
# routine wrongly looked like a post-declaration, even in an unrelated block.

plan 5;

# The ticket's minimal repro: a dynamic var read directly in a method body,
# then an unrelated `my $*CUR := ...` in a nested `do {}` block that also
# calls a callback reading the same name. Both reads must resolve normally
# (through the runtime dynamic-var chain), not trip the compile-time check.
{
    class Foo {
        method go(&task) {
            my @seen;
            @seen.push($*CUR // 'none');
            do {
                my $*CUR := 42;
                task();
            }
            @seen;
        }
    }
    my @out;
    my @seen = Foo.new.go(-> { @out.push($*CUR // 'none') });
    is-deeply @seen, ['none'], 'outer read before the inner declaration sees no dynamic binding';
    is-deeply @out, [42], 'the callback sees the inner do-block\'s dynamic binding';
}

# A read in a NESTED (inner) block, followed by a `my $*x` in the ENCLOSING
# block after the nested block has already closed, is also legal — the inner
# block's own scope closed (and was fully compiled) before the outer `my`
# even exists.
{
    my $seen;
    do {
        do { $seen = $*BOUNDARY // 'none'; }
        my $*BOUNDARY := 5;
    }
    is $seen, 'none', 'a read in a nested block predating an outer my $*x is unaffected';
}

# The genuine illegal case must still be caught: reading a dynamic variable
# and THEN `my`-declaring it again in the very SAME block is a real
# post-declaration error (mirrors roast/S02-names-vars/contextual.t).
throws-like 'do { say $*POSTDECL // "x"; my $*POSTDECL := 1; }',
    X::Dynamic::Postdeclaration, symbol => '$*POSTDECL',
    'reading then my-declaring $*x in the SAME block still throws X::Dynamic::Postdeclaration';

# Same-block illegal case nested one level deeper still throws.
throws-like '{ { say $*NESTED // "x"; my $*NESTED := 1; } }',
    X::Dynamic::Postdeclaration, symbol => '$*NESTED',
    'the same-block check still fires inside a nested block';
