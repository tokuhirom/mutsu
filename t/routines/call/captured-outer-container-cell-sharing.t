use Test;

# A `@`/`%` container captured AND mutated in place (push / element assign) by a
# nested named sub is boxed into a shared `ContainerRef` cell at its declaration
# site, so the sub's by-name mutation and the owner's by-name read alias one cell.
# This must hold under BOTH the blanket-reconcile build (default) and the
# cell-boxing build (`MUTSU_NO_BLANKET_RECONCILE=1`). The original motivating
# case is a user `trait_mod:<is>` pushing attribute names into an outer array
# while the class is composed, then read back after a smartmatch (which restores
# env from the local slots — stale without cell sharing).

plan 9;

# 1-3: user trait_mod:<is> pushing to an outer array, read after a smartmatch.
{
    my @noted-names;
    multi trait_mod:<is>(Attribute $a, :$noted!) {
        push @noted-names, $a.name;
    }
    class C1 {
        has $!a is noted;
        has @!b is noted;
        has %!c is noted;
    }
    ok C1.new ~~ C1, 'class with noted attributes instantiated';
    @noted-names .= sort;
    is +@noted-names, 3, 'trait_mod pushes survive composition + smartmatch';
    is @noted-names, ['$!a', '%!c', '@!b'], 'pushed attribute names are coherent';
}

# 4-5: user trait_mod:<is> writing an outer hash element.
{
    my %seen-args;
    multi trait_mod:<is>(Attribute $a, :$tagged!) {
        %seen-args{$a.name} = $tagged;
    }
    class C2 {
        has $.x is tagged('hello');
        has $.y is tagged(42);
    }
    ok C2.new ~~ C2, 'class with parameterized trait instantiated';
    is %seen-args{'$!x'}, 'hello', 'hash element write through captured container is coherent';
}

# 6-7: a plain named sub pushing to an outer array across multiple calls,
# with an intervening function call that save/restores env.
{
    my @log;
    sub record($x) { push @log, $x }
    sub via($x)    { record($x) }
    via('a');
    my $sep = 'between';   # intervening assignment
    via('b');
    is +@log, 2, 'named-sub container push accumulates across calls';
    is @log, ['a', 'b'], 'accumulated container contents are coherent';
}

# 8-9: a named sub mutating an outer hash element across calls.
{
    my %counts;
    sub bump($k) { %counts{$k} = (%counts{$k} // 0) + 1 }
    bump('x');
    bump('x');
    bump('y');
    is %counts<x>, 2, 'named-sub hash element accumulation is coherent';
    is %counts<y>, 1, 'second hash key tracked independently';
}
