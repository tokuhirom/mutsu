use Test;

# Establishing a grammar's parse-wide dynamic variables means scanning every
# rule pattern in the grammar for `:my $*/@*/%*NAME` declarations. The scan
# walks the `:` positions of each pattern, so a pattern full of *other* colons
# — regex adverbs, `:sym<…>` proto variants — and one whose literals are
# multi-byte must not confuse it: neither by missing a real declaration that
# sits behind them, nor by mistaking an adverb for one.
# Verified against rakudo 2026.07.

plan 3;

grammar G {
    token TOP { :my %*TALLY; <item>+ % ',' { } }
    token item { :i [ 'x' | 'λ' ] }
}

class A {
    method item($/) { %*TALLY{~$/.lc}++ }
    method TOP($/) { make %*TALLY.clone }
}

my $m = G.parse('x,X,λ', :actions(A));
ok $m.defined, 'the grammar parses with an adverb-carrying sibling token';
is $m.made.sort(*.key).map({ "{.key}={.value}" }).join(' '), 'x=2 λ=1',
    'the rule-declared %*TALLY is visible to, and shared by, the action methods';

grammar H {
    proto token sigil { * }
    token sigil:sym<dollar> { :i '$' }
    token sigil:sym<at>     { :i '@' }
    token TOP { :my $*COUNT = 0; <sigil>+ { $*COUNT++ } }
}

class B {
    method TOP($/) { make $*COUNT }
}

is H.parse('$@$', :actions(B)).made, 1,
    'a declaration is still found with `:sym<…>` variants and adverbs in the grammar';
