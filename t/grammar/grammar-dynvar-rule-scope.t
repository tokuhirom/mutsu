use Test;

plan 3;

# A grammar-rule dynamic declaration belongs to the rule invocation that
# reaches it. It must not be installed while proto candidates are measured:
# otherwise the losing dollar candidate leaks $*LAST into the at candidate and
# into the start rule's action.
grammar H {
    proto token sigil { * }
    token sigil:sym<dollar> { :my $*LAST = 'dollar'; '$' }
    token sigil:sym<at>     { '@' }
    token TOP { <sigil> }
}
my @seen;
class B {
    method sigil:sym<dollar>($/) { @seen.push($*LAST // 'none') }
    method TOP($/) { make $*LAST // 'none' }
}

is H.parse('$', :actions(B)).made, 'none',
    'a dynamic declaration in the winning proto candidate is scoped to that rule';
is @seen, ['dollar'], 'the winning rule action still sees its own dynamic binding';
is H.parse('@', :actions(B)).made, 'none',
    'a dynamic declaration in a losing proto candidate is not visible to a sibling';
