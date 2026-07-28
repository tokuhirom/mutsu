use Test;

plan 4;

# A stateless `:actions(Actions)` grammar action is commonly passed as the
# bare type object (not `Actions.new`), e.g. the bundled YAMLish battery does
# `nextwith($string, :actions(Actions), |%args)`. And most match-tree nodes
# in a real grammar (low-level tokens like whitespace/char-class helpers) have
# no corresponding action method at all — the dispatcher must silently skip
# those without mishandling the type-object case.

grammar SparseGrammar {
    token TOP { <word> [ <.ws> <word> ]* }
    token word { <alpha>+ }
    token ws { <.space>+ }
}

class SparseActions {
    method TOP($/) {
        make $<word>>>.made.join('-');
    }
    method word($/) { make ~$/ }
    # No `alpha` or `ws` method: those rules must be silently skipped.
}

my $m1 = SparseGrammar.parse('foo bar baz', :actions(SparseActions));
is $m1.made, 'foo-bar-baz', 'bare type-object actions dispatch works';

my $m2 = SparseGrammar.parse('foo bar baz', :actions(SparseActions.new));
is $m2.made, 'foo-bar-baz', 'instantiated actions dispatch works the same way';

# A proto/`:sym<>` variant with a type-object actions class: the sym-specific
# method is tried first, then the plain rule name, then silently skipped.
grammar SymGrammar {
    token TOP { <alt> }
    proto token alt { * }
    token alt:sym<a> { 'a' }
    token alt:sym<b> { 'b' }
}

class SymActions {
    method TOP($/) { make ~$<alt>.made }
    method alt:sym<a>($/) { make "A:" ~ ~$/ }
    # No method for alt:sym<b> or plain "alt": must fall through silently.
}

is SymGrammar.parse('a', :actions(SymActions)).made, 'A:a',
   'sym-variant action method resolves on a bare type-object actions class';
is SymGrammar.parse('b', :actions(SymActions)).made, '',
   'missing sym-variant action falls through silently on a type-object actions class';
