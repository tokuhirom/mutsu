use Test;

plan 16;

# X::Redeclaration::Outer for references in positions the scope walk reaches
# since it runs on the exhaustive mutable AST visitor (ADR-10499). Each
# expectation was checked against rakudo.

# --- a reference in the redeclaring scope: MUST throw ---

throws-like { EVAL q/my $y = 1; sub f($a = $y) { my $y = 2 }/ },
    X::Redeclaration::Outer, 'parameter default is in the routine scope';

throws-like { EVAL q/my $y = 1; sub g($a where $y) { my $y = 2 }/ },
    X::Redeclaration::Outer, 'parameter where clause is in the routine scope';

throws-like { EVAL q/my $y = 1; for 1 -> $a = $y { my $y = 2 }/ },
    X::Redeclaration::Outer, 'for-loop parameter default';

throws-like { EVAL q/my $y = 1; my $f = -> $a = $y { my $y = 2 }/ },
    X::Redeclaration::Outer, 'pointy-block parameter default';

throws-like { EVAL q/my $y = 1; proto f($) { my $q = $y; my $y; {*} }/ },
    X::Redeclaration::Outer, 'proto body';

throws-like { EVAL q/my $y = 1; { my $z where $y; my $y = 2 }/ },
    X::Redeclaration::Outer, 'variable where clause';

throws-like { EVAL q/my $y = 1; { $_ = "a"; s[a] = $y; my $y = 2 }/ },
    X::Redeclaration::Outer, 'assignment-form substitution thunk';

throws-like { EVAL q/my $y = 1; { subset S of Int where $y; my $y = 2 }/ },
    X::Redeclaration::Outer, 'subset where thunk';

throws-like { EVAL q/my $y = 1; { temp $y = 3; my $y = 2 }/ },
    X::Redeclaration::Outer, 'temp of the outer variable';

throws-like { EVAL q/my $y = 1; { temp $y.foo = 1; my $y = 2 }/ },
    X::Redeclaration::Outer, 'temp of a method on the outer variable';

# --- a reference in a nested code object or a non-running position: MUST NOT ---

lives-ok { EVAL q/my $y = 1; class C { has $.a = $y; my $y = 2 }/ },
    'attribute default is its own thunk scope';

lives-ok { EVAL q/my $y = 1; { "a" ~~ m{ a { $y } }; my $y = 2 }/ },
    'regex code block is a nested code object';

lives-ok { EVAL q/my $y = 1; sub f(&c:(Int $ = $y)) { my $y = 2 }/ },
    'a code parameter signature is never run';

lives-ok {
    EVAL q/my $y = 1; react { whenever Supply.from-list(1) -> $y { my $r = $y; my $y } }/
}, 'whenever parameters are the block\'s own lexicals';

# --- the self-initializer check reaches the same positions ---

is EVAL(q/my $x = "a" ~~ m{ a { $x.raku } }; 1/), 1,
    'regex code block in an initializer sees the new binding';

lives-ok { EVAL q/my $x = sub ($a where { $x }) { 1 }/ },
    'a where block in an initializer is a nested code object';
