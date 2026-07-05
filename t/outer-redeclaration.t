use Test;

plan 12;

# X::Redeclaration::Outer: redeclaring a lexical with `my`/`state` *after* it has
# been referenced (resolving to an enclosing binding) in the same scope.

# --- cases that MUST throw X::Redeclaration::Outer ---

throws-like { EVAL q/
    sub s($i is copy) {
        my @array;
        for 1..3 {
            @array.push($i);
            my $i = 1;
        }
    };
    s(9);/ },
    X::Redeclaration::Outer,
    'redeclare param after use in inner block';

throws-like { EVAL q/
    sub s($i is copy) {
        my @array;
        for 1..3 {
            @array.push($i);
            my ($i) = 1;
        }
    };
    s(9);/ },
    X::Redeclaration::Outer,
    'redeclare param after use in inner block (signature form)';

throws-like { EVAL q/my $x = 1; { say $x; my $x = 2 }/ },
    X::Redeclaration::Outer,
    'redeclare outer my after use in bare block';

throws-like { EVAL q/my $x = 1; { say $x; state $x = 2 }/ },
    X::Redeclaration::Outer,
    'state redeclaration is also caught';

throws-like { EVAL q/my $x = 1; sub f { say $x; my $x = 2 }; f/ },
    X::Redeclaration::Outer,
    'redeclaration across a routine boundary is caught';

throws-like { EVAL q/my @a = 1, 2; { @a.elems; my @a = 3 }/ },
    X::Redeclaration::Outer,
    'array sigil variant is caught';

# --- cases that MUST NOT throw ---

lives-ok { EVAL q/my $x = 5; { my $x = 1; my $r = $x }/ },
    'shadowing without prior use is fine';

lives-ok { EVAL q/my $x = 1; { my $x = 2; my $r = $x }/ },
    'redeclaration BEFORE use is fine';

lives-ok { EVAL q/my $x = 1; { { my $r = $x }; my $x = 2 }/ },
    'reference in a deeper nested block does not count';

lives-ok { EVAL q/my $x = 1; { my $r = $x }; { my $x = 2 }/ },
    'reference in a sibling block does not count';

lives-ok { EVAL q/my $x = 1; { my $r = $x }/ },
    'plain outer reference without redeclaration is fine';

lives-ok { EVAL q/my $x = 5; { my $y = $x; my $r = $y }/ },
    'referencing outer to initialize a differently-named my is fine';
