use v6;
use Test;

plan 9;

# While a role or class is being declared, its own name is not yet visible to
# its traits: `role Exception is ::Exception` (SQL::Abstract) and plain
# `role Exception is Exception` inherit the core `Exception`, not the role
# itself (#11072). A role composed with `does` inside a package is also not an
# MRO entry of the composing class.

# Each top-level case redeclares a core name, so it runs in its own process.
sub run-code(Str $code) {
    my $proc = run $*EXECUTABLE, '-e', $code, :out, :err;
    $proc.out.slurp(:close).trim ~ $proc.err.slurp(:close).trim
}

is run-code(q:to/END/), "((X) (Exception) (Any) (Mu))\nTrue\n1",
    role Exception is ::Exception { method m { 1 } }
    class X does Exception { }
    say X.^mro; say X.new ~~ CORE::Exception; say X.new.m;
    END
    'a top-level role `is ::Exception` inherits the core Exception';

is run-code(q:to/END/), "((X) (Exception) (Any) (Mu))\nboom",
    role Exception is Exception { }
    class X does Exception { method message { 'boom' } }
    say X.^mro;
    try { die X.new; CATCH { default { say .message } } }
    END
    '... and so does one written without the leading `::`';

is run-code(q:to/END/), "((Exception) (Exception) (Any) (Mu))\nTrue",
    class Exception is ::Exception { }
    say Exception.^mro; say Exception.new ~~ CORE::Exception;
    END
    'a top-level class `is ::Exception` inherits the core Exception';

like run-code('class Foo is Foo { }'), /'cannot inherit from itself'/,
    'a class naming itself with no core type of that name is still an error';

# A role composed inside a package.
class Base { }
module M {
    role Base is ::Base { method m { 'm' } }
    class X does Base { }
}
is M::X.^mro.map(*.^name).join(' '), 'M::X Base Any Mu',
    'a package role `is ::Base` inherits the outer class, and is not an MRO entry';
is M::X.new.m, 'm', "... and its methods are composed";

class P { }
module N {
    role R2 { }
    class Y is P does R2 { }
}
is N::Y.^mro.map(*.^name).join(' '), 'N::Y P Any Mu',
    'a sibling role composed with `does` inside a module is not an MRO entry';
is N::Y.^mro(:roles).map(*.^name).join(' '), 'N::Y N::R2 P Any Mu',
    '... though `:roles` lists it';

module SA {
    role Exception is ::Exception { method m { 1 } }
    class Thrown does Exception { method message { 'sa' } }
}
is SA::Thrown.^mro.map(*.^name).join(' '), 'SA::Thrown Exception Any Mu',
    'a package-scoped `role Exception is ::Exception` composes as in rakudo';
