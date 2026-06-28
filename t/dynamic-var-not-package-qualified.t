use v6;
use Test;

# Regression: assigning to a twigil variable inside a non-unit `package`/`module`
# block wrongly package-qualified its name. For `@*ARGS` the compiler checked the
# sigil (`@`) for a twigil instead of the char after it (`*`), so `@*ARGS = …`
# became `@Zef::CLI::*ARGS` -> "Dynamic variable @Zef::CLI::*ARGS not found".
# Surfaced loading Zef::CLI: `@*ARGS = eager gather …` inside `package Zef::CLI`.

plan 7;

# Dynamic array/scalar assignment inside a package keeps the bare dynamic name.
{
    my @*DYN = 1, 2, 3;
    package P { @*DYN = 7, 8 }
    is @*DYN.elems, 2, '@*DYN = … inside a package writes the dynamic var, not @P::*DYN';
}

{
    my $*FLAG = 0;
    package Q { $*FLAG = 42 }
    is $*FLAG, 42, '$*FLAG = … inside a package writes the dynamic var';
}

# Reading a process dynamic inside a package still works.
package R {
    our $pid-ok = $*PID > 0;
}
ok $R::pid-ok, 'reading $*PID inside a package works';

# Ordinary `our` package variables ARE still package-qualified (not broken).
package S { our $shared = 10 }
is $S::shared, 10, 'our package var is still qualified and externally visible';

# A `module` block behaves the same.
module M {
    our sub bump { $*X = 99 }
}
{
    my $*X = 1;
    M::bump();
    is $*X, 99, 'dynamic write from a module sub reaches the caller dynamic';
}

# Attribute twigils inside a class are unaffected.
class C {
    has $.x = 5;
    method bump { $!x = 6; $!x }
}
is C.new.bump, 6, 'attribute twigil $!x works inside a class method';
is C.new.x, 5, 'attribute accessor works';
