use Test;

plan 5;

# An anonymous type declared inside a package has no name of its own: the
# enclosing package must not leak into `.^name` (#11669).
my $top = grammar { token TOP { \d+ } }.^name;
like $top, /^ '<anon|' \d+ '>' $/, 'top-level anonymous grammar';

module Mx {
    our $g = grammar { }.^name;
    our $c = class { }.^name;
    our $r = role { }.^name;
}
like $Mx::g, /^ '<anon|' \d+ '>' $/, 'anonymous grammar inside a module';
like $Mx::c, /^ '<anon|' \d+ '>' $/, 'anonymous class inside a module';
like $Mx::r, /^ '<anon|' \d+ '>' $/, 'anonymous role inside a module';

class Mx::Named { }
is Mx::Named.^name, 'Mx::Named', 'named qualified type is unaffected';

done-testing;
