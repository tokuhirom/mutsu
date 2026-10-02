use v6;
use Test;

# From Deps: a bare enum value as a type-only method parameter
# (`multi method m(Store)`) selects on that value, like a sub does.
plan 5;

enum LC <Store New Scope>;
class C {
    multi method m(Store) { "s" }
    multi method m(New)   { "n" }
    multi method m(Scope) { "sc" }
}
is C.new.m(New), "n", 'New';
is C.new.m(Scope), "sc", 'Scope';
is C.new.m(LC::Store), "s", 'qualified';

class D { method only(New) { "ok" } }
my $v = New;
is D.new.only($v), "ok", 'single (non-multi) method';
dies-ok { D.new.only(Store) }, 'wrong enum value is rejected';
