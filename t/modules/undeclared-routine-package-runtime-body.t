use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

plan 5;

# A class declared inside a routine body makes the BEGIN prologue split the
# enclosing class body: its declarations move ahead and its run-time
# statements follow as a separate part. Both parts are one lexical scope, so
# the undeclared-routine check must let a call in the run-time part see the
# body's own routines (the Timezones::ZoneInfo regression).

is_run q:to/CODE/, { :0status, :out("1\n"), :err('') }, 'a call in the class body after the split';
    class C {
        sub isleap($y) { 1 }
        method mk {
            class Tmp { }
        }
        say isleap(1);
    }
    CODE

is_run q:to/CODE/, { :0status, :out("True\n"), :err('') }, 'a call in an anonymous sub assigned in the body';
    class D {
        sub isleap(\year) { year %% 4 }
        method new() {
            class Tmp2 { }
            self.bless
        }
        my &transtime;
        &transtime = sub ($year) { isleap($year) };
        method t($y) { transtime($y) }
    }
    say D.t(2024);
    CODE

is_run q:to/CODE/, { :0status, :out("1\n"), :err('') }, 'in a package body too';
    package P {
        sub helper($y) { $y }
        sub mk { class Tmp3 { } }
        say helper(1);
    }
    CODE

# An undeclared routine is still reported.
is_run q:to/CODE/, { :out(''), :err(/'Undeclared routine:' .* 'nosuch used at line 5'/) }, 'a genuinely undeclared routine is reported';
    class E {
        method mk {
            class Tmp4 { }
        }
        say nosuch(1);
    }
    CODE

# A routine of the class body is still not visible outside it.
is_run q:to/CODE/, { :out(''), :err(/'Undeclared routine:' .* 'inner used at line 7'/) }, 'a body routine stays out of scope outside the body';
    class F {
        method mk {
            class Tmp5 { }
        }
        sub inner($y) { 1 }
    }
    say inner(1);
    CODE
