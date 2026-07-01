use Test;

plan 4;

# `.^add_fallback($cond, $calc)` registers a dynamic method fallback: when a
# method is not found on a value of that class, $cond($obj, $name) is checked;
# the first that returns True has $calc($obj, $name) produce the method body,
# which is then invoked with the invocant.

# Lisp-style car/cdr accessors on a chain of Pairs (S06/advent2013-day09).
my $lisp-list = 1 => 2 => 3 => Nil;

Pair.^add_fallback(
    -> $, $name { $name ~~ /^c<[ad]>+r$/ },
    -> $, $name {
        -> $p {
            $name ~~ /^c(<[ad]>*)(<[ad]>)r$/;
            my $r = $1 eq 'a' ?? $p.key !! $p.value;
            $0 ne '' ?? $r."c{$0}r"() !! $r;
        }
    }
);

is $lisp-list.car,   1, 'car returns the key';
is $lisp-list.cadr,  2, 'cadr walks one cdr then car';
is $lisp-list.caddr, 3, 'caddr walks two cdrs then car';

# A name that does not match the condition still throws.
dies-ok { $lisp-list.no-such-method }, 'unmatched method name still throws';
