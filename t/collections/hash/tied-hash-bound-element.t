use Test;

# Tied (user Associative) hash: a `:=` bind to an immutable literal makes the
# element read-only even though the write routes through ASSIGN-KEY; and a
# multi-element `:delete` slice deletes each key. Modeled on the Hash::Agnostic
# distribution's t/01-basic.rakutest (T-051).

plan 8;

class MyHash does Associative {
    has %!hash;
    method AT-KEY($key)          is raw { %!hash.AT-KEY($key)         }
    method BIND-KEY($key,\value) is raw { %!hash.BIND-KEY($key,value) }
    method EXISTS-KEY($key)             { %!hash.EXISTS-KEY($key)     }
    method DELETE-KEY($key)             { %!hash.DELETE-KEY($key)     }
    method ASSIGN-KEY($key,\value) is raw { self.AT-KEY($key) = value }
    method keys()                       { %!hash.keys                 }
}

my %h is MyHash;

# --- bound-element read-only (routes through ASSIGN-KEY) ------------------
is (%h<i> := 137), 137, 'can bind a literal to a tied element';
is %h<i>, 137, 'tied bound element reads back the literal';
dies-ok { %h<i> = 666 }, 'assign to a literal-bound tied element dies';
is %h<i>, 137, 'tied bound element value preserved after failed assign';
is %h<i>:delete, 137, 'tied bound element deletes to its value';

# --- multi-element slice delete ------------------------------------------
%h<d> = 628;
%h<e> = 271;
%h<f> = 6;
is-deeply %h<d e f>:exists, (True, True, True), 'tied slice :exists';
is-deeply %h<d e f>:delete, (628, 271, 6), 'tied slice :delete returns each value';
is-deeply %h<d e f>:exists, (False, False, False), 'tied slice deleted';
