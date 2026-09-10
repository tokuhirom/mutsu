use v6;
use Test;

plan 6;

# An unsupplied renamed named parameter with a sigiled leaf variable must
# bind the sigil's empty container, exactly like the plain `:%h` form —
# Cro::HTTP::Server.new's `:ssl(:tls(%tls-in))` binds `%tls-in = {}` when
# neither :ssl nor :tls is passed.

sub one-alias(:tls(%t)) { %t }
is-deeply one-alias(), {}, 'unsupplied :tls(%t) binds empty hash';
is-deeply one-alias(tls => {a => 1}), {a => 1}, 'supplied :tls(%t) binds the hash';

sub two-alias(:ssl(:tls(%tls-in))) { %tls-in }
is-deeply two-alias(), {}, 'unsupplied :ssl(:tls(%tls-in)) binds empty hash';
is-deeply two-alias(ssl => {b => 2}), {b => 2}, 'outer alias key still binds';

sub arr-alias(:a(@x)) { @x }
is-deeply arr-alias(), [], 'unsupplied :a(@x) binds empty array';

sub scalar-alias(:c(:$colour)) { $colour }
ok scalar-alias() === Any, 'unsupplied scalar alias still binds Any';
