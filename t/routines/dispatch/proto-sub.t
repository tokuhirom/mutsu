use Test;
plan 3;

proto sub foo($x);
multi sub foo(Int $x) { $x + 1 }
multi sub foo(Str $x) { $x ~ "!" }

is foo(1), 2, 'proto dispatches to Int multi';
is foo("a"), "a!", 'proto dispatches to Str multi';
dies-ok { foo(1.5) }, 'proto without match dies';
