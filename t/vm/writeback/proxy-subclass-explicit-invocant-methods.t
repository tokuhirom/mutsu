use Test;

# Methods of a Proxy subclass (annotations' Binder shape: `Mu \SELF:` invocant,
# attributes in the subclass cell) run on the compiled-method fast lane (perf);
# attribute reads and writes, the invocant and the proxy protocol stay intact.
plan 8;

my class Cell is Proxy {
    has $!value = 0;
    has $!reads = 0;
    method new is raw { callwith :FETCH(sub ($) { 42 }), :STORE(sub ($, $) { }) }
    method bump(Mu \SELF: Int $by --> Int) { $!value += $by }
    method peek(::?CLASS:D: --> Int) { $!value }
    method count(Cell:D $: ) { $!reads++; $!reads }
    method plain(Int $by) { $!value + $by }
}
my $c := Cell.new;
is $c.VAR.bump(2), 2, 'Mu \SELF: method writes a subclass attribute';
is $c.VAR.bump(3), 5, 'and again';
is $c.VAR.peek, 5, '::?CLASS:D: method reads it';
is $c.VAR.plain(1), 6, 'a plain method reads it';
is $c.VAR.count, 1, 'an attribute counter';
is $c.VAR.count, 2, 'keeps counting';
is $c.VAR.bump(0), 5, 'attribute state is shared across the methods';
is $c, 42, 'the proxy still FETCHes';
