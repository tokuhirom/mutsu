use Test;

# ADR-14843: `self` and `::?CLASS` reach EVAL'd code and by-name lookups
# through the running method's record. Expected values are raku's.

plan 8;

class E {
    has $.v = 7;
    method ev     { EVAL 'self.v' }
    method ev-cls { EVAL '::?CLASS.^name' }
    method ev-nested { EVAL 'EVAL "self.v + 1"' }
    method in-block { my $r; for 1..2 { $r ~= EVAL('self.v') }; $r }
    method sym-name { self.^name }
    method viaref { my $m = self.can('ev')[0]; self.$m }
}

my $e = E.new;
is $e.ev, 7, 'EVAL sees self';
is $e.ev-cls, 'E', 'EVAL sees ::?CLASS';
is $e.ev-nested, 8, 'nested EVAL sees self';
is $e.in-block, '77', 'EVAL in a loop body sees self';
is $e.viaref, 7, 'a method reached by .$m sees its self';

# the top level has no self
throws-like { EVAL 'self' }, Exception, 'self outside a method is an error';

# a different object in the same expression
my $e2 = E.new(v => 9);
is $e2.ev, 9, 'a second object answers its own value';
is "{$e.ev}{$e2.ev}", '79', 'interleaved in one expression';
