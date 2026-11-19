use Test;

# A method whose optional named parameters are left out binds each one's
# type object.  The method fast path binds that omission itself (it used to
# hand every such call to the general binder); these pin that both paths,
# and repeated calls that replay a method-call plan, agree with rakudo.

plan 13;

class C {
    method untyped(:$a, :$b) { ($a.raku, $b.raku).join(',') }
    method typed(Str :$s, Int :$i, Num :$n, Bool :$b) { ($s, $i, $n, $b).map(*.raku).join(',') }
    method uint(UInt :$u) { $u.raku }
    method with-default(:$a = 42, :$b) { "$a {$b.raku}" }
    method computed(:$a = self.base) { $a }
    method base() { 'base' }
    submethod TWEAK(:$authority, :$host) { $!seen = "{$authority.raku} {$host.raku}" }
    has $.seen;
}

my $c = C.new;
is $c.untyped, 'Any,Any', 'omitted untyped named params bind Any';
is $c.untyped(:b(2)), 'Any,2', 'a supplied one binds its value, the other Any';
is $c.typed, 'Str,Int,Num,Bool', 'omitted typed named params bind their type objects';
is $c.uint, 'Int', 'an omitted UInt binds its refinee Int';
# An omitted `Str:D :$d` dies instead: see
# t/routines/signature/omitted-definite-param-dies.t (#18075).
is $c.with-default, '42 Any', 'a constant default and an omitted param';
is $c.computed, 'base', 'a computed default is evaluated';

my @untyped = (^5).map({ $c.untyped });
is @untyped.unique.join('|'), 'Any,Any', 'repeated calls keep binding Any';
my @mixed = (^5).map({ $_ %% 2 ?? $c.untyped(:a($_)) !! $c.untyped });
is @mixed.join('|'), '0,Any|Any,Any|2,Any|Any,Any|4,Any',
    'alternating supplied and omitted calls bind each time';

is C.new.seen, 'Any Any', 'TWEAK sees omitted named params as Any';
is C.new(:authority<x>).seen, '"x" Any', 'TWEAK sees a supplied one';

# An omitted named param shadows a same-named outer lexical.
my $a = 'outer';
class D { method m(:$a) { $a.raku } }
is D.new.m, 'Any', 'an omitted named param shadows an outer lexical';
is D.new.m(:a<in>), '"in"', 'and a supplied one too';

class E { method m(:$x, *%rest) { "{$x.raku} {%rest.keys.sort.join(',')}" } }
is E.new.m(:y(1)), 'Any y', 'an omitted named param beside a named slurpy';
