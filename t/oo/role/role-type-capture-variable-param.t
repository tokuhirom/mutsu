use Test;

# Found via the Configuration distribution: `role Generator[::T $builder]`
# binds the type capture AND the variable `$builder` to the argument.

plan 7;

role G1[::T $b] {
    method m { $b.^name }
    method t { T.^name }
}

is G1[Int].m, 'Int', 'variable half of ::T $b, punned role type object';
is G1[Int].t, 'Int', 'type capture half still bound';
is G1[Str].new.m, 'Str', 'variable half through .new';

class D does G1[Int] {}
is D.new.m, 'Int', 'variable half when composed into a class';

my $stored;
class PB { method any() is rw { Proxy.new: FETCH => -> $s { 1 }, STORE => -> $s, $v { $stored = $v } } }
role Gen[::T $builder] {
    method gen { return sub config(&block) { block $builder } }
}
my $seen;
Gen[PB].gen.({ $seen = .WHAT.^name });
is $seen, 'PB', 'a closure over the variable half sees the argument';
Gen[PB].gen.({ .any = 42 });
is $stored, 42, 'method on the captured type object is used, not Any.any';

class E does G1[Str] does G1[Str] { }
is E.new.m, 'Str', 'repeat composition';
