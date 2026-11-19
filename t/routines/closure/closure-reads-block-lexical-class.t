use Test;

# A closure written inside a block that declares `my class Inner` reads that
# class through its bare name even when it runs after the block has exited,
# not a same-named class of the scope it is called from (#17711).

plan 10;

class Inner { method tag { 'outer' } }

sub after-exit() {
    my $c;
    { my class Inner { method tag { 'block' } }; $c = { Inner.tag } }
    $c()
}
is after-exit(), 'block', 'a block closure reads the block class after the block exits';

sub pointy() {
    my &c;
    { my class Inner { method tag { 'block' } }; &c = -> { Inner.tag } }
    c()
}
is pointy(), 'block', '... from a pointy block';

sub anon-sub() {
    my $c;
    { my class Inner { method tag { 'block' } }; $c = sub { Inner.new.tag } }
    $c()
}
is anon-sub(), 'block', '... from an anonymous sub, through .new';

sub nested() {
    my $c;
    { my class Inner { method tag { 'block' } }; $c = { { Inner.tag }() } }
    $c()
}
is nested(), 'block', '... from a closure nested in the closure';

sub type-object() {
    my $c;
    { my class Inner { method tag { 'block' } }; $c = { Inner } }
    $c()
}
is type-object().tag, 'block', 'the type object itself is the block class';
is type-object().^name, 'Inner', '... and keeps its short name';

sub own-shadow() {
    my $c;
    {
        my class Inner { method tag { 'block' } };
        $c = { my class Inner { method tag { 'own' } }; Inner.tag }
    }
    $c()
}
is own-shadow(), 'own', "the closure's own lexical class shadows the block's";

sub routine-level() {
    my class Inner { method tag { 'sub' } }
    my $c = { Inner.tag };
    $c
}
is routine-level()(), 'sub', "a routine's lexical class, read after the routine returned";

my @closures;
for 1..2 {
    my class Inner { method tag { 'loop' } };
    @closures.push: { Inner.tag };
}
is @closures>>.().join(','), 'loop,loop', 'a closure made in a loop body reads its class';

is Inner.tag, 'outer', 'the outer class is untouched';
