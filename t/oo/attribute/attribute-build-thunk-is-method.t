use Test;

plan 6;

sub foo { 7 }
class A {
    has $.x = foo();
    has $.y = 5;
}

my $thunk = A.^attributes.head.build;
is $thunk.WHAT.raku, 'Method', 'Attribute.build of a non-constant default is a Method';
ok $thunk ~~ Callable, '... and is Callable';
is A.^attributes.tail.build.WHAT.raku, 'Int', 'a constant default is its value';

my @defaults = A.^BUILDPLAN.grep(*[0] == 400);
is @defaults.elems, 2, 'both attributes have a 400 default task';
is @defaults[0][3].WHAT.raku, 'Method', 'the 400 payload of a non-constant default is a Method';
is A.new.x, 7, 'the default still evaluates';
