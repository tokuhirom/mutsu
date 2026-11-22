use Test;

# An `is rw` method whose tail is a bare `$!x` writes the copy of the class
# that declares it, when a parent and a child both declare a private `$!x`
# (#18558).

plan 9;

class A { has Int $!x = 1; method m { $!x }; method acc is rw { $!x } }
class B is A { has $!x = 2; method bx { $!x }; method bacc is rw { $!x } }

my $b = B.new;
$b.acc = 7;
is "{$b.m}{$b.bx}", "72", 'the parent method writes the parent copy';
$b.bacc = 9;
is "{$b.m}{$b.bx}", "79", 'the child method writes the child copy';
$b.acc += 1;
is "{$b.m}{$b.bx}", "89", 'compound assignment reaches the parent copy';
$b.acc++;
is $b.m, 9, 'increment reaches the parent copy';
$b.acc = Nil;
is $b.m.raku, 'Int', 'Nil restores the parent declaration\'s type object';
throws-like { $b.acc = "s" }, X::TypeCheck::Assignment,
    'the store is checked against the parent declaration';
is $b.m.raku, 'Int', 'a rejected store leaves the copy alone';

class C { has $!y = 1; method acc is rw { $!y }; method g { $!y } }
my $c = C.new;
$c.acc = 9;
is $c.g, 9, 'an unshadowed attribute is still written';

my $a = A.new;
$a.acc = 5;
is $a.m, 5, 'a parent instance is still written';
