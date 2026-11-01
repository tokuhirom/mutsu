use Test;
plan 16;

# `.WHO`, `.WHERE` and `.DEFINITE` are Rakudo macros/Mu methods that do not go
# through a user method (ADR-15829). Pinned against `raku`.
class Foo { method WHO { "mine" }; method DEFINITE { "mine" }; method WHERE { "mine" } }

is 42.WHO.^name, 'Stash', '.WHO of a value is a Stash';
is Int.WHO.^name, 'Stash', '.WHO of a type object is a Stash';
is Foo.new.WHO.^name, 'Stash', 'a user method named WHO is not called';
is Foo.WHO.^name, 'Stash', 'nor on the type object';

is 42.DEFINITE, True, '.DEFINITE of a value';
is Int.DEFINITE, False, '.DEFINITE of a type object';
is Foo.new.DEFINITE, True, 'a user DEFINITE is not called on an instance';
is Foo.DEFINITE, False, 'nor on the type object';
is "x".DEFINITE, True, '.DEFINITE of a Str';
is Any.DEFINITE, False, '.DEFINITE of Any';

my $a = Foo.new;
my $b = Foo.new;
is $a.WHERE ~~ Int, True, '.WHERE is an Int';
is $a.WHERE == $a.WHERE, True, '.WHERE is stable for one object';
is $a.WHERE != $b.WHERE, True, '.WHERE differs between objects';
is Foo.new.WHERE.WHAT.gist, '(Int)', 'a user WHERE is not called';
is Int.WHERE ~~ Int, True, '.WHERE of a type object';
is 42.WHERE ~~ Int, True, '.WHERE of a value';
