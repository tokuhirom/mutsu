use v6;
use Test;

plan 7;

# An unsupplied `&cb?` role parameter is the Callable type object, as it is for
# a sub, so the role can hand it on to a parameterisation of itself.
role R[&rel1, &rel2?, Str :$model] {
    method second { &rel2 }
    method second-defined { &rel2.defined }
    method second-is-callable { &rel2 ~~ Callable }
    method first-result($x) { rel1($x) }
    method again { R[&rel1, &rel2, :$model] }
}

my &double = -> $x { $x * 2 };
my $plain = (class { }).new but R[&double, :model<M>];
is $plain.second.^name, 'Callable', 'an unsupplied &rel2? reads as Callable';
nok $plain.second-defined, 'and is not defined';
ok $plain.second-is-callable, 'and smartmatches Callable';
is $plain.first-result(21), 42, 'the supplied &rel1 still works';
lives-ok { $plain.again }, 'the unsupplied parameter can be passed to R[...] again';
is $plain.again.first-result(4), 8, 'the re-parameterised role works';

my $both = (class { }).new but R[&double, -> $y { $y + 1 }, :model<N>];
nok $both.second-is-callable.not, 'a supplied &rel2 is the callable';
