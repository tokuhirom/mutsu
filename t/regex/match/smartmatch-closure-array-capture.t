use v6;
use Test;

# Regression (#9429, WhereList): a closure invoked through `~~` read a stale
# `@`-sigiled capture from an earlier closure of the same routine. The
# interpreter-path call wrote the closure's own `@m` back into the caller env,
# and the next call let that leaked caller array shadow its own capture.
# Also pins `Code.ACCEPTS`, which answered a composed-method Sub.

plan 10;

my @seen;
sub mk(*@m) { sub ($v) { @seen.push: ~@m; True } }

5 ~~ mk(1);
5 ~~ mk(3);
is @seen, ['1', '3'], 'each smartmatched closure sees its own slurpy capture';

@seen = ();
my $a = mk(4);
my $b = mk(6);
5 ~~ $b;
5 ~~ $a;
is @seen, ['6', '4'], '... in any call order';

@seen = ();
sub mk-plus(+@m) { sub ($v) { @seen.push: ~@m; True } }
5 ~~ mk-plus([1]);
5 ~~ mk-plus([3]);
is @seen, ['1', '3'], '... for a +@ capture';

@seen = ();
sub mk-copy(*@m) { my @n = @m; sub ($v) { @seen.push: ~@n; True } }
5 ~~ mk-copy(1);
5 ~~ mk-copy(3);
is @seen, ['1', '3'], '... for a my @ capture';

{
    my @m = 9;
    @seen = ();
    5 ~~ mk(1);
    is @seen, ['1'], 'a same-named caller array does not shadow the capture';
    is @m, [9], '... and is not overwritten by it';
}

sub all-items(+@matchers) { sub (\v) { so v.all ~~ all @matchers } }
subset StrArray of Array where all-items Str;
sub foo(+bar where all-items any(Str, Int)) { bar.elems }
ok ['a', 'b'] ~~ StrArray, 'a where-list subset built first';
is foo(42, 'x'), 2, 'a later where-list closure checks its own matchers';

is (sub ($v) { $v > 3 }).ACCEPTS(5), True, 'Code.ACCEPTS calls the code';
is (-> $x { $x }).ACCEPTS(0), 0, "Code.ACCEPTS answers the call's own result";
