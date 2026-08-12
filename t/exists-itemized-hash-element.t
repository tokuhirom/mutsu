use v6;
use Test;

# `:exists` on a subscript whose target is an itemized (`$%(...)` / `${...}`)
# hash — e.g. the chained `@in[0]{$key}:exists` — popped the element as a
# Scalar-wrapped hash, which no target arm of the exists op matched, so the
# answer was always False. Every subscript READ resolves the Scalar wrapper;
# the exists op now does the same. (Text::CSV's csv(key => ...) gates on
# `@in[0]{$key}:exists` over rows built with `$%( @h Z=> @r )` and died with
# error 4001 on data whose key existed.)

plan 6;

my @b = ${bar => 1, baz => 2}, ${bar => 3};
ok @b[0]{"bar"}:exists, 'chained [0]{key}:exists on an itemized hash element';
ok @b[0]<baz>:exists, 'angle form too';
nok @b[0]{"nope"}:exists, 'missing key still False';

my $x = @b[1];
ok $x{"bar"}:exists, ':exists through a scalar variable holding an itemized hash';
nok $x{"baz"}:exists, 'missing key through the scalar is False';

my $key = "bar";
my @rows = ["1", "2"], ["3", "4"];
my @h = <bar baz>;
my @aoh = @rows.map(-> @r { $%( @h Z=> @r ) });
ok @aoh[0]{$key}:exists, 'the Text::CSV shape: mapped $%(Z=>) rows answer :exists';

done-testing;
