use Test;

# From Text::CodeProcessing (REPLSandbox): `$!save_ctx := $*MAIN_CTX`, then
# `.repl-eval(..., :outer_ctx($!save_ctx))` on the next call. A variable
# `:=`-bound to an object keeps that object's container even after the
# variable it was bound from is declared again by a later loop iteration.
plan 8;

class V { }
class R {
    method q(:$o) { $o.WHAT.raku }
    method s(*%a) { %a<o>.WHAT.raku }
    method p($x)  { $x.WHAT.raku }
}
my $r = R.new;

my (@named, @slurpy, @pos);
{
    my $ctx;
    for 1, 2, 3 {
        my $m = 0;
        @named.push: $r.q(:o($ctx));
        @slurpy.push: $r.s(:o($ctx));
        @pos.push: $r.p($ctx);
        $m := V.new;
        $ctx := $m;
    }
}
is @named.join(','), 'Any,V,V', 'named argument of a method';
is @slurpy.join(','), 'Any,V,V', 'slurpy named argument of a method';
is @pos.join(','), 'Any,V,V', 'positional argument of a method';

# the same through a dynamic variable and an attribute
class S {
    has $.save;
    has $.r = R.new;
    has @.log;
    method ev {
        my $*MC;
        @!log.push: $!r.q(:o($!save));
        $*MC := V.new;
        $!save := $*MC if $*MC;
    }
}
my $s = S.new;
$s.ev for 1..3;
is $s.log.join(','), 'Any,V,V', 'attribute bound to a dynamic variable';

my $x = V.new;
my $y := $x;
ok $y =:= $x, 'a bound object variable is the same container';
my $z = V.new;
nok $z =:= $x, 'a distinct variable is not';
{
    my $n;
    for 1, 2 {
        my $v = "s$_";
        $n := $v;
    }
    is $n, 's2', 'plain value binding follows its source';
}
{
    my $o = V.new;
    my $alias := $o;
    $o = 5;
    is $alias, 5, 'a write to the source is seen through the bound alias';
}
