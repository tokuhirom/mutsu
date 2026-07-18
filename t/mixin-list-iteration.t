use Test;

plan 6;

# A role mixin over a list-ish value iterates as the inner value does
# (JSON::Marshal's `_marshal(Positional:D)` recursed to a stack overflow when
# `.map` handed the whole mixin back as one item — JSON::Class t/050-array.t).
role R { method tag { "R" } }

my $x = (1, 2);
$x does R;
is $x.map({ $_.^name ~ ":" ~ $_ }).join("|"), "Int:1|Int:2",
    '.map on a mixed-in list iterates the elements';
is $x.grep(* > 1).join(","), "2", '.grep iterates the elements';
is $x.join("/"), "1/2", '.join iterates the elements';
my $n = 0;
for $x -> $e { $n++ }
is $n, 1, 'for over the itemized mixin scalar iterates once (like a plain $x)';

# A mixin over a scalar stays a single item and keeps its mixin identity.
my $y = 42 but R;
is $y.map({ $_.^name }).join(","), Q[Int+{R}], 'scalar mixin maps as one item';
is $y.tag, "R", 'and keeps the role';

done-testing;
