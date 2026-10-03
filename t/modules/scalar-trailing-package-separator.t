use Test;

plan 4;

# Rakudo accepts a trailing :: on a scalar before either a postfix or a term
# boundary. The current oracle resolves it to the same scalar as the bare name.
my $pkg = Int;
is $pkg::.^name, 'Int', 'trailing :: is accepted before a method call';
is $pkg::.WHO.^name, 'Stash', 'trailing :: works before .WHO';
ok $pkg:: =:= $pkg, 'trailing :: reads the same scalar';

my $pkg2 = $pkg::.WHO;
is $pkg2.^name, 'Stash', 'the value can initialize another scalar';
