use Test;

# From the Path::Map distribution: an indexed (or scalar) assignment passed as
# a call argument yields the element's container, so an `is rw` parameter
# binds the caller's storage.
plan 8;

my %h;
my $f = -> $v is rw { $v = $v ~ "!" };
$f(%h<a><b> = "x");
is-deeply %h, {a => {b => "x!"}}, 'nested hash element, code-variable call';

my @a;
$f(@a[1] = "z");
is-deeply @a, [Any, "z!"], 'array element, code-variable call';

my $x;
$f($x = "y");
is $x, "y!", 'scalar assignment, code-variable call';

sub g($v is rw) { $v *= 2 }
my @b;
g(@b[0] = 4);
is @b[0], 8, 'array element, named sub';

my %k;
g(%k<n> = 5);
is %k<n>, 10, 'hash element, named sub';

class Cache {
    has %!c;
    method key { "k" }
    method run($key) { g(%!c{self.key}{$key} = 3); %!c }
}
is-deeply Cache.new.run("q"), {k => {q => 6}}, 'accessor-keyed attribute chain';

# `where * > N` is a WhateverCode that .constraints must apply to the topic.
my $s = sub (Int :$bar! where * > 43) { 1 };
my $c = $s.signature.params[0].constraints;
ok 99 ~~ $c, 'WhateverCode where constraint accepts a matching value';
nok 10 ~~ $c, 'WhateverCode where constraint rejects a non-matching value';
