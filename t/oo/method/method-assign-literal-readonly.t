use Test;

# `X.=meth` is `X = X.meth`, so its target must be a container. A literal is
# not one: rakudo raises X::Assignment::RO naming the value (#18986).

plan 4;

throws-like { "ab12".=uc }, X::Assignment::RO,
    message => 'Cannot modify an immutable Str (ab12)',
    'a Str literal target';
throws-like { (42).=succ }, X::Assignment::RO,
    message => 'Cannot modify an immutable Int (42)',
    'an Int literal target';

my $x = "q";
$x .= uc;
is $x, 'Q', 'a variable target still assigns';

my @a = <a b>;
@a[1] .= uc;
is-deeply @a, ['a', 'B'], 'an element target still assigns';

