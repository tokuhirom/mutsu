use Test;

# From the Color distribution: `clip-to 0, $_, 255 for values %r` writes the
# clamped value back through the `is rw` parameter, like `%r.values` does.
plan 5;

sub clip-to($min, $v is rw, $max) { $v = ($min max $v) min $max }

my %r = a => 300, b => 5;
clip-to 0, $_, 255 for values %r;
is %r<a>, 255, 'statement-modifier for over values %h aliases the value';
is %r<b>, 5, 'in-range value unchanged';

my %h = a => 300;
for values %h { $_ = 7 }
is %h<a>, 7, 'for values %h { $_ = ... } writes back';

my @a = 1, 2;
for values @a { $_ *= 10 }
is-deeply @a, [10, 20], 'for values @a writes back';

my %g = a => 1;
for values %g { clip-to 5, $_, 9 }
is %g<a>, 5, 'block form passing $_ to an is rw parameter';
