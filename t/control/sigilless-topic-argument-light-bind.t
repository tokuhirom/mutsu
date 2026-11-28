use Test;

plan 9;

# `f($_)` into a sigilless (`\v`) parameter, where `$_` holds a plain value,
# binds the value itself (a hash-keys topic is no Scalar, so `v =:= v.self`), and
# a write through the parameter reaches the caller's writable `$_`. The
# positional-light call path now serves this shape (#19455): JSON::Fast's
# `jsonify($_)` / `pretty-associative($_)` calls took the general binder.

sub peek(\v) { v.^name ~ ':' ~ v }
sub same(\v) { v =:= v.self }
sub up(\v)   { v = v.uc; 'ok' }
sub keep(\v) { v = v; 'kept' }

$_ = 'b';
is peek($_), 'Str:b', 'a mainline topic is read as its value';
is up($_), 'ok', 'a write through the parameter succeeds';
is $_, 'B', 'and reaches the mainline topic';

my @seen;
for <x y> { @seen.push: peek($_) }
is @seen.join(','), 'Str:x,Str:y', 'a for topic over a List is read as its value';

my %h = a => 1;
for %h.keys { ok same($_), 'a hash-keys topic is not boxed' }

my @a = <p q>;
up($_) for @a;
is @a.join(','), 'P,Q', 'an Array element topic is written through its container';

$_ = 'z';
is keep($_), 'kept', 'a write of the same value';
is $_, 'z', 'leaves the topic as it was';

# Many calls in a loop keep binding and writing back per call.
my $n = 0;
sub count-it(\v --> Nil) { $n += v }
count-it($_) for 1..100;
is $n, 5050, 'a nested-sub call loop over topics';
