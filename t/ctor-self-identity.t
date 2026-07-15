use v6;
use Test;

# `self` inside BUILD/TWEAK is the constructed object itself (raku semantics):
# same identity as the value `.new`/`.bless` returns, and mutations made through
# a stored `self` alias after construction stay visible both ways. mutsu
# historically ran each BUILD/TWEAK step against a phantom intermediate
# instance, so a stored `self` went stale. All expectations below verified
# against raku.
#
# (The stored aliases use `=`, not `:=`: a `:=` bind anywhere in a TWEAK body
# trips a pre-existing, unrelated bug where the exit reconcile falsely adopts
# an array attribute's frame value as a `:=` override and drops its per-op
# mirrored `.push` — that reproduces on the pre-change binary too.)

plan 8;

my $tweak-self;
my $build-self;
my $parent-tweak-self;

class P {
    has $.y;
    submethod TWEAK() { $parent-tweak-self = self; }
}
class C is P {
    has $.x is rw;
    has @.items;
    submethod BUILD(:$!x) { $build-self = self; }
    submethod TWEAK() { $tweak-self = self; @!items.push("t"); }
}

my $c = C.new(x => 5, y => 7);
ok $build-self === $c, 'self inside BUILD is the returned object';
ok $tweak-self === $c, 'self inside TWEAK is the returned object';
ok $parent-tweak-self === $c, 'self inside an ancestor TWEAK is the returned object';
is $c.x, 5, 'BUILD-bound attribute is set';
is $c.items, ['t'], 'TWEAK attribute mutation is visible on the returned object';

$c.x = 42;
is $tweak-self.x, 42, 'post-construction mutation is visible through the stored self';

$tweak-self.x = 43;
is $c.x, 43, 'mutation through the stored self is visible on the returned object';

my $b = C.bless(x => 9);
is $b.x, 9, 'bless threads the same live object through BUILD/TWEAK';

done-testing;
