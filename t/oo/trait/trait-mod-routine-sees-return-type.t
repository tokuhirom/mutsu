use v6;
use Test;

# The routine a `trait_mod:<is>` candidate receives is the routine `&name`
# answers: it carries the declared return type. Upstream NativeCall builds a
# native call's return marshalling from `$r.signature.returns` (#11203).

plan 6;

my @seen;
multi trait_mod:<is>(Routine $r, :$probe!) {
    @seen.push: $r.returns.^name ~ ' ' ~ $r.signature.returns.^name;
}

sub arrow(Str --> Int) is probe { 1 }
sub returns-trait(Str) returns Str is probe { 'a' }
sub of-trait(Str) of Num is probe { 1e0 }
sub none(Str) is probe { }

is @seen[0], 'Int Int', '--> T is visible to the trait';
is @seen[1], 'Str Str', 'returns T is visible to the trait';
is @seen[2], 'Num Num', 'of T is visible to the trait';
is @seen[3], 'Mu Mu', 'no declared return type reads Mu';
is arrow('x'), 1, 'the routine still runs';
is &arrow.returns.^name, 'Int', '&name agrees';
