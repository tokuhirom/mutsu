use v6;
use Test;

# `say` (render_gist_value) used to fall back to the native gist on ANY
# error from `.gist`, which swallowed a genuine user exception thrown while
# `.gist` forced a lazy Seq — `say f().list` printed an empty line and
# execution continued. Only a dispatch failure (no `.gist` candidate) may
# fall back now. Expected values verified against raku.

plan 4;

# A die inside a routine-created gather propagates through say's gist force.
my $died = False;
try {
    sub f() { gather { take 1; die "boom-after-take" } }
    say f().list;
}
$died = True if $!.defined && $!.message eq "boom-after-take";
ok $died, 'say propagates a die raised while .gist forces a lazy gather';

# A die from a user-defined .gist propagates too.
my $died2 = False;
try {
    class G { method gist() { die "gist-boom" } }
    say G.new;
}
$died2 = True if $!.defined && $!.message eq "gist-boom";
ok $died2, 'say propagates a die from a user-defined .gist';

# Ordinary say still works after the narrowing (dispatch fallback intact).
my $out = (1, 2, 3).gist;
is $out, '(1 2 3)', 'plain .gist still renders normally';

# note (stderr gist path) shares render_gist_value: a plain value renders.
lives-ok { say [1, { a => 2 }] }, 'say of nested structures still lives';

done-testing;
