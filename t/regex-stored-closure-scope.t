use v6;
use Test;

plan 13;

# --- Bug 1: a stored regex keeps its defining scope ---------------------------

sub make() { my $word = 'abc'; return rx/ $word /; }
my $r = make();
ok ("xx abc yy" ~~ $r).defined, 'escaped regex still resolves its lexical (main repro)';

sub mk($w) { my $word = $w; return rx/ $word /; }
my $ra = mk('aaa');
my $rb = mk('bbb');
ok ("aaa" ~~ $ra).defined, 'first call keeps its own value';
ok ("bbb" ~~ $rb).defined, 'second call keeps its own value';
nok ("bbb" ~~ $ra).defined, 'calls do not share a frame (snapshot-vs-shared-frame discriminator)';

my @res;
for <one two> -> $w { @res.push: rx/ $w /; }
ok ("one" ~~ @res[0]).defined, 'loop iteration 0 keeps its value';
ok ("two" ~~ @res[1]).defined, 'loop iteration 1 keeps its value';
nok ("two" ~~ @res[0]).defined, 'iterations do not share';

my %h;
sub outer { my $x = 'qq'; sub inner { return rx/ $x /; }; %h<r> = inner(); }
outer();
ok ("a qq b" ~~ %h<r>).defined, 'nested sub, stored in a hash, matched later';

sub mk5 { my $pat = 'ab'; return rx/ <$pat> /; }
ok ("xaby" ~~ mk5()).defined, '<$var> assertion form survives the frame';

# Match-time evaluation: mutation after construction is visible (raku-verified).
{
    my $pat = 'abc';
    my $re = rx/ $pat /;
    $pat = 'zzz';
    nok ("abc" ~~ $re).defined, 'interpolation sees the mutated value, not a snapshot (1)';
    ok  ("zzz" ~~ $re).defined, 'interpolation sees the mutated value, not a snapshot (2)';
}

# The code-bearing capture must be a live cell, not a stale snapshot (W5/W6).
{
    my $x = 1;
    my $re = rx/ abc <?{ $x == 2 }> /;
    $x = 2;
    ok ("abc" ~~ $re).defined, 'embedded code sees a same-scope mutation after construction';
}
sub mk6 { my $w = 'no'; my $r2 = rx/ abc <?{ $w eq 'yes' }> /; $w = 'yes'; return $r2; }
ok ("abc" ~~ mk6()).defined, 'embedded code sees a mutation made before the frame died';
