use v6;
use Test;

plan 23;

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

# Like bare `$var` interpolation above (and verified against real rakudo,
# which surprised the author of issue #8040's fix): a stored `<@var>`
# assertion re-reads the array on every match, so a reassignment made AFTER
# construction but BEFORE a later match is visible to that match.
{
    my @alts = <cat dog>;
    my $re = rx/ ^ <@alts> $ /;
    @alts = <emu>;
    nok ("cat" ~~ $re).defined, '<@var> follows a reassignment made before a match';
    ok  ("emu" ~~ $re).defined, 'and matches the reassigned value';
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

# --- Bug 2: <$var>-family calls are capture-isolated (raku-verified) ----------

{
    my $inner = rx/ $<d>=(\d+) /;
    my $m = "n=123" ~~ / 'n=' <$inner> /;
    is ~$m, 'n=123', '<$inner> still consumes its text';
    nok $m[0].defined,   '<$var> does not leak positional captures into $/';
    nok $m<d>.defined,   '<$var> does not leak named captures into $/';
}
{
    my $inner2 = rx/ (\d+) /;
    "n=123" ~~ / 'n=' $inner2 /;
    nok $0.defined, 'bare $var regex interpolation does not leak captures either';
}
{
    my @pats = rx/(\d+)/, rx/(x+)/;
    "n=123" ~~ / 'n=' <@pats> /;
    nok $0.defined, '<@pats> alternation does not leak captures either';
}

# --- Issue #8951: a <$re>-interpolated Regex value that is ITSELF a
# closure (its pattern embeds @(...)/{...} code) must resolve that code
# against its OWN defining scope, not whatever is live at the OUTER
# pattern's match site. Before the fix, <$var> extracted only the pattern
# TEXT of a RegexCaptured value, so the embedded @(...) either tripped the
# X::SecurityPolicy check meant for untrusted strings, or (once that check
# was bypassed) silently failed to resolve %named and matched nothing.
{
    my %named = a => 1, b => 2, c => 3;
    my $named-re = rx/ @(%named.keys) /;
    sub normalize(Str $v) {
        my $r = $v;
        $r ~~ s:g/ <$named-re> /X/;
        $r;
    }
    is normalize('has a and b and c'), 'hXs X Xnd X Xnd X',
        '<$re> interpolated from an unrelated scope resolves its OWN captured lexicals';
}

# Same defect, reached through <@var> array-alternation of Regex-valued
# elements instead of a single <$var>.
{
    my %named = a => 1, b => 2;
    my @alts = rx/ @(%named.keys) /, rx/ zz /;
    sub normalize2(Str $v) {
        my $r = $v;
        $r ~~ s:g/ <@alts> /X/;
        $r;
    }
    is normalize2('a zz b'), 'X X X',
        '<@var> array element that is a closure resolves its OWN captured lexicals';
}

# A genuine Regex value's own @(...)/$(...) is trusted -- it can only have
# been written by actual regex literal syntax, never smuggled in through a
# runtime string -- so interpolating it via <$var> must not trip the
# X::SecurityPolicy check that guards against the latter.
{
    my %named = a => 1;
    my $named-re = rx/ @(%named.keys) /;
    lives-ok { 'a' ~~ / <$named-re> / },
        '<$re> interpolating a code-bearing Regex value does not trip X::SecurityPolicy';
}
