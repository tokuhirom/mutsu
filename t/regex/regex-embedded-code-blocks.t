use v6;
use Test;

# What an embedded `{ ... }` code block inside a regex/grammar can see, and
# when a block's implicit/explicit `state` restarts.

plan 34;

# --- $0 / $<name> mid-match are Match objects, not raw strings ------------

my $zero-what;
my $zero-gist;
my $zero-from;
my $zero-to;
'123' ~~ / (\d) { $zero-what = $0.^name; $zero-gist = $0.gist; $zero-from = $0.from; $zero-to = $0.to; } \d+ /;
is $zero-what, 'Match', '$0 inside an embedded block is a Match';
is $zero-gist, '｢1｣',   'and gists like a Match, not like a Str';
is $zero-from, 0,       'with the capture start offset';
is $zero-to,   1,       'and the capture end offset';

my $named-what;
my $named-gist;
'abc' ~~ / $<x>=(\w) { $named-what = $<x>.^name; $named-gist = $<x>.gist; } \w+ /;
is $named-what, 'Match', '$<name> inside an embedded block is a Match';
is $named-gist, '｢a｣',   'and gists like a Match';

# `.made` on a mid-match capture answers (Nil) rather than dying the way a Str
# would on a method it does not have.
my $made-ok = False;
'abc' ~~ / $<y>=(\w) { $made-ok = !$<y>.made.defined; } \w+ /;
ok $made-ok, 'a mid-match named capture answers .made';

# An unmatched optional capture is Nil mid-match, exactly as in the final $/.
my $opt-defined = True;
'b' ~~ / (a)? (b) { $opt-defined = $0.defined; } /;
nok $opt-defined, 'an unmatched optional capture reads Nil inside the block';

# --- $/ is the match so far ----------------------------------------------

my @slash-so-far;
'abcd' ~~ / . { @slash-so-far.push: ~$/ } . { @slash-so-far.push: ~$/ } . /;
is @slash-so-far, ['a', 'ab'], '$/ inside a block is the match so far';

# --- $¢ (the cursor) ------------------------------------------------------

my ($cursor-name, $cursor-from, $cursor-pos);
'abc' ~~ /. { $cursor-name = $¢.^name; $cursor-from = $¢.from; $cursor-pos = $¢.pos } ./;
is $cursor-name, 'Match', '$¢ inside an embedded block is a Match';
is $cursor-from, 0,       '$¢ reports the match start';
is $cursor-pos,  1,       'and the current cursor position';

# `$$` is the end-of-line anchor even when a code block follows it with no
# intervening space: `.$${ ... }` is anchor + block, not a `${...}` variable.
my $c;
'abc' ~~ /.$${ $c = $¢ }/;
is $c.gist, '｢c｣', '$$ directly followed by a code block keeps its anchor meaning';

my $ran = 0;
ok ('abc' ~~ /.$${ $ran = 1 }/).defined, 'the anchored match still succeeds';
is $ran, 1, 'and the block ran';

# --- a `{ ... }` in a string is its own block: its `state` restarts --------

sub count-it { "Count is {$++}" }
is count-it(), 'Count is 0', 'an interpolated {$++} starts at 0';
is count-it(), 'Count is 0', 'and restarts on the next call (traps.rakudoc)';

sub count-explicit { "N { state $n = 0; $n++ }" }
is count-explicit(), 'N 0', 'an explicit `state` in an interpolation block starts at 0';
is count-explicit(), 'N 0', 'and restarts too';

my @loop-interp;
for ^3 { @loop-interp.push: "{$++}" }
is @loop-interp, ['0', '0', '0'], 'the interpolation block re-clones each iteration';

# The documented workarounds DO count, because neither introduces a block.
sub count-ctx  { "Count is $($++)" }
is count-ctx(), 'Count is 0', '$($++) counts from 0';
is count-ctx(), 'Count is 1', 'and keeps counting';

sub count-concat { "Count is " ~ $++ }
is count-concat(), 'Count is 0', '~ $++ counts from 0';
is count-concat(), 'Count is 1', 'and keeps counting';

# --- the same rule for ordinary nested blocks -----------------------------

sub nested-tail { { state $c = 0; $c++ } }
is nested-tail(), 0, 'a tail bare block restarts its state per call';
is nested-tail(), 0, 'and again';

sub nested-do { do { state $c = 0; $c++ } }
is nested-do(), 0, 'a `do { }` block restarts its state per call';
is nested-do(), 0, 'and again';

# A loop BODY is the block the loop statement clones once, so its state is
# shared across iterations of one execution and restarts on the next.
sub loop-body {
    my @out;
    for ^3 { state $n = 0; @out.push: $n; $n++ }
    @out;
}
is loop-body(), [0, 1, 2], 'a loop body shares its state across iterations';
is loop-body(), [0, 1, 2], 'and restarts when the loop statement runs again';

# A `$` written directly in a routine body belongs to that body, which is
# cloned once per call, so it keeps counting.
sub body-anon { my $v = $++; $v }
is body-anon(), 0, 'a bare $ in a routine body counts from 0';
is body-anon(), 1, 'and keeps counting across calls';

# --- a grammar method called as an assertion sees an instance -------------

grammar MarkedG {
    has Bool $.invalid;
    token TOP { <a> }
    token a { \w+ <.mark> }
    method mark() {
        $!invalid = True;
        self;
    }
}
my $m = MarkedG.parse('hello');
ok $m.defined, 'a <.method> assertion returning self is a zero-width success';
is ~$m, 'hello', 'and the parse covers the whole string';
