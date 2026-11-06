use Test;
use nqp;

# A callback the unit of `REPL.repl-eval` runs writes the caller's lexical
# (#16977): the `$*OUT` the caller rebound is the unit's dynamic `$*OUT`, and
# the role method assigns a file-level `my`.
plan 4;

my $buf = '';
my $repl = REPL.new(nqp::getcomp("Raku"), {});

sub run($code) {
    my $*OUT = $*OUT but role { method print(*@a) { $buf ~= @a.join; True } };
    my $exc;
    $repl.repl-eval($code, $exc, :interactive(1));
}

run('print "p"');
is $buf, 'p', 'a role method on the caller\'s $*OUT writes a file-level lexical';
run('print "q"');
is $buf, 'pq', 'and again';

my $other = 'keep';
sub run-other($code) {
    my $*OUT = $*OUT but role { method print(*@a) { $buf ~= @a.join; True } };
    my $exc;
    $repl.repl-eval($code, $exc, :interactive(1));
}
run-other('print "r"');
is $other, 'keep', 'an unrelated caller lexical is untouched';

# The unit still does not see the caller's lexicals.
my $hidden = 42;
my $res;
{
    my $exc;
    $res = $repl.repl-eval('my $x = 1; $x', $exc, :interactive(1));
}
is $buf, 'pqr', 'the unit run leaves the buffer as the callbacks made it';
