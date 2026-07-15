use v6;
use Test;

plan 4;

# A user $*OUT override whose print writes a captured-outer / our variable
# must keep that write when say/print/put/note fires inside a *method* body.
# Regression pin for advent2010-day14.t: Say/Put/Print/Note opcodes were not
# counted as calls, so can_skip_merge restored a stale env snapshot on method
# exit and dropped the write.

our $out = '';
sub capture($code) {
    temp our $out = '';
    my $*OUT = class { method print(*@args) { $out ~= @args.join } }
    $code();
    return $out;
}

class Sayer {
    method one   { say "from-method" }
    method multi { say "a"; say "b" }
    method puts  { print "p"; put "q" }
}

is capture({ Sayer.new.one }), "from-method\n", 'say inside method reaches $*OUT override';
is capture({ Sayer.new.multi }), "a\nb\n", 'successive says accumulate';
is capture({ Sayer.new.puts }), "pq\n", 'print/put inside method';

# capture-said idiom from advent2010-day14 (lexical, not our)
sub capture-said($code) {
    my $output = '';
    my $*OUT = class { method print(*@args) { $output ~= @args.join } }
    $code();
    return $output.lines;
}
class C { method sing { say "row"; say "boat" } }
is-deeply capture-said({ C.new.sing }), ("row", "boat"), 'lexical accumulation across method says';
