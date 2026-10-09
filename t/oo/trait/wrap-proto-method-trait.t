use Test;
plan 2;

# #11701: A trait applied to a `proto method` wraps the dispatcher (what
# Method::Protected's `is protected` does).
my @log;
my multi sub trait_mod:<is>(Method:D $m, :$logged!) {
    $m.wrap: method (|c) { @log.push: "in"; my $r = callsame; @log.push: "out"; $r };
}
class T {
    proto method q(|) is logged {*}
    multi method q(Int $x) { @log.push: "body"; $x + 1 }
}
is T.new.q(1), 2, 'trait-wrapped proto method returns the candidate result';
is @log.join(","), "in,body,out", 'the trait wrapper runs around the dispatch';
