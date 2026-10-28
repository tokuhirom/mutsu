use Test;

# Seq.grep on a materialized Seq is lazy: the matcher runs when the result is
# consumed, under the consumer's dynamic scope (Red's ResultSeq.grep fallback,
# Red t/41-fallback).

plan 5;

my @calls;
my $g = (1..4).Seq.grep({ @calls.push($_); $_ %% 2 });
is @calls.elems, 0, 'grep on a Seq has not run the matcher yet';
is $g.list.join(","), "2,4", 'consuming the result filters';
is @calls.elems, 4, 'matcher ran once per element on consumption';

sub mk(@a, &f) {
    my $*WHO = "callee";
    return @a.Seq.grep: &f;
}
my $*WHO = "caller";
my @seen;
my $r = mk([1, 2, 3], { @seen.push($*WHO); True });
$r.list.elems;
is-deeply @seen, ["caller", "caller", "caller"],
    'matcher of a returned Seq.grep sees the consumer dynamic variable';

is (1, 2, 3).Seq.grep(:k, { $_ > 1 }).list.join(","), "1,2", ':k adverb still works';
