use Test;
use MONKEY-SEE-NO-EVAL;

plan 3;

sub load-pod($code) {
    EVAL "$code\n\$=pod";
}

my $pod = load-pod(q:to/CODE/);
=begin pod
Example
=end pod
CODE

isa-ok $pod, Array, 'EVAL of a bare $=pod returns the Pod array';
is $pod.elems, 1, 'the EVAL result keeps the collected Pod block';
isa-ok $pod[0], Pod::Block::Named, 'the collected block keeps its Pod type';
