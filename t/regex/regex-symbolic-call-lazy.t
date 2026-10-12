use Test;

plan 6;

# `<::(EXPR)>` computes the rule's name where the cursor reaches the call and
# then runs the rule as a frame, entering its ends one at a time like
# `<name>`: a code block in the callee runs once per end entered, not once per
# end computed. Each expectation was checked against rakudo.

my @log;
grammar Plain {
    regex TOP { <::('r')> 'x' }
    regex r { a+ { @log.push: 'r' ~ $/.to } }
}
ok Plain.parse('aax'), 'a symbolic call of a rule matches';
is @log.join(','), 'r2', '... and its code block ran only at the end that was entered';

@log = ();
grammar Proto {
    regex TOP { <::('p')> 'x' }
    proto regex p {*}
    regex p:sym<a> { a+ { @log.push: 'p' ~ $/.to } }
}
ok Proto.parse('aax'), 'a symbolic call of a proto matches';
is @log.join(','), 'p2', '... and runs the candidate lazily too';

grammar Capture {
    token TOP { <::('r')>+ }
    token r { \d }
}
my $m = Capture.parse('123');
is $m<r>.elems, 3, 'a written-out name still files its Match under the rule name';

grammar Computed {
    token TOP { <::($*name)>+ }
    token r { \d }
}
my $*name = 'r';
is Computed.parse('12').keys.elems, 0, 'a computed name files nothing';
