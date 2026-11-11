use Test;

# A bracket or brace inside a string literal of a code block (`{ ... }`,
# `<?{ ... }>`) is code, not regex structure: it neither opens nor closes the
# `[ ... ]` group around it (#16888, EEL's `eel-group`, which a `:my` sent down
# the structural parser).

plan 8;

grammar Group {
    token TOP {
        $<open>=[ '[' | '(' ]
        :my $x;
        'x'
        [ <?{ $<open>.Str eq '[' }> ']' | <?{ $<open>.Str eq '(' }> ')' ]
    }
}
ok Group.parse('(x)'), 'the second branch of a group whose code holds a quoted [';
ok Group.parse('[x]'), 'the first branch';
nok Group.parse('(x]'), 'a mismatched close';

my $r = rx/ :my $y; 'x' [ <?{ '[' eq 'z' }> 'q' | ')' ] /;
ok 'x)' ~~ $r, 'a quoted [ in an assertion of an rx//';
my $s = rx/ :my $y2; 'x' [ <?{ ']' eq 'z' }> 'q' | ')' ] /;
ok 'x)' ~~ $s, 'a quoted ]';
my $t = rx/ :my $y3; 'x' [ { my $c = '}' } 'q' | ')' ] /;
ok 'x)' ~~ $t, 'a quoted } in a plain code block';
my $u = rx/ :my $y4; 'x' [ <?{ my $isn't = 1; $isn't == 2 }> 'q' | ')' ] /;
ok 'x)' ~~ $u, "an apostrophe inside an identifier opens no string";

my @seen;
grammar Order {
    token TOP {
        $<open>=[ '[' | '(' ] :my $x; 'x'
        [ <?{ @seen.push(1); $<open>.Str eq '[' }> ']' | <?{ @seen.push(2); $<open>.Str eq '(' }> ')' ]
    }
}
Order.parse('(x)');
is-deeply @seen, [2], 'longest-token matching skips the branch whose literal cannot match';
