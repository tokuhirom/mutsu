use Test;

# A recursive `regex` (not `token`) with a frugal `[ <A> | . ]*?` body
# (#9596). Each iteration of the frugal loop must try the LTM winner `<A>`
# before `.` -- frugality only changes how many iterations are preferred --
# and a nested `<A>` must hand its ends to the caller one at a time instead
# of enumerating every end of every nested call up front, which was
# exponential in the nesting depth.

plan 17;

grammar Braces {
    token TOP { <A> }
    regex A { '{' [ <A> | . ]*? '}' }
}

{
    my $m = Braces.parse('{{a}{b}}');
    ok $m, 'nested blocks parse';
    is ~$m<A>, '{{a}{b}}', 'the outer <A> spans the whole input';
    is $m<A><A>.map(~*).join(','), '{a},{b}', 'each nested block is its own <A>';
}

{
    my $m = Braces.parse('{{a}}');
    ok $m, 'a single nested block parses';
    is ~$m<A><A>[0], '{a}', 'the nested block is an <A>, not `.` characters';
}

{
    my $m = Braces.subparse('{{a}{b}}');
    is ~$m, '{{a}{b}}', 'subparse takes the whole nested block';
    is $m<A><A>.elems, 2, 'subparse sees both nested blocks';
}

{
    my grammar Empty {
        token TOP { <A> }
        regex A { '{' [ <A> | . ]*? '}' }
    }
    my $m = Empty.subparse('{{}}');
    is ~$m, '{{}}', 'an empty nested block is an <A> too';
    is ~$m<A><A>[0], '{}', 'and is captured as one';
}

{
    my $s = '{' ~ ('{ab{c}d}' x 12) ~ '}';
    my $m = Braces.parse($s);
    ok $m, 'a long input parses';
    is $m<A><A>.elems, 12, 'every top-level nested block is its own <A>';
    is $m<A><A>[0]<A>[0].Str, '{c}', 'and so is every inner one';
}

{
    my grammar Greedy {
        token TOP { <A> }
        regex A { '{' [ <A> | . ]* '}' }
    }
    my $m = Greedy.parse('{{a}{b}}');
    ok $m, 'the greedy form parses';
    is ~$m<A><A>[0], '{a}{b}', 'and its first <A> grows as far as it can';
}

{
    my grammar Frugal {
        regex TOP { <A> }
        regex A { '{' [ <A> | . ]*? '}' }
    }
    my $m = Frugal.parse('{{a}{b}}');
    ok $m, 'a regex TOP parses too';
    is $m<A><A>.elems, 2, 'with both nested blocks as <A>';
}

# Left recursion through a `regex` is still grown with a seed.
{
    my grammar LeftRec {
        token TOP { <expr> }
        regex expr { <term> | <expr> '+' <term> }
        regex term { \d }
    }
    ok LeftRec.parse('1+2+3'), 'left recursion in a regex still parses';
}
