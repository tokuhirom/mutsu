use Test;

# `$?LINE` inside a regex code block is the line the block is written on: in a
# regex literal, and in the code of an anonymous token/regex/rule term (#16603).

plan 6;

my @seen;
'a' ~~ /a {
    @seen.push($?LINE);
}/;
is @seen[0], 10, 'literal: the line of the statement inside the block';

my $t = token {
    a { @seen.push($?LINE) }
};
'a' ~~ /<$t>/;
is @seen[1], 15, 'anonymous token: the line inside the term';

my $r = regex { a { @seen.push($?LINE) } };
'a' ~~ /<$r>/;
is @seen[2], 20, 'anonymous regex on one line';

'a' ~~ /<$t>/;
is @seen[3], 15, 'a second match keeps the written line';

'a' ~~ /a { @seen.push($?LINE) } b?/;
is @seen[4], 27, 'one-line literal';

my $q = rule {
    a {
        @seen.push($?LINE)
    }
};
'a' ~~ /<$q>/;
is @seen[5], 32, 'rule: a block spanning lines';

done-testing;
