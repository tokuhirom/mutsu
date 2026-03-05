use Test;

plan 12;

# Nested capture groups should produce Match objects with subcaptures
ok("abcd" ~~ m/(a(b(c))(d))/, 'Nested captured');
is(~$0, "abcd", 'Nested $0');
is(~$0[0], "bc", 'Nested $0[0]');
is(~$0[0][0], "c", 'Nested $0[0][0]');
is(~$0[1], "d", 'Nested $0[1]');

# Simple backreference: $0 refers to first capture group
ok("aa" ~~ /(a)$0/, 'Simple backref');
is(~$/, "aa", 'Simple backref match');

# x operator has higher precedence than ~
is("a" ~ "b" x 3, "abbb", 'x binds tighter than ~');
is(2 ~ 3 x 4, "23333", 'x binds tighter than ~ (numeric)');

# Replication combined with concatenation
is("hello" ~ "-" x 3, "hello---", 'concat with replication');

# eval-lives-ok should handle large capture variables
{
    my $big = '$' ~ 1 x 100000;
    my $result = EVAL($big);
    ok(!defined($result), 'Large capture var evaluates to undefined');
}

# join on Instance (Match) objects should not crash
"abc" ~~ /(abc)/;
is($0.join(""), "abc", 'join on Match object');
