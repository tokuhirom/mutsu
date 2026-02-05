use Test;
plan 12;

# Basic embedded comments with different brackets
ok #`[comment] 1, 'embedded comment with []';
ok #`(comment) 1, 'embedded comment with ()';
ok #`{comment} 1, 'embedded comment with {}';
ok #`<comment> 1, 'embedded comment with <>';

# Multiline embedded comments
ok #`[
    Multiline
    comment
] 1, 'multiline embedded comment';

# Nested brackets
ok #`((nested)) 1, 'nested parens in embedded comment';
ok #`{{nested}} 1, 'nested braces in embedded comment';
ok #`[[nested]] 1, 'nested brackets in embedded comment';
ok #`<<nested>> 1, 'nested angle brackets in embedded comment';

# Embedded comment in expression
my $x = 3 #`(comment) + 2;
is $x, 5, 'embedded comment in arithmetic expression';

# Unicode brackets
ok #`『comment』 1, 'embedded comment with WHITE CORNER BRACKET';
ok #`「comment」 1, 'embedded comment with CORNER BRACKET';
