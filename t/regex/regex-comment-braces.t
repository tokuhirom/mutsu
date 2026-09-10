use Test;

plan 4;

# Test that # comments with braces inside regex/token bodies
# don't break parsing or matching

# Basic: comment with braces in token body
grammar G1 {
    token TOP {
        # comment with {{ braces
        'hello'
    }
}
ok G1.parse('hello'), 'token with comment containing braces parses correctly';

# Proto regex with dynamic vars and comments containing braces
my ($*LEFT, $*RIGHT) = ('{{', '}}');
grammar G2 {
    regex TOP { ^ <hunk>* $ }
    regex hunk { <tag> }
    proto regex tag { <...> }
    token tag:sym<var> { $*LEFT \h* (\w+) \h* $*RIGHT }
    token tag:sym<mmm> {
        <?{ $*LEFT eq '{{' and $*RIGHT eq '}}' }>

        # Use $*LEFT here to force longer token than sym<var>.
        # Otherwise, '{{' might be tried before '{{{'!

        $*LEFT '{' \h* (\w+) \h* '}' $*RIGHT
    }
}
my $m = G2.parse('{{{ bar }}}');
ok $m, 'proto regex with comments containing braces matches';
is ~$m, '{{{ bar }}}', 'matched the full triple-mustache';

# Ensure the longer (mmm) variant wins over var
my $m2 = G2.parse('{{ foo }}');
ok $m2, 'var variant also still works';
