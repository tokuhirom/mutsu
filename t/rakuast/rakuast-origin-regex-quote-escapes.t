use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a double-quoted string in a
# regex is placed when it holds single-character escapes (`\n`, `\e`, `\"`),
# and the regex atoms after it are placed too.

plan 4;
sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) if $class ~~ /Regex|Quoted|StrLiteral/ }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}
is placed(Q[/ "a\n" b /]),
    'QuotedRegex@0-11 Regex::Sequence@2-10 Regex::WithWhitespace@2-8 Regex::Quote@2-7 QuotedString@2-7 StrLiteral@3-6 Regex::WithWhitespace@8-10 Regex::Literal@8-9',
    'a newline escape';
is placed(Q{/^ "\e[34m# a comment\e[0m\n"/}),
    'QuotedRegex@0-30 Regex::Sequence@1-29 Regex::WithWhitespace@1-3 Regex::Anchor::BeginningOfString@1-2 Regex::Quote@3-29 QuotedString@3-29 StrLiteral@4-28',
    'escape and comment characters';
is placed(Q[/ "say \"hi\"" /]),
    'QuotedRegex@0-16 Regex::WithWhitespace@2-15 Regex::Quote@2-14 QuotedString@2-14 StrLiteral@3-13',
    'escaped quotes';
is placed(Q[/ "\t" | "\r" /]),
    'QuotedRegex@0-15 Regex::Alternation@2-14 Regex::WithWhitespace@2-7 Regex::Quote@2-6 QuotedString@2-6 StrLiteral@3-5 Regex::WithWhitespace@9-14 Regex::Quote@9-13 QuotedString@9-13 StrLiteral@10-12',
    'tab and return escapes in an alternation';
