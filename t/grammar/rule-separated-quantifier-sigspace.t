use Test;

# In a `rule`, sigspace around a separated quantifier goes where the source
# whitespace is, as in rakudo (#10569):
#   - after the separator atom      -> matched after every separator
#   - between quantifier and `%`    -> matched once after the whole construct
#   - between atom and quantifier   -> matched after every repetition
#   - right after `%`               -> not significant

my @inputs = "a,b", "a, b", "a ,b", "a , b", "a,b ", "a, b ", "a b", "a,b, c";

sub row(Mu \g) { @inputs.map({ g.parse($_) ?? 1 !! 0 }).join }

grammar Issue { rule TOP { <item>* % ',' }; token item { \w+ } }
grammar NoWs  { rule TOP {<item>*%','}; token item { \w+ } }
grammar SepWs { rule TOP {<item>*%',' }; token item { \w+ } }
grammar QuWs  { rule TOP {<item>* %','}; token item { \w+ } }
grammar AtWs  { rule TOP {<item> *%','}; token item { \w+ } }
grammar Brack { rule TOP { <item>+ % [','] }; token item { \w+ } }
grammar Both  { rule TOP { <item> +% ',' }; token item { \w+ } }
grammar Seps  { rule TOP {<item>* %% ','}; token item { \w+ } }

plan 11;

is row(Issue), '11001101', 'the issue: whitespace may follow the separator, not precede it';
is row(NoWs),  '10000000', 'no whitespace: none accepted';
is row(SepWs), '11000001', 'whitespace after the separator atom';
is row(QuWs),  '10001000', 'whitespace before % is trailing whitespace';
is row(AtWs),  '10101000', 'whitespace before the quantifier follows each item';
is row(Brack), '11001101', 'bracketed separator';
is row(Both),  '11111101', 'whitespace before the quantifier and after the separator';
is row(Seps),  '10001000', '%% separator';

grammar Plus { rule TOP {<item> +}; token item { \w } }
ok Plus.parse("a b c"), 'whitespace before + repeats with the atom';
nok Plus.parse("abc"), '... and is required between items';

grammar Lit { rule TOP {'a' * 'b'} }
is ("a b", "ab", "a a b").map({ Lit.parse($_) ?? 1 !! 0 }).join, '101', 'quoted atom before a quantifier';
