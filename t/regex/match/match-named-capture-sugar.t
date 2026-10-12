use Test;

# `$<name>` is sugar for `$/<name>`: both read the named capture of the
# frame's `$/` the same way, whether `$/` is the match the last `~~` set or
# an action method's `$/` parameter.

plan 9;

if 'ab12' ~~ / $<word>=[<alpha>+] $<num>=[\d+] / {
    is ~$<word>, 'ab', '$<name> reads a named capture';
    ok $<word> === $/<word>, '$<name> and $/<name> are the same Match';
    is $<missing>, Nil, 'a capture the match does not have is Nil';
}

'a b c' ~~ / [$<l>=[\w] \s*]+ /;
is $<l>.elems, 3, 'a quantified named capture reads as its list';
is $<l>[1], 'b', 'its elements are the per-iteration matches';

'x' ~~ /y/;
is $<l>, Nil, '$<name> after a failed match is Nil';

grammar Pair {
    token TOP { <key> '=' <value> }
    token key { \w+ }
    token value { \w+ }
}
class PairActions {
    method TOP($/) {
        my $inner = 'zz' ~~ /z/;
        make ~$<key> ~ ':' ~ ~$<value>;
    }
}
is Pair.parse('k=v', :actions(PairActions)).made, 'k:v',
    '$<name> in an action method reads its $/ parameter, not a nested match';

class Seen {
    has @.keys;
    method key($/) { @!keys.push: ~$/ }
    method TOP($/) { make $<key>.made // ~$<key> }
}
my $seen = Seen.new;
is Pair.parse('abc=def', :actions($seen)).made, 'abc',
    '$<name> in an action method sees the child match';
is-deeply $seen.keys, ['abc'], 'the child action ran once';
