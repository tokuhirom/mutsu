use Test;

plan 8;

# Test 1-3: regex alternation with quote chars in character classes
is ('n' ~~ / <["\\/bfnrt]> | 'u' /).Str, 'n',
   'char class with " in alternation matches n';
is ('"' ~~ / <["\\/bfnrt]> | 'u' /).Str, '"',
   'char class with " in alternation matches "';
is ('u' ~~ / <["\\/bfnrt]> | 'u' /).Str, 'u',
   'alternation second branch matches u';

# Test 4-5: grammar alias capture action dispatch
# When using <alias=.rule>, the action method for "rule" should be called,
# not the method for "alias".
grammar TestGrammar {
    token TOP { [ <str> | '\\' <str=.esc> ]* }
    token str { <-[\\]>+ }
    token esc { <[nrt]> }
}

class TestActions {
    method TOP($/) {
        make $<str>>>.made.join;
    }
    method str($/) { make ~$/ }
    method esc($/) { make "ESC:" ~ ~$/ }
}

my $m = TestGrammar.parse('a\\nb', :actions(TestActions.new));
ok $m.defined, 'grammar with alias captures parses';
is $m<str>[1].made, 'ESC:n',
   'aliased capture dispatches to original rule action';

# Test 6-8: individual match elements have correct made values
is $m<str>[0].made, 'a', 'non-aliased capture uses its own action';
is $m<str>[2].made, 'b', 'non-aliased capture after alias uses its own action';
is $m.made, 'aESC:nb', 'TOP made combines all str captures correctly';
