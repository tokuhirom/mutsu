use Test;

plan 9;

# A routine whose `$/` is a parameter keeps it across a regex smartmatch in
# its body: the match answers, but `$/` (and `$0`) stay the argument. This is
# the grammar-action idiom `method term($/) { make do given ~$/ { when
# $_ ~~ /.../ { ... } } }` (EBNF::Grammar).
sub failed-match($/) { my $r = "x" ~~ /y/; $/ }
isa-ok failed-match("a" ~~ /a/), Match, 'a failed match leaves the parameter';
sub ok-match($/) { my $r = so "b" ~~ /(b)/; [$r, ~$/, ~$0] }
is-deeply ok-match("a" ~~ /(a)/), [True, 'a', 'a'], 'a successful match does too, $0 included';
sub in-given($/) { my $r = do given "x" { when $_ ~~ /y/ { 1 }; default { 2 } }; [$r, ~$/] }
is-deeply in-given("a" ~~ /a/), [2, 'a'], 'inside given/when';
sub in-for($/) { for "x" { if $_ ~~ /x/ { } }; ~$/ }
is in-for("a" ~~ /a/), 'a', 'inside a for loop';

grammar G {
    token TOP { <word> }
    token word { \w+ }
}
class A {
    method TOP($/) { make $<word>.made }
    method word($/) {
        my $s = ~$/;
        make do given $s { when $_ ~~ /^ <[aeiou]>/ { "vowel:$_" }; default { "other:$_" } }
    }
}
is G.parse("apple", :actions(A)).made, 'vowel:apple', 'make after a matching when';
is G.parse("pear", :actions(A)).made, 'other:pear', 'make after a non-matching when';

# Without a `$/` parameter the match still sets `$/`.
sub plain { "a" ~~ /a/; "x" ~~ /y/; $/ }
nok plain().defined, 'an ordinary routine sees the failed match';
sub plain2 { if "b" ~~ /(b)/ { ~$0 } }
is plain2(), 'b', 'and the captures of a successful one';
sub own-slash { my $/; "c" ~~ /c/; ~$/ }
is own-slash(), 'c', 'a `my $/` is still assigned';
