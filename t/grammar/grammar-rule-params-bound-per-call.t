use Test;

# A rule's parameters are bound for each call and read where the cursor
# reaches them (ADR-12917 direction 2): `$param` in the body, a code block, a
# `** { … }` count, a call's arguments and a method call's colon arguments all
# see the value of *this* call, also when the same rule is entered again with
# another value further down.

plan 14;

grammar Indent {
    token TOP { <line('  ')> }
    token line(Str $ind) { $ind <word> [ \n $ind <word> ]* }
    token word { \w+ }
}
ok Indent.parse("  ab\n  cd"), 'a parameter matches literally, every time';
nok Indent.parse("  ab\n cd"), 'a different indent does not match';

grammar Nested {
    token TOP { <blk('')> }
    token blk($ind) {
        :my $new;
        <?before $ind $<sp>=[' '+] { $new = $ind ~ $<sp> }>
        $new <item($new)>+ % [ \n $new ]
    }
    token item($i) { '-' \h* [ <sub=.blk($i)> || <val($i, 1)> ] }
    token val($i, Int $min) { \w ** { $min..* } }
}
my $m = Nested.parse("  - a\n  - bb");
ok $m, 'nested calls with values derived from the caller';
is $m<blk><item>[1]<val>.Str, 'bb', 'a parameter read by a `** { }` count';

grammar Recursive {
    token TOP { <lvl('')> }
    token lvl($ind) { $ind \w+ [ \n <lvl($ind ~ ' ')> ]? }
}
ok Recursive.parse("a\n b\n  c"), 'a rule re-entered with a longer value';
nok Recursive.parse("a\n b\n c"), 'the inner call does not see the outer value';

grammar Method {
    token TOP { <t('q')> }
    token t($s) { 'a' <.seen: $s> }
    method seen($s) { $*SEEN = $s; self }
}
my $*SEEN;
ok Method.parse('a'), 'a method call with colon arguments';
is $*SEEN, 'q', 'the method receives the parameter value';

grammar Code {
    token TOP { <t(3)> }
    token t(Int $n) { (\d+) <?{ $0 == $n }> }
}
ok Code.parse('3'), 'a code assertion reads the parameter';
nok Code.parse('4'), 'and rejects another value';

grammar Typed {
    token TOP { <t(5)> }
    token t(Str $s) { $s }
}
throws-like { Typed.parse('5') }, X::TypeCheck::Binding,
    'a type mismatch still fails the binding';

grammar RegexArg {
    token TOP { <t(/b+/)> c }
    regex t($r) { a $r }
}
is RegexArg.parse('abbc')<t>.Str, 'abb', 'a Regex argument is matched as a pattern';

grammar CaseFold {
    token TOP { :i <t('AB')> }
    token t($x) { :i $x }
}
ok CaseFold.parse('ab'), 'an interpolated parameter honours :i';

# A parameter used in a top-level alternation branch of a rule reached through
# another rule's frame (YAMLish's `map-entry`).
grammar Branch {
    token TOP { <outer> }
    token outer { <me('')> }
    token me(Str $i) { 'k' | '?' $i ':' }
}
ok Branch.parse('?:'), 'a parameter in a top-level alternation branch';
