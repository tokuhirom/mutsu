use Test;

# A grammar method called as a subrule may answer a Match another grammar made
# over the same target (`Other.subparse(self.orig, :pos(self.pos))`, a slang's
# `FOREIGN-LANG`): its `.to` is a position in that target, not an offset from
# the call, and the capture's `.made` is what the other grammar's actions made.

plan 4;

grammar Words {
    token TOP  { \s* <word>+ % \s+ \s* }
    token word { \w+ }
}
class Words-actions {
    method TOP($/) { make $<word>.map(~*).List }
}

grammar Outer {
    rule TOP { 'x' <name> \{ <body=.foreign('TOP')> \} }
    token name { \w+ }
    method foreign($rule) {
        Words.subparse(self.orig, :pos(self.pos), :$rule, :actions(Words-actions))
    }
}

my $m = Outer.parse('x foo { a b c }');
ok $m, 'the parse continues after the foreign match';
is ~$m<name>, 'foo', 'the capture before it';
is-deeply $m<body>.made, <a b c>, "the foreign actions' made value";

grammar Tail {
    token TOP { 'x' <body=.foreign> ';' }
    method foreign { Words.subparse(self.orig, :pos(self.pos)) }
}
ok Tail.parse('x a b;'), 'the parse resumes at the end of the foreign match';

# vim: expandtab shiftwidth=4
