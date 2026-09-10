use Test;

# A sigil-prefixed alias (`$<part> = <rule>`) shares the subrule's Match node
# with the rule-name capture. The node must still carry the action method's
# `.made` value when the alias occurs in an ordered alternation, including when
# the same alias name selects different rules in different branches.

plan 8;

grammar Forward {
    token TOP { ^ [ $<part> = <text> || $<part> = <code> ]* $ }
    token text { <-[<]>+ }
    token code { '<' \w+ '>' }
}

grammar Reverse {
    token TOP { ^ [ $<part> = <code> || $<part> = <text> ]* $ }
    token text { <-[<]>+ }
    token code { '<' \w+ '>' }
}

class Actions {
    method text($/) { make 'T:' ~ $/.Str }
    method code($/) { make 'C:' ~ $/.Str }
    method TOP($/) { make $<part>.map({ .made // 'NIL' }).join(',') }
}

my $forward = Forward.parse('ab<x>cd', actions => Actions);
is $forward.made, 'T:ab,C:<x>,T:cd',
    'aliases in the first branch preserve every subrule .made';
is $forward<part>[0].made, 'T:ab', 'the first forward alias carries .made';
is $forward<part>[1].made, 'C:<x>', 'the code branch carries .made';

my $reverse-text = Reverse.parse('abcd', actions => Actions);
is $reverse-text.made, 'T:abcd',
    'a text match in the second branch carries its .made';

my $reverse-code = Reverse.parse('<x>', actions => Actions);
is $reverse-code.made, 'C:<x>',
    'a code match in the second branch carries its .made';

my $reverse-mixed = Reverse.parse('ab<x>cd', actions => Actions);
is $reverse-mixed.made, 'T:ab,C:<x>,T:cd',
    'the same alias can select different action-bearing rules per iteration';
is $reverse-mixed<part>[0].made, 'T:ab',
    'the first reverse iteration retains its own rule action';
is $reverse-mixed<part>[1].made, 'C:<x>',
    'the second reverse iteration retains its own rule action';
