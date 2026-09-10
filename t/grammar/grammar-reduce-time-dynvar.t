use Test;

# Reduce-time grammar actions: a `$*` dynamic variable written by an action that
# runs while a `<subrule>*` quantifier is still matching must affect the matching
# of subsequent iterations. This models Template::Mustache's `{{=<% %>=}}`
# delimiter change, where a tag finalizer reassigns `($*LEFT,$*RIGHT)`.

plan 6;

grammar G {
    regex TOP { <chunk>* (.*) }
    regex chunk { (\w+?) [ <delim> | <var> ] }
    token delim { $*L '=' (\w+) '=' $*R }
    token var { $*L (\w+) $*R }
}
class A {
    has @.vars;
    method chunk($/) { ($<delim>//$<var>).?made }
    method var($/)   { @!vars.push(~$0) }
    method delim($/) { my $f = { $*L = '<'; $*R = '>'; }; $f() }
}

my $a = A.new;
my $*L = '{';
my $*R = '}';
my $m = G.parse('a{=x=}b<c>', :actions($a));
ok $m.defined, 'parse succeeded across a mid-parse delimiter change';
is $m<chunk>.elems, 2, 'both chunks matched (delimiter change took effect)';
is ~$m<chunk>[0], 'a{=x=}', 'first chunk used the original delimiters';
is ~$m<chunk>[1], 'b<c>', 'second chunk used the new delimiters';
is $a.vars.join(','), 'c', 'var action saw the chunk matched under new delimiters';

# An interpolated scalar matches literally even when it ends/starts with the
# separator-quantifier infix `%`, which must not bind to a preceding `\h*`.
my $*LEFT = '<%';
my $*RIGHT = '%>';
ok ('<% bar %>' ~~ / $*LEFT \h* (\w+) \h* $*RIGHT /).defined,
    'multi-char delimiter containing % interpolates as a literal match';
