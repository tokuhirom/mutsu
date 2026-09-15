use Test;

# `[ ... ]` and `( ... )` are the same construct with a different bracket, and
# a regex group body reads by one set of rules: a backslash escape never moves
# the depth, a quoted string's content is literal, a character class's members
# are literal while an assertion holds a nested regex, and `#` outside any
# `<...>` starts a comment.
#
# The `(...)` scanner learned all of that; the `[...]` one counted brackets raw,
# so a quoted `]` closed the group one level down and the ENCLOSING construct is
# what failed -- `Code::Coverable` (and `Code::Coverage` / `Test::Coverage`
# through it) reported its error at a `while` fifteen lines above the
# `elsif $line ~~ /^ \s* [[')' | ']'] \s+]? ... $/` that actually broke (#7954).
# Both scanners are one function now.

plan 18;

# A quoted bracket inside a NESTED group of the same kind.
is (']x' ~~ /[[']']]/).Str, ']', 'a quoted ] in a nested [ ] group';
is (']x' ~~ /[[ ']' x]]/).Str, ']x', 'and the group continues past it';
is ('[x' ~~ /[['[']]/).Str, '[', 'a quoted [ in a nested [ ] group';
is (']]x' ~~ /[ ']]' x]/).Str, ']]x', 'a quoted ]] does not close the group twice';
is ('a)b' ~~ /( ( ')' ) )/).Str, ')', 'a quoted ) in a nested ( ) group';

# The reduced shape from the distribution: an alternation of quoted brackets,
# nested one level.
is ('} else {' ~~ /^ \s* [[')' | ']' | '}'] \s+]? else/).Str, '} else',
    'an alternation of quoted brackets inside an optional group';

# Double quotes behave the same as single ones.
is (']x' ~~ /[[ "]" x]]/).Str, ']x', 'a double-quoted ] in a nested group';

# A character class's members are literal in BOTH bracket kinds.
is ('a.b' ~~ /[ \w <[.)]> \w ]/).Str, 'a.b', 'a ) inside a char class does not nest (in [ ])';
is ('a.b' ~~ /( \w <[.)]> \w )/).Str, 'a.b', 'a ) inside a char class does not nest (in ( ))';
is ('a]b' ~~ /[ \w <[.\]]> \w ]/).Str, 'a]b', 'an escaped ] inside a char class';
ok ('x' ~~ /[<-['"]>]/).so, 'a quote inside a negated char class is a member, not a delimiter';

# An assertion holds a nested regex, so a quote there really does open a string.
ok ('ab' ~~ /[ a <!before '>'> b ]/).so, 'a quoted > inside a lookahead, in a [ ] group';
ok ('ab' ~~ /( a <!before '>'> b )/).so, 'a quoted > inside a lookahead, in a ( ) group';

# A backslash escape never moves the depth, at any nesting level.
is ('a]b' ~~ /[[ \w \] \w ]]/).Str, 'a]b', 'an escaped ] inside a nested [ ] group';

# A comment runs to end of line inside either group kind.
my $with-comment = ']x' ~~ /[      # a comment with ] and ) in it
    ']' x
]/;
is $with-comment.Str, ']x', 'a comment inside a [ ] group';

# Plain nesting, with nothing quoted, is unchanged.
is ('ab' ~~ /[[a][b]]/).Str, 'ab', 'plain nested [ ] groups still nest';
is ('b' ~~ /[[a]|[b]]/).Str, 'b', 'alternation between nested groups';
is ('ab' ~~ /[(a)(b)]/)[1].Str, 'b', 'capture groups inside a [ ] group still capture';
