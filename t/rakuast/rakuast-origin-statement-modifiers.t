use Test;

# A statement modifier and the expression it modifies carry the origins
# Rakudo gives them (ADR-10723 Stage 2): the modifier from its keyword to the
# end of its condition, the expression from the statement's start to just
# before the keyword. Equal terms before and after the keyword keep their own
# ranges. Ranges measured on rakudo 2026.09.

plan 23;

sub range($node) { with $node.origin { "{.from}..{.to}" } else { 'none' } }
sub parts($node) { my @c; $node.visit-children(-> $c { @c.push: $c }); @c }

my $src = q:to/END/;
say $*x if $*x;
say 1 for 1, 2;
say(1) unless $*x;
$*y while $*x;
$*y until $*x;
say 3 given $*x;
say 4 with $*x;
say 5 without $*x;
say 1  if  $*x;
END
my @s = $src.AST.statements;

# `say $*x if $*x`: the argument and the condition are the same term.
my @p = parts(@s[0]);
is range(@p[0]), '0..7', 'the modified call';
is range(parts(parts(@p[0])[1])[0]), '4..7', 'its argument keeps the first range';
is range(@p[1]), '8..14', 'an if modifier';
is range(parts(@p[1])[0]), '11..14', 'its condition keeps the second range';

@p = parts(@s[1]);
is range(@p[0]), '16..21', 'the call modified by a for';
is range(@p[1]), '22..30', 'a for modifier';
is range(parts(@p[1])[0]), '26..30', 'its list';

@p = parts(@s[2]);
is range(@p[0]), '32..38', 'a call with parentheses';
is range(@p[1]), '39..49', 'an unless modifier';

@p = parts(@s[3]);
is range(@p[0]), '51..54', 'a variable modified by a while';
is range(@p[1]), '55..64', 'a while modifier';

@p = parts(@s[4]);
is range(@p[1]), '70..79', 'an until modifier';

@p = parts(@s[5]);
is range(@p[0]), '81..86', 'the call modified by a given';
is range(@p[1]), '87..96', 'a given modifier';

@p = parts(@s[6]);
is range(@p[1]), '104..112', 'a with modifier';

@p = parts(@s[7]);
is range(@p[1]), '120..131', 'a without modifier';

@p = parts(@s[8]);
is range(@p[0]), '133..138', 'extra whitespace stays out of the call';
is range(@p[1]), '140..147', 'and the modifier starts at its keyword';

# A comment between the parts is part of the call's range, as in Rakudo.
my $commented = "say 1 #c\n if \$*x;".AST.statements[0];
is range(parts($commented)[0]), '0..8', 'a comment before the keyword is part of the call';
is range(parts($commented)[1]), '10..16', 'the modifier still starts at its keyword';

# Semantics are unchanged.
my @out;
EVAL('@out.push(1) if True; @out.push(2) unless True; @out.push($_) for 3, 4');
is @out.join(','), '1,3,4', 'modifiers still run';
is EVAL('my $n = 0; $n++ while $n < 3; $n'), 3, 'a while modifier still loops';
is EVAL('my $r; $r = $_ given 5; $r'), 5, 'a given modifier still topicalizes';
