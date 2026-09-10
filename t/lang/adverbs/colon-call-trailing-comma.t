use v6;
use Test;

# A colon method-call argument list (`$obj.method: a, b`) may carry a trailing
# comma before the close of an enclosing group — `(obj.m: 1, 2,)` and
# `[obj.m: 1, 2,]` — just as it may before `;` / `}`. The closer belongs to the
# surrounding group, not the argument list. mutsu previously tried to parse
# another argument after the comma, hit the `)` / `]`, and failed with
# "right-hand expression after '='" (the parse blocker behind File-TreeBuilder,
# whose `(Node::Grammar.new.parse: $text, :actions(...),) or die` uses it).

plan 7;

class G {
    method m(*@a) { @a.join(',') }
}
class C {
    method !p(*@a) { @a.sum }
    method go { (self!p: 1, 2, 3,) }
}

# Trailing comma before a closing paren.
is (G.new.m: 1, 2,), '1,2', 'colon-call trailing comma before ) ';

# Trailing comma before a closing bracket.
is-deeply [G.new.m: 1, 2,], ['1,2'], 'colon-call trailing comma before ]';

# Multi-line, with a nested colon-call arg and `or die` after the group.
my $r = (G.new.m:
    'a',
    'b',
) or die 'unreachable';
is $r, 'a,b', 'multi-line colon-call trailing comma before ) then or';

# Private-method colon-call with a trailing comma.
is C.new.go, 6, 'private colon-call trailing comma before )';

# Trailing comma before ';' and '}' must still work (unchanged).
my $s = (G.new.m: 1, 2,);
is $s, '1,2', 'trailing comma before ) in a statement';

sub take-list(@x) { @x.join('|') }
is (take-list [G.new.m: 9, 8,]), '9,8', 'nested in a sub call';

# A non-trailing colon-call (no trailing comma) is unaffected.
is (G.new.m: 1, 2), '1,2', 'colon-call without trailing comma still works';

done-testing;
