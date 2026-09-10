use Test;

# `not`/`so` are loose-precedence unary operators. On the right of a tight `&&`
# they bind loosely: `A && not B` is `A && (not B)`, and `A && not B && C` is
# `A && not (B && C)`. mutsu used to treat `not` after `&&` as a bareword,
# producing a parse error (`if A && not B { ... }`). `||` already handled this.

plan 7;

is (True  && not False), True,  'True && not False is True';
is (True  && not True),  False, 'True && not True is False';
is (False && not True),  False, 'False && not True short-circuits to False';

# Loose binding: not grabs the rest of the && chain.
is (True && not False && False), True, 'A && not B && C parses as A && not (B && C)';

# Works as an `if` condition followed by a block (the HTTP::Tiny pattern).
my $a = True;
my $b = False;
my $hit = 0;
if $a && not $b { $hit = 1 }
is $hit, 1, 'if A && not B { } parses and runs';

# `so` (the positive loose unary) on the RHS of &&.
is (True && so 5), True, 'True && so 5 is True';

# A method call with colon args after `&& not` (mirrors HTTP::Tiny exactly).
class H { method can-reuse($a, $b, $c) { False } }
my $h = H.new;
my $ok = 0;
if $h && not $h.can-reuse: 1, 2, 3 { $ok = 1 }
is $ok, 1, '&& not $obj.meth: a, b, c { } parses';
