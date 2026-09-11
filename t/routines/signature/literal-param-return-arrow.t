use v6;
use Test;

# Regression (#7954, the `expected statement ...` parse-failure index): a
# signature's `-->` written with no space after a LITERAL parameter. The literal
# parameter is recognized by running the full expression parser, whose postfix
# layer happily lexed the `--` of `-->` onto the literal it had just read, so
# `multi method norm(M:D: 'column-sum'--> Numeric)` (Math::Matrix) came back as
# `('column-sum'--) > Numeric` — not a literal at all, and the whole parameter
# list then failed to parse. A sigiled parameter was unaffected, because the
# parameter parser reads its name itself and never reaches the postfix layer.

plan 13;

# The construct the index reduced to: the Math::Matrix multi, whose literal
# parameter is what selects between the candidates.
class Mat {
    multi method norm(Mat:D: 'column-sum'--> Numeric){ 1 }
    multi method norm(Mat:D: 'row-sum'--> Numeric){ 2 }
    multi method norm(Mat:D: --> Numeric){ 3 }
}
is Mat.new.norm('column-sum'), 1, 'a string literal parameter dispatches with `-->` right behind it';
is Mat.new.norm('row-sum'), 2, 'and the second literal candidate is reachable too';
is Mat.new.norm, 3, 'and the literal-free candidate still matches';

# Every literal spelling, with and without the space before the arrow.
sub str-tight("lit"--> Int) { 1 }
sub str-loose("lit" --> Int) { 2 }
is str-tight("lit"), 1, 'a double-quoted literal parameter takes a tight `-->`';
is str-loose("lit"), 2, 'and a spaced one still works';

sub int-tight(3--> Int) { 4 }
is int-tight(3), 4, 'an integer literal parameter takes a tight `-->`';

sub neg-tight(-3--> Int) { 5 }
is neg-tight(-3), 5, 'a negated integer literal parameter takes a tight `-->`';

sub num-tight(1.5--> Str) { "r" }
is num-tight(1.5), "r", 'a rational literal parameter takes a tight `-->`';

# The literal is still a literal: it constrains the argument.
dies-ok { str-tight("other") }, 'the literal parameter still rejects a different string';
dies-ok { int-tight(4) }, 'and still rejects a different integer';

# The declared return type survives the tighter spelling.
sub bad-return("lit"--> Int) { "not an Int" }
dies-ok { bad-return("lit") }, 'the `-->` return type is enforced, not swallowed';

# A sigiled parameter — the shape that already worked — must keep working.
sub sigiled($x--> Int) { 6 }
is sigiled(9), 6, 'a sigiled parameter takes a tight `-->`';

# Postfix `--` on a term is untouched: only a signature owns `-->`.
my $n = 5;
my $before = $n--;
ok $before == 5 && $n == 4, 'postfix `--` still decrements outside a signature';
