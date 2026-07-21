use lib 't/lib';
use Test;
use ImportedPredicate;

plan 6;

# An imported-function call that is the LEFT operand of a larger expression at
# statement-expression position was truncated: the statement-level parser
# treated `has-interp($s)` as a bare `Stmt::Call` and dropped the trailing
# `?? A !! B` / `~~` / `&&`, because the "is this actually an expression prefix?"
# check only applied to the hardcoded KNOWN_CALLS list, not to imported
# functions. (Template::HAML::Tag: `has-interp($s) ?? InterpString.new(...) !!
# $s`.) These now parse and evaluate.

sub maybe-interp($s) {
    has-interp($s) ?? 'interp' !! 'plain';
}
is maybe-interp('#{x}'), 'interp', 'imported call ?? then-branch (true)';
is maybe-interp('plain'), 'plain', 'imported call ?? else-branch (false)';

sub is-bool($s) { has-interp($s) ~~ Bool }
is is-bool('x'), True, 'imported call as left operand of ~~';

sub anded($s) { has-interp($s) && 'both' }
is anded('#{x}'), 'both', 'imported call as left operand of &&';

# Multi-line ternary (the exact Template::HAML::Tag shape).
sub multi($s) {
    has-interp($s)
      ?? 'yes'
      !! 'no';
}
is multi('#{x}'), 'yes', 'imported call multi-line ternary (true)';
is multi('z'), 'no', 'imported call multi-line ternary (false)';
