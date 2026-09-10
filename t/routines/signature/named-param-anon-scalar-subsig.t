use Test;

# A named parameter whose alias is an anonymous scalar `$`, followed by a
# sub-signature, and/or a `!`/`?` required marker between them:
#   :literal($) ($payload, *@arguments)
#   :function($)! (Str :$over, *%args)
# The anonymous scalar parses to the internal name `__ANON_STATE__` (unlike
# `@`/`%`, which keep a sigil prefix), so it was wrongly excluded from the
# "single simple variable" alias branch and the trailing sub-signature went
# unparsed. Separately, a `!`/`?` marker before the sub-signature was not
# consumed. Found via the real-distribution compat sweep (SQL::Abstract,
# docs/dist-compat-sweep.md).

plan 8;

# Parses (the load smoke test the dist needs)
lives-ok { EVAL 'sub a(:foo($) ($x, $y)) { }' }, ':name($) with sub-signature parses';
lives-ok { EVAL 'sub b(:foo($)! ($x, $y)) { }' }, ':name($)! (required marker before sub-sig)';
lives-ok { EVAL 'sub c(:foo($)? ($x, $y)) { }' }, ':name($)? (optional marker before sub-sig)';
lives-ok { EVAL 'sub d(Map :function($)! (Str :$over, *%args)) { }' }, 'typed named anon scalar with marker + sub-sig';
lives-ok { EVAL 'sub e(:foo($p)! ($x, $y)) { }' }, ':name($var)! marker before sub-sig (named var)';

# The already-working sibling forms still parse
lives-ok { EVAL 'sub f(:foo(@) (@a)) { }' }, 'anonymous array alias still parses';
lives-ok { EVAL 'sub g(:foo($)) { }' }, 'anonymous scalar alias with no sub-sig still parses';
lives-ok { EVAL 'sub h(:foo($p) ($x, $y)) { }' }, 'named-var alias + sub-sig still parses';
