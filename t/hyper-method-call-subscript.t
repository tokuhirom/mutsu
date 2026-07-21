use v6;
use Test;

# A hyper method call (`>>.name` / `».name`) may be directly subscripted, just
# like a plain method call: `@a>>.b{$k}` parses as `(@a>>.b){$k}`. Previously the
# `{...}` was left as a *separate block statement*, which silently truncated the
# enclosing declaration list — a role body would drop every method declared after
# such a call, so a later private method vanished from composition.

plan 4;

# 1. Positional subscript on a hyper method call result evaluates correctly.
{
    my @a = [1, 2, 3], [4, 5, 6];
    is (@a>>.reverse)[0].join(','), '3,2,1', 'positional subscript on hyper result';
}

# 2. The subscript form without explicit grouping parses the same way.
{
    my @a = [1, 2, 3], [4, 5, 6];
    is (@a>>.reverse[0]).join(','), '3,2,1', 'hyper-call subscript without extra parens';
}

# 3. A `>>.method{subscript}` inside a method-call argument must not swallow the
#    rest of the enclosing role body. The private `!init` declared AFTER such a
#    method used to be dropped, so `self!init` failed at composition.
{
    role R {
        has %.opt;
        method pick($k) { self.wrap(self.opt>>.self{$k}) }
        method wrap($v) { $v }
        method !init() { 'inited' }
        method run() { self!init }
    }
    class C does R { }
    is C.new.run, 'inited', 'private method after >>.m{subscript} survives composition';
}

# 4. The same construct in a bare parenthesized expression also keeps following
#    statements intact (a positional subscript, whose runtime semantics are
#    unambiguous, exercised here).
{
    my @a = [10, 20], [30, 40];
    my $i = 1;
    my $picked = (@a>>.reverse[$i]);
    my $after = 'reached';
    is $after, 'reached', 'statement after paren >>.m[subscript] still runs';
}
