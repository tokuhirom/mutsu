use Test;
use MONKEY-SEE-NO-EVAL;

plan 9;

# From language version 6.d on, the `:sym<>` colonpair of a plain subroutine
# name is reserved: `sub meow:sym<bar> {}` raises X::Syntax::Reserved
# (roast MISC/misc.t, #16412).
throws-like 'use v6.d; sub meow:sym<bar> {}', X::Syntax::Reserved, ':sym<...>';
throws-like 'use v6.d; sub meow:sym«bar» {}', X::Syntax::Reserved, ':sym«...»';
throws-like 'use v6.d; sub meow:foo<bar>:sym<bar> {}', X::Syntax::Reserved, ':foo<bar>:sym<...>';
throws-like 'use v6.d; sub meow:sym<bar>:foo<bar> {}', X::Syntax::Reserved, ':sym<...>:foo<bar>';
throws-like 'sub meow:sym<bar> {}', X::Syntax::Reserved, 'reserved by default (6.d)';

# Not reserved: other colonpairs, operator categories, and grammar tokens.
lives-ok { EVAL 'sub meow:foo<bar> {}' }, ':foo<bar> is an ordinary extension';
lives-ok { EVAL 'sub infix:sym<foo>($a, $b) { 1 }' }, 'operator category with :sym';
lives-ok { EVAL 'multi sub infix:sym<bar>($a, $b) { 1 }' }, 'multi operator with :sym';
lives-ok { EVAL 'grammar G { token t:sym<a> { a } }' }, 'a grammar token with :sym';
