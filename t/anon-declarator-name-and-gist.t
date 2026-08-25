use v6;
use Test;
use lib $?FILE.IO.parent(2).add("roast/packages/Test-Helpers/lib");
use Test::Util;

plan 16;

# An expression-position package declarator (`anon class`/`anon role`/
# `anon grammar`, and the postfixed `(grammar G { ... }).^name` form) must
# accept the same Unicode identifier-start class as the statement-position
# declarator. An ASCII-only gate used to reject a non-ASCII name here even
# though `grammar þ { ... }` at statement level parsed fine.

# --- anon class ------------------------------------------------------------

is (anon class Foo {}).^name, 'Foo', 'anon class takes an ASCII name';
is (anon class þ {}).^name, 'þ', 'anon class takes a non-ASCII name';
is (anon class þ { method m { 7 } }).new.m, 7,
    'a non-ASCII-named anon class has working methods';
is (anon class þ {}).gist, '(þ)', 'a non-ASCII-named anon class gists with parens';

# --- anon role -------------------------------------------------------------

is (anon role Bar {}).^name, 'Bar', 'anon role takes an ASCII name';
is (anon role þ {}).^name, 'þ', 'anon role takes a non-ASCII name';

# --- grammar in expression position ----------------------------------------

is (grammar Baz { token TOP { . } }).^name, 'Baz',
    'an expression-position grammar takes an ASCII name';
is (grammar þ { token TOP { . } }).^name, 'þ',
    'an expression-position grammar takes a non-ASCII name';

my $ascii-grammar = anon grammar Qux { token TOP { . } };
is $ascii-grammar.^name, 'Qux', 'anon grammar takes an ASCII name';
is $ascii-grammar.parse('x').Str, 'x', 'an ASCII-named anon grammar parses';

my $uni-grammar = anon grammar þ { token TOP { . } };
is $uni-grammar.^name, 'þ', 'anon grammar takes a non-ASCII name';
is $uni-grammar.parse('x').Str, 'x', 'a non-ASCII-named anon grammar parses';

# --- anon sub: name kept, gist carries the & sigil -------------------------

my $ascii-sub = anon sub Foo { 42 };
is $ascii-sub.name, 'Foo', 'anon sub keeps an ASCII name';
is $ascii-sub.gist, '&Foo', 'a named anon sub gists with its & sigil';

is_run 'say anon class þ {};', { :out("(þ)\n") },
    'say renders a non-ASCII-named anon class as (þ)';
is_run 'say anon sub þ  { 42 };', { :out("&þ\n") },
    'say renders a non-ASCII-named anon sub as &þ';

# vim: ft=raku
