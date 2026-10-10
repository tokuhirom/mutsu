use Test;

# A `@` parameter's `.VAR.name` is the caller variable it was bound from. The
# binder records that as metadata of the parameter (ADR-12529 phase 1): a
# closure that uses the parameter still sees it, and one that does not use it
# no longer captures it.

plan 5;

my @mine = 1, 2;

sub direct(@kh) { @kh.VAR.name }
is direct(@mine), '@mine', 'the parameter reports its source variable';

sub in-closure(@kh) { my &c = -> { @kh.VAR.name }; c() }
is in-closure(@mine), '@mine', 'a closure over the parameter sees it too';

sub unused(@x) { my &c = -> { 42 }; c() }
is unused(@mine), 42, 'a closure that does not use the parameter runs';

sub optional(@x?) { my &c = -> { @x.VAR.name }; c() }
is optional(), 'element', 'an unsupplied parameter reports "element"';

sub nested(@kh) { my &c = -> { my &d = -> { @kh.VAR.name }; d() }; c() }
is nested(@mine), '@mine', 'a closure inside a closure sees the source name';
