use Test;

# Distilled from Terminal::UI (Frame's `has $.border-color = t.bright-white`,
# `t` being the term Terminal::ANSI::OO exports through `sub EXPORT`). The
# attribute default runs in the constructing caller's env, where a lexical
# `my $t` shares the plain env key with the exported term `t`; the bareword
# must keep naming the term.

plan 3;

use lib 't/lib';
use ExportTermUser;

sub plain { my $t = 7; ExportTermUser.new.greeting }
is plain(), 'hello', 'caller scalar $t does not replace exported term t';

sub in-block { my $o; { my $t = 7; $o = ExportTermUser.new; }; $o.greeting }
is in-block(), 'hello', 'block-scoped $t does not replace exported term t';

sub with-list { my ($t, $p) = 1, 2; ExportTermUser.new.greeting }
is with-list(), 'hello', 'list-declared $t does not replace exported term t';
