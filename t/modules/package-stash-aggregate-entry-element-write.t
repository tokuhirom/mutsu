use Test;

# An element write below a package stash's entry (`GLOBAL::<%Z><a>++`) writes
# the package variable the entry names, creating it when it does not exist
# yet, as `%GLOBAL::Z<a>++` does (#18204).

plan 9;

GLOBAL::<%Z><a>++;
is-deeply %GLOBAL::Z, %(a => 1), 'increment below a missing % entry';

GLOBAL::<@L>[2]++;
is-deeply @GLOBAL::L, [Any, Any, 1], 'increment below a missing @ entry';

GLOBAL::<%Y><a> = 5;
is-deeply %GLOBAL::Y, %(a => 5), 'assignment below a missing % entry';

GLOBAL::<%X><a> += 2;
is-deeply %GLOBAL::X, %(a => 2), 'compound assignment below a missing % entry';

package Foo { our %reg; }
Foo::<%reg><x>++;
Foo::<%reg><x>++;
is-deeply %Foo::reg, %(x => 2), 'an existing package variable';

Bar::<@q>[0] = 'z';
is-deeply @Bar::q, ['z'], 'a package not declared yet';

GLOBAL::<$S><k> = 1;
is-deeply $GLOBAL::S, %(k => 1), 'below a $ entry';

my %h;
MY::<%h><a> = 3;
is-deeply %h, %(a => 3), 'a lexical stash entry is still the lexical';

nok GLOBAL::<%W>:exists, 'reading an entry creates nothing';
