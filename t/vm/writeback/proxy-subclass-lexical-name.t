use v6.d;
use Test;

plan 6;

# A lexical `my class` Proxy subclass is named by its visible name, not by the
# storage name the class is registered under.
my class T is Proxy { }
my $m := T.new(FETCH => -> $ { 42 }, STORE => -> $, $ { });
is $m.VAR.^name, 'T', '.^name of a lexical Proxy subclass is its visible name';
is $m.VAR.WHAT.^name, 'T', '.WHAT.^name agrees';
is $m.VAR.WHAT.raku, 'T', '.WHAT.raku shows the visible name';
is $m.VAR.WHAT.gist, '(T)', '.WHAT.gist shows the visible name';
is $m, 42, 'the FETCH still runs';

class U is Proxy { }
my $n := U.new(FETCH => -> $ { 1 }, STORE => -> $, $ { });
is $n.VAR.^name, 'U', 'a package class keeps its name';
