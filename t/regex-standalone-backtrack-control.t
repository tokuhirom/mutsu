use Test;

plan 7;

# `:` on its own is the backtrack control "commit to the atom just matched" — a
# ratchet on the PRECEDING atom, not a modifier and not a `:my` declaration. It
# was rejected outright ("Unrecognized regex metacharacter :"), which killed the
# whole rule it appeared in (YAMLish writes its key token this way).
# Note raku only accepts it after a plain, unquantified atom.

grammar Ratchets {
    token space { <[\ \t]> }
    token pf    { <-[\-\?\:\,\#\ \t]> }

    token pair     { 'a' : 'b' }
    token triple   { 'a' : 'b' : 'c' }
    token classes  { <[a..z]> : <[a..z]> }
    token subrule  { <.pf> : <-[\:\#]>* }
    token captured { $<v>=[ <.pf> : <-[\:\#]>* ] }
    token trailing { <.pf> <-[\:\#]>* <!after <.space>> : }
}

ok Ratchets.parse('ab',  :rule<pair>).defined,    'a solitary `:` between two literals';
ok Ratchets.parse('abc', :rule<triple>).defined,  'two solitary `:` controls in one rule';
ok Ratchets.parse('ab',  :rule<classes>).defined, '`:` after a character class';
ok Ratchets.parse('ab',  :rule<subrule>).defined, '`:` after a non-capturing subrule call';
ok Ratchets.parse('ab',  :rule<trailing>).defined, '`:` after a zero-width assertion';

my $c = Ratchets.parse('ab', :rule<captured>);
ok $c.defined, '`:` inside a named capture group';
is ~$c<v>, 'ab', 'the capture still spans the whole group';
