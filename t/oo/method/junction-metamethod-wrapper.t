use Test;

# A junction reaches a metamethod, a built-in HOW and a method wrapper whole
# (LibTmux's `is single-operation` admission trait).

plan 11;

my $j = any('a', 'b');
is $j.^isa(Junction), 1, '.^isa on a junction does not thread';
is $j.^name, 'Junction', '.^name of a junction';
is $j.HOW.isa($j, Junction), 1, 'a built-in HOW method takes a junction argument whole';
is $j.^can('Str').elems, 2, '.^can on a junction does not thread';

class P {
    method m(Str:D $v, Str:D :socket-name(:$socket) = 'd') { "$v/$socket" }
}
my @seen;
P.^lookup('m').wrap(sub (|c) { @seen.push(c.hash<socket> // c.list[1]); callsame });

my $r = P.new.m(any('a', 'b'));
isa-ok @seen[0], Junction, 'the wrapper receives a positional junction whole';
is $r.raku, 'any("a/d", "b/d")', 'callsame threads at the original signature';

@seen = ();
$r = P.new.m('x', :socket(any('p', 'q')));
is @seen.elems, 1, 'the wrapper runs once for a named junction';
isa-ok @seen[0], Junction, 'the wrapper receives a named junction (alias name) whole';
is $r.raku, 'any("x/p", "x/q")', 'the original threads the named junction';

class Q is P { }
@seen = ();
$r = Q.new.m(any('c', 'd'));
is @seen.elems, 1, 'an inherited wrapped method binds to its wrapper first';
is $r.raku, 'any("c/d", "d/d")', 'and threads at the original';
