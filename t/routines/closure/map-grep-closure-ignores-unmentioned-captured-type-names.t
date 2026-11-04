use Test;

# A map/grep callback carries the program's names (types, enums) in its
# captured environment. The inline loop does not install a captured type or
# enum name the body never mentions in the frame that consumes it, but a body
# that DOES name one (directly, in a nested closure, in a smartmatch or by
# name through EVAL) still finds it even when the consuming frame is another
# routine that does not see the name.
plan 9;

my class Pt { has $.x; method gist { "Pt($!x)" } }
enum Colour <Red Green Blue>;

sub apply(&f, @items) { @items.map(&f).list }
sub keep(&f, @items) { @items.grep(&f).list }

is apply(* * 2, (1, 2, 3)).join(','), '2,4,6', 'a body naming nothing';
is keep(* > 1, (1, 2, 3)).join(','), '2,3', 'a grep body naming nothing';

my &mk = { Pt.new(x => $_) };
is apply(&mk, (1, 2)).map(*.gist).join(','), 'Pt(1),Pt(2)', 'a body naming a lexical class, consumed by another routine';

my &is-pt = { $_ ~~ Pt };
is keep(&is-pt, (Pt.new(x => 1), 5, Pt.new(x => 2))).elems, 2, 'a grep body smartmatching a lexical class';

my &nested = { my &g = { Pt.new(x => $_ * 10) }; g() };
is apply(&nested, (1, 2)).map(*.gist).join(','), 'Pt(10),Pt(20)', 'a nested closure naming the class';

my &col = { Colour($_) };
is apply(&col, (0, 2)).map(*.Str).join(','), 'Red,Blue', 'a body naming an enum type';

use MONKEY-SEE-NO-EVAL;
my &ev = { EVAL 'Pt.new(x => ' ~ $_ ~ ')' };
is apply(&ev, (7,)).map(*.gist).join(','), 'Pt(7)', 'a body reaching the class by name through EVAL';

is (1, 2, 3).map(* * 2).grep(* > 2).sum, 10, 'a chain in the declaring frame';
sub inner { my class Local { method v { 42 } }; my &lc = { Local.v + $_ }; apply(&lc, (1, 2)).join(',') }
is inner(), '43,44', 'a class local to a routine, named by the body, consumed by another routine';
