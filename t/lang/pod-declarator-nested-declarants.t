use Test;

# `$=pod`'s declarator entries get their concrete WHEREFORE from a walk of the
# whole unit (ADR-0137 typed visitor), nested declarations included: a `sub`
# declared inside a block or another routine is documented like any other,
# and a nested `multi` candidate takes its number in the unit-wide candidate
# order the parser's doc table uses, so the later candidates keep theirs.

plan 4;

sub outer {
    #| inner doc
    multi g(Int $x) { }
}
#| a doc
multi g(Str $x) { }
#| b doc
multi g(Num $x) { }
{
    #| in block
    sub blocky($y) { }
}

my %by-doc = $=pod.map({ (.contents.join) => .WHEREFORE });
is %by-doc{'inner doc'}.signature.raku, ':(Int $x)', 'a nested multi candidate';
is %by-doc{'a doc'}.signature.raku, ':(Str $x)', 'the first top-level candidate after it';
is %by-doc{'b doc'}.signature.raku, ':(Num $x)', 'the second top-level candidate after it';
is %by-doc{'in block'}.name, 'blocky', 'a sub declared in a bare block';
