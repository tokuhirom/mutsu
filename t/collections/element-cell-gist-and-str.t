use v6;
use Test;

# An element producer that hands out LIVE CELLS rather than plain values --
# `.values`, `.pairs`, `.kv`, `.Seq`, `.sort` (`Value::seq_element_containers`
# in `src/vm/vm_element_producers.rs`) -- must not cost the elements their own
# `.gist`/`.Str`.
#
# `say`/`put`/`print`/`note` choose between pure Rust rendering and real method
# dispatch by probing the collection's elements. The probe looked through
# nested Arrays/Hashes/Pairs but not through a container cell, so a cell
# holding an object reported "no dispatch needed" and the whole collection
# rendered through the pure path: `say @a.values` printed `(F() F())` -- the
# bare type-object placeholder -- while the very same elements gisted correctly
# through the Array itself. The `.raku` twin
# (`contains_dispatch_leaf_seen`) already looked through both, which is why
# `.raku` never showed the bug (#8134).
#
# Every expectation was measured against rakudo.

plan 16;

class Obj {
    has $.n;
    method gist { "G$!n" }
    method Str  { "S$!n" }
    method raku { "R$!n" }
}

my @a = Obj.new(n => 1), Obj.new(n => 2);
my %h = a => Obj.new(n => 3);

# The Array itself was never broken -- it is the baseline the cell-producing
# routines have to match.
is @a.gist, '[G1 G2]', 'the Array gists its elements';

# The cell producers.
is @a.values.gist, '(G1 G2)', '.values gists its elements';
is @a.Seq.gist,    '(G1 G2)', '.Seq gists its elements';
is @a.sort.gist,   '(G1 G2)', '.sort gists its elements';
is @a.pairs.gist,  '(0 => G1 1 => G2)', '.pairs gists its values';
is %h.values.gist, '(G3)', 'Hash .values gists its elements';
is %h.kv.gist,     '(a G3)', 'Hash .kv gists its values';
is %h.pairs.gist,  '(a => G3)', 'Hash .pairs gists its values';

# `say` and `note` render with `.gist`, `put`/`print` with `.Str`; the probe is
# shared, so pin both sides of it.
{
    # `is @a.values.gist` above covers gist; `.join` exercises the `.Str` half
    # of the same probe, which is what put/print render with.
    is @a.values.join(' '), 'S1 S2', '.values stringifies its elements with .Str';
    is @a.sort.join(' '),   'S1 S2', '.sort stringifies its elements with .Str';
    is %h.values.join(' '), 'S3',    'Hash .values stringifies with .Str';
}

# `.raku` was already correct and must stay so.
is @a.values.raku, '(R1, R2).Seq', '.values keeps the per-element .raku';

# A cell of ordinary values keeps working (the probe must look THROUGH a cell,
# not answer "dispatch" for every cell it meets).
{
    my @plain = 1, 2, 3;
    is @plain.values.gist, '(1 2 3)', 'a cell of Ints renders unchanged';
    my %ph = x => 1;
    is %ph.values.gist, '(1)', 'a Hash cell of Ints renders unchanged';
}

# The reported symptom: a Version subclass, whose sort key (`.Str`, "1.0.0")
# differs from its display form (`.gist`, "v1.0.0"), so the substitution the
# issue suspected in `sort` would have been visible here.
{
    class MyVer is Version { }
    my @vs = MyVer.new("1.0.0"), MyVer.new("2.0.0");
    is @vs.gist, '[v1.0.0 v2.0.0]', 'the Array of Version subclasses gists with the v prefix';
    is @vs.sort.gist, '(v1.0.0 v2.0.0)', '.sort keeps the v prefix (the #8134 repro)';
}

done-testing;
