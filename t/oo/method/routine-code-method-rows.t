use Test;

# `Routine`'s and `Code`'s own methods as rows (ADR-11276 §9.166), on a `Sub`
# value and on a `&name` handle of a multi. Expected values are Rakudo's.

plan 22;

sub double($x) { $x * 2 }
multi m(Int $x) { "int" }
multi m(Str $x) { "str" }

# wrap / unwrap
{
    my $h = &double.wrap(-> $x { callwith($x) + 1 });
    is double(3), 7, 'Routine.wrap on a Sub';
    &double.unwrap($h);
    is double(3), 6, 'Routine.unwrap with the handle restores it';
    my $mh = &m.wrap(-> $x { "w:" ~ callsame });
    is m(1), "w:int", 'Routine.wrap on a multi';
    &m.unwrap($mh);
    is m(1), "int", 'Routine.unwrap on a multi';
    throws-like { &double.unwrap($h) }, X::Routine::Unwrap, 'unwrap with a spent handle';
    ok $h.^name.contains("WrapHandle") || $h.defined, 'wrap answers a handle';
}

# assuming
{
    my &d = &double.assuming(5);
    is d(), 10, 'Code.assuming on a Sub';
    my &ms = &m.assuming("x");
    is ms(), "str", 'Code.assuming on a multi';
    sub named(:$a, :$b) { "$a$b" }
    is &named.assuming(:a<x>)(:b<y>), "xy", 'Code.assuming with a named argument';
}

# candidates / dispatchees / cando
{
    is &m.candidates.elems, 2, 'Routine.candidates of a multi';
    is &m.dispatchees.elems, 2, 'Routine.dispatchees';
    is &double.candidates.elems, 1, 'a Sub is its own candidate';
    is &m.cando(\(1)).elems, 1, 'Code.cando keeps the binding candidates';
    is &m.cando(\(1))[0](1), "int", '... which are callable';
    is &double.cando(\(1, 2)).elems, 0, 'Code.cando of a Sub that cannot bind';
}

# rw, prec, is-implementation-detail
{
    sub plain { }
    sub lv is rw { my $x }
    nok &plain.rw, 'Routine.rw of a plain sub';
    ok &lv.rw, 'Routine.rw of an is rw sub';
    sub infix:<★>($a, $b) is assoc<right> { $a ~ $b }
    is &infix:<★>.associative, "right", 'Code.associative';
    ok &infix:<★>.prec ~~ Hash, 'Code.prec is a Hash';
    is &infix:<★>.prec<assoc>, "right", '... carrying the associativity';
    sub hidden is implementation-detail { }
    ok &hidden.is-implementation-detail, 'Code.is-implementation-detail';
    nok &plain.is-implementation-detail, '... False by default';
}
