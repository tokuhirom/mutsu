use Test;

plan 9;

# `.Num` on the type object of a concrete-only Cool type dies with
# X::Parameter::InvalidConcreteness, naming the type object (#11992).
for (Int, 'Int', Str, 'Str', UInt, 'Int', Complex, 'Complex', Rat, 'Rational') -> $t, $exp {
    try { $t.Num };
    my $m = ~$!;
    ok $m.contains("Invocant of method 'Num' must be an object instance")
        && $m.contains("'$exp'") && $m.contains($t.^name),
        "{$t.^name}.Num names the type object";
}

throws-like { Int.Num }, X::Parameter::InvalidConcreteness,
    'Int.Num throws X::Parameter::InvalidConcreteness';
is Num.Num.raku, 'Num', 'Num.Num is identity';
is 3.Num, 3e0, 'concrete Int.Num still works';
is "4".Num, 4e0, 'concrete Str.Num still works';

# vim: expandtab shiftwidth=4
