use Test;

# An `our sub` declared in a `for` loop body closes over the loop's own
# pointy-block parameter: after the loop it reads the last iteration's
# binding, as a `my` declared in the same body does (mutsu#10512).

plan 6;

package P1 { for 1..2 -> $i { our sub g { $i // 'u' } } }
is P1::g(), 2, 'package-level loop: our sub sees the last iteration';

package P2 { for 1..2 -> $i { my $j = $i; our sub g { "$i $j" } } }
is P2::g(), '2 2', 'loop param and body lexical agree';

package P3 { sub f { for 1..2 -> $i { our sub g { $i // 'u' } } }; f() }
is P3::g(), 2, 'loop inside a routine';

package P4 {
    our @subs;
    for 1..3 -> $i { our sub g { $i }; @subs.push: &g }
}
is @P4::subs».().join(','), '1,2,3', 'each iteration binds its own parameter';

package P5 { for 1..2 -> Int $i { our sub g { $i } } }
is P5::g(), 2, 'typed loop parameter';

package P6 {
    our @seen;
    for 1..2 -> $i { our sub g { $i }; @seen.push: g() }
}
is @P6::seen.join(','), '1,2', 'calls inside the loop see the current iteration';
