use Test;

# A closure that writes a variable it captured shares the variable's cell,
# whether it escapes the creating frame or is called on the spot, so the
# write reaches the variable without a copy-back when the closure returns
# (ADR-12529 §3, slice 42). Checked against rakudo.

plan 8;

{
    my $a = 0;
    -> $i { $a += $i } for 1..3;
    is $a, 6, 'a pointy block run by a statement-modifier for';
}

{
    my $s;
    if "x" -> Str $t { $s := (1, 2).map: { $_ * 2 } }
    is-deeply $s.Seq, (2, 4).Seq, 'a binding made in a pointy if block';
}

{
    my $outer = 0;
    my &run = -> &b { b() };
    run({
        CONTROL { when CX::Warn { $outer = 42; .resume } }
        warn 'w';
    });
    is $outer, 42, 'a CONTROL handler in a block passed to a routine';
}

{
    my $n = 0;
    sub twice(&f) { f(); f() }
    twice({ $n++ });
    is $n, 2, 'a block a routine calls twice';
}

{
    my $total = 0;
    (1, 2, 3).map({ $total += $_ }).eager;
    is $total, 6, 'a map block';
}

{
    my @seen;
    my $count = 0;
    for 1..2 { { $count++; @seen.push($count) }() }
    is-deeply @seen, [1, 2], 'a nested block called inside a loop';
}

{
    my %h = a => { b => { c => 1, d => 2 } };
    sub assign-through(\target, \values) { subtest 'assign' => { plan 1; ok (target = values) } }
    assign-through(%h{'a';'b';<c d>}, (7, 8));
    is-deeply %h, { a => { b => { c => 7, d => 8 } } }, 'a sigilless parameter assigned from a block stays an alias';
}
