use Test;

plan 4;

sub feed-arr() {
    my @r;
    for 1..2 -> $k {
        (1, 2) ==> map { $_ * $k } ==> my @t;
        @r.push(@t.join("+"));
    }
    @r.join("|")
}
is feed-arr(), "1+2|2+4", '==> my @t in a loop body is fresh per iteration';

sub feed-hash() {
    my @r;
    for 1..2 -> $k {
        (1, 2) ==> map { $_ => $k } ==> my %t;
        @r.push(%t.elems);
    }
    @r.join("|")
}
is feed-hash(), "2|2", '==> my %t in a loop body is fresh per iteration';

sub paren-decl() {
    my @r;
    for 1..2 -> $k { @r.push((my @a).push($k).join("+")) }
    @r.join("|")
}
is paren-decl(), "1|2", '(my @a) in a loop body is fresh per iteration';

{
    my @x;
    for 1..2 { (my @b).push($_) for ^3; @x.push((my @c).elems) }
    is @x.join(","), "0,0", 'statement-modifier loop inside a loop body';
}
