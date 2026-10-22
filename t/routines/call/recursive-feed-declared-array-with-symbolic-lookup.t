use Test;

plan 4;

# A symbolic lookup anywhere in the program (`$::("X")`) latches the
# reflective-name-access mode, in which every frame slot is mirrored into the
# env by name. The "initialize once per declaration site" marker of an
# expression-position `my @v` then reached a recursive callee, which skipped
# its own declaration and fed the caller's array (#14498).
sub dyn { my $m = $::("X"); return $m; }

sub rec(Int $n) {
    my @in = 1..$n;
    @in ==> map -> $i { $i == 2 ?? rec(1).join('+') !! $i } ==> my @val;
    return @val.join(',');
}
is rec(3), '1,1,3', 'a recursive sub feeding `my @val` keeps one array per call';

sub deep(Int $n) {
    (1, 2) ==> map { $n > 0 && $_ == 2 ?? deep($n - 1) !! $_ } ==> my @v;
    @v.raku
}
is deep(1), '[1, "[1, 2]"]', 'the innermost call fills its own array';
is deep(2), '[1, "[1, \"[1, 2]\"]"]', '... at every depth';

sub plain(Int $n) {
    my @v;
    @v.append((1, 2).map({ $n > 0 && $_ == 2 ?? plain($n - 1) !! $_ }));
    @v.join(',');
}
is plain(1), '1,1,2', 'a statement-position declaration was never affected';
