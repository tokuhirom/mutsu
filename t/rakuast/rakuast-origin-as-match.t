use Test;

plan 7;

# `Origin.as-match` (a `RakuAST::Origin::Match` with the file, line, offsets
# and text of a node's span) is an undocumented rakudo method that
# static::CLI's analysis calls (maintainer-approved, #15597). Read from a
# CHECK-time closure, where the unit's statements exist.
my @seen;
BEGIN {
    my $cu = $*CU;
    $cu.add-check-code({
        my $origin = $cu.statement-list.statements.first(*.fileline.ends-with(':18')).origin;
        my $m = $origin.as-match;
        @seen = $m.^name, $m.file, $m.line, $m.from, $m.to, $m.Str, $m.chars;
    });
}
my $marker = 42;

is @seen[0], 'RakuAST::Origin::Match', 'it is an Origin::Match';
ok @seen[1].chars > 0, 'it names the file';
is @seen[2], 18, 'it carries the line the span begins on';
is @seen[5], 'my $marker = 42', 'Str is the text of the span';
is @seen[4] - @seen[3], @seen[5].chars, 'to - from is the span length';
is @seen[6], @seen[5].chars, 'chars is the span length';
ok @seen[3] < @seen[4], 'from precedes to';
