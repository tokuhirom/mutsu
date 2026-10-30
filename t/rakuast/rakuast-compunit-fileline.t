use Test;

plan 6;

# `.fileline` ("<file>:<line>" of a node's origin) is an undocumented rakudo
# method that static::CLI's analysis calls (maintainer-approved, #15531).
# It is read from a CHECK-time closure, where the unit's statements exist.
my @seen;
BEGIN {
    my $cu = $*CU;
    $cu.add-check-code({
        @seen.push: $cu.fileline;
        @seen.push: .fileline for $cu.statement-list.statements;
    });
}
my $x = 1;
my $y = 2;

sub line-of($fileline) { $fileline.split(':')[*-1].Int }
sub file-of($fileline) { $fileline.split(':')[0 .. *-2].join(':') }

is line-of(@seen[0]), 1, 'the unit starts on line 1';
ok @seen.elems >= 4, 'every top-level statement answers';
is file-of(@seen[0]), file-of(@seen[2]), 'a statement names the same file as the unit';
ok file-of(@seen[0]).chars > 0, 'the file is named';
is line-of(@seen[*-2]), 26, 'a statement answers its own line';
is line-of(@seen[*-1]), 27, 'the next statement answers the following line';
