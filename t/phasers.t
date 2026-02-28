use Test;
plan 3;

my $x = 0;
BEGIN { $x = 2; }
is $x, 2, 'BEGIN runs immediately';

my $out = "";
{
    ENTER { $out ~= "E"; }
    LEAVE { $out ~= "L"; }
    $out ~= "B";
}
is $out, "EBL", 'ENTER/LEAVE wrap block';

my $seq = "";
for 1..3 -> $i {
    FIRST { $seq ~= "F"; }
    NEXT { $seq ~= "N"; }
    $seq ~= "X";
    LAST { $seq ~= "L"; }
}
is $seq, "FXNXNXNL", 'FIRST/NEXT/LAST in loop';
