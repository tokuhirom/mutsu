use Test;
plan 4;

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
is $seq, "FXNXNXL", 'FIRST/NEXT/LAST in loop';

is_run 'say "body"; END { say "end" }', { out => "body\nend\n" }, 'END runs after program';
