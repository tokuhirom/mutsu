use Test;

plan 6;

{
    my $undone = 0;
    my $x = 0;
    is (while ++$x < 3 { UNDO ++$undone; +$x }), "1 2", "while expression returns iteration values";
    is $undone, 0, "while expression does not run UNDO for successful iterations";
}

{
    my $undone = 0;
    my $x = 0;
    is (until ++$x > 2 { UNDO ++$undone; +$x }), "1 2", "until expression returns iteration values";
    is $undone, 0, "until expression does not run UNDO for successful iterations";
}

{
    my $undone = 0;
    is (loop (my $x = 0; ++$x < 3; ) { UNDO ++$undone; +$x }), "1 2", "C-style loop expression returns iteration values";
    is $undone, 0, "C-style loop expression does not run UNDO for successful iterations";
}
