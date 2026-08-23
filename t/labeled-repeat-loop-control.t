use Test;

plan 2;

{
    my $completed = False;
    OUTSIDE-NEXT: repeat { next OUTSIDE-NEXT } while False;
    $completed = True;
    ok $completed, 'next LABEL exits a labeled repeat/while loop';
}

{
    my $completed = False;
    OUTSIDE-LAST: repeat { last OUTSIDE-LAST } while False;
    $completed = True;
    ok $completed, 'last LABEL exits a labeled repeat/while loop';
}
