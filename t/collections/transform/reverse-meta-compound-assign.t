use Test;

plan 3;

{
    my $a;
    10 R+= ($a ||= 42);
    is-deeply $a, 52, 'R+= with ||= assigns through RHS container (1)';
    10 R+= ($a ||= 42);
    is-deeply $a, 62, 'R+= with ||= assigns through RHS container (2)';
}

{
    my $a = 12;
    ((10 R+= ($a [R-]= 42)) //= 100) += 1000;
    is-deeply $a, 1040, 'nested reverse meta compound assignment chain';
}
