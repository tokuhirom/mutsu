use Test;

plan 2;

{
    sub foo(Str :@foo) { }

    my $c = \();
    ok ?&foo.cando($c), "cando works with empty capture";
    lives-ok { foo(|$c) }, "calling with slipped empty capture lives";
}
