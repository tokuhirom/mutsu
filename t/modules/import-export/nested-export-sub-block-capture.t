use Test;

# An `is export` sub declared in a block of an inline module closes over the
# block's `my` variables. The importer aliases the routine at BEGIN time,
# before the block runs; once it has run, a call through the import alias
# reads that latest activation's lexicals, not a stale lookup by name
# (#10559).

plan 9;

{
    module M1 { if True { my $x = 5; sub g1 is export { $x } } }
    import M1;
    is g1(), 5, 'sub in a branch reads the branch lexical';
}
{
    module M2 { { my $x = 6; sub g2 is export { $x } } }
    import M2;
    is g2(), 6, 'sub in a bare block reads the block lexical';
}
{
    module M3 { { my $x = 5; sub g3 is export { $x }; $x = 7 } }
    import M3;
    is g3(), 7, 'a write after the declaration is seen';
}
{
    module M4 { { my $x = 1; sub g4 is export { $x++ } } }
    import M4;
    g4();
    is g4(), 2, 'the sub writes through to the same variable';
}
{
    module M5 { for 1..3 -> $i { my $x = $i * 10; sub g5 is export { $x } } }
    import M5;
    is g5(), 30, 'a loop body: the latest iteration answers';
}
{
    module M6 { my $y = 3; { my $x = 5; sub g6 is export { $x + $y } } }
    import M6;
    is g6(), 8, 'package-level and block lexicals together';
}
{
    module M7 { if True { my $x = 5; sub g7 is export { $x } } }
    import M7;
    my $x = 99;
    is g7(), 5, "the caller's same-named lexical does not answer";
}
{
    module M8 { if True { my $x = 5; sub g8 is export { $x }; our &h8 = &g8 } }
    import M8;
    is g8(), 5, 'the import alias and the in-block code value agree';
}
{
    module M9 { if True { my $x = 5; our sub g9 { $x } } }
    is M9::g9(), 5, 'an our sub in a branch still reads the branch lexical';
}
