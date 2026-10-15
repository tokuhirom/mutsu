use Test;

# ADR-8751 stage 2: a block-scoped `use` binds an imported user infix
# operator in the block's lexical import set. Every operator form in the
# block reads it -- a binary use, a reduction, a hyper, a compound assignment,
# `&infix:<op>` -- also from a closure called after the block exited, and from
# a `.map` / `gather` body; a callee written outside the block does not see it.
plan 13;

use lib 't/lib';

sub infix:<+%+>($a, $b) { "main($a,$b)" }
sub uibo-callee { 1 +%+ 2 }

sub mk { use UseInBlockOps; -> { 1 +%+ 2 } }
is mk()(), 'M(1,2)', 'a closure keeps the binary operator';

sub mk-reduce { use UseInBlockOps; -> { [+%+] 1, 2, 3 } }
is mk-reduce()(), 'M(M(1,2),3)', 'a reduction reads the import';

sub mk-scan { use UseInBlockOps; -> { [\+%+] 1, 2 } }
is mk-scan()().join('|'), '1|M(1,2)', 'so does a triangle reduction';

sub mk-hyper { use UseInBlockOps; -> { (1, 2) >>+%+<< (3, 4) } }
is mk-hyper()().join('|'), 'M(1,3)|M(2,4)', 'a hyper reads the import';

sub mk-assign { use UseInBlockOps; -> { my $x = 1; $x +%+= 5; $x } }
is mk-assign()(), 'M(1,5)', 'a compound assignment reads the import';

sub mk-amp { use UseInBlockOps; -> { &infix:<+%+>(5, 6) } }
is mk-amp()(), 'M(5,6)', '&infix:<op> reads the import';

sub mk-map { use UseInBlockOps; (1, 2).map({ $_ +%+ 0 }) }
is mk-map().join('|'), 'M(1,0)|M(2,0)', 'a .map body consumed after the block reads it';

sub mk-gather { use UseInBlockOps; gather { take 3 +%+ 4 } }
is mk-gather().join, 'M(3,4)', 'so does a gather body';

{
    use UseInBlockOps;
    is 1 +%+ 2, 'M(1,2)', 'inside the block the import wins over the unit\'s operator';
    is uibo-callee(), 'main(1,2)', 'a callee written outside the block does not see it';
    is (any(1, 2) +%+ 3).raku, 'any("M(1,3)", "M(2,3)")', 'a Junction operand autothreads';
    is uibo-j(all(3, 4)).raku, 'all("j(3)", "j(4)")', 'so does one passed to an imported routine';
}
is 1 +%+ 2, 'main(1,2)', 'after the block the unit\'s own operator answers again';
