use v6;
use Test;

# Writes to a block `my` lexical captured by an escaped `our sub` must reach
# the persisted shared cell across calls made AFTER the declaring block has
# exited — mirroring the read side (`&OUR::f()` resolves the capture through
# the cell). Regression pins for:
#   * post-increment losing its write once an intervening `say` flushed the
#     dead top-level slot into env as Nil (second call restarted from 0)
#   * plain assignment (`$a = v`) landing only on the callee env copy and
#     being dropped on return
#   * compound assignment (`$a += v`) having no cell resolution at all
# All expected values raku-verified (tmp probe, 2026-07-10).

plan 21;

# post-increment accumulates across escaped calls
{ my $a = 3; our sub eosl-inc { $a++ } }
is &OUR::eosl-inc(), 3, 'post-increment: first escaped call sees the block value';
is &OUR::eosl-inc(), 4, 'post-increment: second escaped call continues from the cell';
is &OUR::eosl-inc(), 5, 'post-increment: third escaped call continues from the cell';

# an intervening say (which syncs top-level locals into env) must not
# disturb the cell
{ my $b = 10; our sub eosl-inc-b { $b++ } }
is &OUR::eosl-inc-b(), 10, 'increment before intervening say';
say '# noise';
is &OUR::eosl-inc-b(), 11, 'increment survives an intervening say';

# plain assignment through the escaped sub reaches the shared cell
{ my $c = 3; our sub eosl-set { $c = 42 }; our sub eosl-peek-c { $c } }
&OUR::eosl-set();
is &OUR::eosl-peek-c(), 42, 'plain assignment reaches the cell';
&OUR::eosl-set();
is &OUR::eosl-peek-c(), 42, 'repeated assignment stays visible';

# compound assignment (numeric)
{ my $d = 5; our sub eosl-add { $d += 7 }; our sub eosl-peek-d { $d } }
is &OUR::eosl-add(), 12, 'compound += first escaped call';
is &OUR::eosl-add(), 19, 'compound += accumulates in the cell';
is &OUR::eosl-peek-d(), 19, 'compound += result visible to a sibling reader';

# compound assignment (string)
{ my $e = "x"; our sub eosl-cat { $e ~= "y" } }
is &OUR::eosl-cat(), 'xy', 'compound ~= first escaped call';
is &OUR::eosl-cat(), 'xyy', 'compound ~= accumulates in the cell';

# pre-increment
{ my $f = 100; our sub eosl-preinc { ++$f } }
is &OUR::eosl-preinc(), 101, 'pre-increment first escaped call';
is &OUR::eosl-preinc(), 102, 'pre-increment accumulates in the cell';

# post- and pre-decrement share the cell
{ my $g = 50; our sub eosl-dec { $g-- }; our sub eosl-predec { --$g } }
is &OUR::eosl-dec(), 50, 'post-decrement first escaped call';
is &OUR::eosl-predec(), 48, 'pre-decrement continues from the cell';
is &OUR::eosl-dec(), 48, 'post-decrement continues from the cell';

# calls inside the block and after it agree on one cell
my @order;
{
    my $h = 0;
    our sub eosl-h { $h++ }
    @order.push(&OUR::eosl-h());
    @order.push(&OUR::eosl-h());
}
@order.push(&OUR::eosl-h());
@order.push(&OUR::eosl-h());
is @order.join(','), '0,1,2,3', 'inside-block and escaped calls share one cell';

# assignment then increment through sibling escaped subs
{ my $i = 1; our sub eosl-seti { $i = 20 }; our sub eosl-inci { $i++ } }
&OUR::eosl-seti();
is &OUR::eosl-inci(), 20, 'increment sees a prior escaped assignment';
is &OUR::eosl-inci(), 21, 'increment accumulates after the assignment';

# an unrelated top-level lexical with the same name is untouched
my $a = 1000;
&OUR::eosl-inc();
is $a, 1000, 'same-named top-level lexical is not clobbered by the escaped write';

done-testing;
