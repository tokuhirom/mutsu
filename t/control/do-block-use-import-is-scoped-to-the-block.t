# head-skip-tail's t/01-basic.rakutest imports Test inside a `do` block:
#   BEGIN my (&plan, &ok) = do { use Test; (&plan, &ok) }
# and then loads a module that declares `my proto sub skip`. Test's `skip`, imported
# by the `use` inside the `do` block, stayed in the routine registry after the block
# and the later `my proto sub skip` was refused as a redeclaration.

BEGIN my (&plan, &ok, &is) = do {
    use Test;
    (&plan, &ok, &is)
}

plan 4;

my $value = do { use Test; 40 + 2 };
is $value, 42, 'the block with a use still yields its value';

my proto sub skip(Mu, |) {*}
my multi sub skip($n, +values) { "mine $n" }
is skip(1, 2), 'mine 1', 'a my multi sub skip is not a redeclaration of the imported Test::skip';

sub ok-twice { ok True, 'the routines imported through the block still work'; ok True, 'twice' }
ok-twice;
