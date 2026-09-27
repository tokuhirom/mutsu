unit module BlockImportModUser;

# A `use` inside a block (here a constant's initializer, as in the EC dist's
# ed25519) imports the operators into that block only. They must not leak into
# the rest of the unit module once the block is left.
constant d = { use BlockImportModOps; my $*modulus = 7; 3 - 5 }();

our sub inner-result { d }
our sub outer-minus { 3 - 5 }
our sub whatever-minus { (* - 1)(3) }
our sub buf-last { my $b = buf8.new(1, 2, 3); $b[*-1] = 9; $b[2] }
