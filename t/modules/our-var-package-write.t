use lib $*PROGRAM.parent.add('lib');
use Test;
use OurVarExport;

plan 7;

# `our` is an alias for a package variable, so a write through the lexical
# alias must be visible under the package-qualified name straight away — not
# only after the declaring block exits (a module mainline never "exits" before
# its consumer runs, which is what made an exported `our` read as Nil).
module Pkg {
    our $x;
    $x = 7;
}
is $Pkg::x, 7, 'a write to an `our` lexical reaches the package variable';

module Pkg2 {
    our @a;
    @a = 1, 2, 3;
    our %h;
    %h = :k(1);
}
is-deeply @Pkg2::a, [1, 2, 3], 'the same holds for an `our` array';
is-deeply %Pkg2::h, {:k(1)}, 'the same holds for an `our` hash';

# The exported forms, assigned in a `unit module` mainline.
is $val, 7, 'an exported `our` scalar carries its assigned value';
is-deeply @list, [1, 2, 3], 'an exported `our` array carries its assigned value';
is code(), 0, 'an exported `our` code variable is callable';
is code(), 1, 'the exported closure keeps its state across calls';
