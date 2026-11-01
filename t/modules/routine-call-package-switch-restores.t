use Test;
use lib 't/lib';
use PackageSwitchProbe;

# A call into a routine declared in another package runs in that package and
# puts the caller's back afterwards, with the switch held as a symbol (no
# package-name String is built per call). Checked against rakudo.
plan 5;

package Outer {
    our sub here() { $?PACKAGE.^name }
}

is where-am-i(), 'PackageSwitchProbe', 'an imported routine runs in its own package';
is Outer::here(), 'Outer', 'a routine of a package declared in the program runs in it';
is (Outer::here(), where-am-i(), Outer::here()).join(','), 'Outer,PackageSwitchProbe,Outer',
    'the package is restored after each call';
is call-back({ Outer::here() }), 'Outer', 'a block called back from another package keeps its own';
my @seen;
for 1..3 { @seen.push: where-am-i() }
is @seen.unique.join(','), 'PackageSwitchProbe', 'a hot loop keeps switching to the same package';
