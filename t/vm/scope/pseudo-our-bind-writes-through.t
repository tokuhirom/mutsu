use Test;

# `$OUR::x := $y` rebinds the current package's `our` variable to `$y`'s
# container: a later write through `$y` is seen through `$OUR::x` (and `$Pkg::x`
# inside a package), while a lexical `$x` that aliased the old `our` container
# keeps it (#10859).

plan 10;

our $x30 = 31;
my $x = 39;
$OUR::x30 := $x;
$x = 45;
is $OUR::x30, 45, 'a write through the bind source is seen through OUR::';
ok $OUR::x30 =:= $x, 'and the two names are one container';
is $x30, 31, 'the lexical alias of the old our variable keeps its container';

our $q = 1;
my $w = 2;
$OUR::q := $w;
$OUR::q = 9;
is $w, 9, 'a write through OUR:: reaches the bind source';

package P {
    our $v = 1;
    my $s = 39;
    $OUR::v := $s;
    $s = 45;
    is $OUR::v, 45, 'inside a package, OUR:: sees the write';
    is $P::v, 45, 'and so does the package-qualified name';
    is $v, 1, 'while the lexical alias keeps the old container';
}

our @arr = 1;
my @src = 2, 3;
@OUR::arr := @src;
@src.push(4);
is-deeply @OUR::arr, [2, 3, 4], 'an array bind through OUR:: tracks the source';

our %hash = a => 1;
my %hsrc = b => 2;
%OUR::hash := %hsrc;
%hsrc<c> = 3;
is-deeply %OUR::hash.keys.sort.List, <b c>, 'and so does a hash bind';

{
    our $nested = 1;
    my $n = 5;
    $OUR::nested := $n;
    $n = 6;
    is $OUR::nested, 6, 'a bind in a nested block tracks the source too';
}
