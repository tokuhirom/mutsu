use Test;

# A `package`/`module` block is a scope of its own: a `my` it declares is a
# fresh lexical and never overwrites a same-named enclosing one, while a plain
# write to an enclosing lexical, `$_` or `$/` still reaches it (#13428).

plan 14;

my $w = 1;
package S { my $w = 5 }
is $w, 1, 'a body `my $w` leaves the enclosing `$w` alone';

sub in-routine { my $v = 1; package T { my $v = 6 }; $v }
is in-routine(), 1, '... inside a routine too';

my @a = 1, 2;
module M { my @a = 9; our sub get { @a } }
is @a.join(','), '1,2', 'a body `my @a` leaves the enclosing array alone';
is M::get().join(','), '9', 'the package sub reads the body array';

my %h = a => 1;
module HM { my %h = b => 2; our sub get { %h } }
is %h.keys.join(','), 'a', 'a body `my %h` leaves the enclosing hash alone';
is HM::get().keys.join(','), 'b', 'the package sub reads the body hash';

my $k = 1;
package K { my $k = 10; our sub kk { $k } }
is $k, 1, 'the enclosing scalar is kept';
is K::kk(), 10, 'the package sub reads the body scalar';

module PM { my @x; our sub add($v) { @x.push($v); @x.elems } }
PM::add(1);
is PM::add(2), 2, 'a package sub mutates the body array in place';

my $z = 1;
package R { $z = 2 }
is $z, 2, 'a plain write to an enclosing lexical reaches it';

$_ = 5;
package Q { $_ = 7 }
is $_, 7, 'the package body shares `$_`';

"x" ~~ /x/;
package P { "a" ~~ /a/ }
is ~$/, 'a', 'the package body shares `$/`';

my $q = 'top';
module N { our $q = 'our' }
is $q, 'top', 'an `our` in the body leaves the enclosing `my` alone';
is $N::q, 'our', '... and is the package variable';
