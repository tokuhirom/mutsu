use Test;

# `.^set_name` renames the type itself: every type object of it, however it
# is reached, gists and stringifies under the new name (rakudo).

plan 6;

class A { }
A.^set_name('Q');
is A.^name, 'Q', '.^name answers the new name';
is A.gist, '(Q)', 'the type object gists under the new name';
is (A,).gist, '((Q))', 'so does a list holding it';
is A.^mro.gist, '((Q) (Any) (Mu))', 'and an MRO listing it';

class B { }
role R { }
my $a = B.new but R;
my $b = B.new but R;
$a.^set_name('Ren');
is $b.WHAT.gist, '(Ren)', 'a rename of a mixin type reaches every object of it';
is $b.^mro.gist, '((Ren) (B) (Any) (Mu))', 'including its MRO head';
