use Test;

# `PROCESS::<$X> := value` binds the value itself: the stash entry is not a
# container, so `$*X.VAR` is the value's own type and `my @a = $*X` flattens an
# Iterable. An assignment (`PROCESS::<$X> = value`) does make a Scalar.

plan 11;

PROCESS::<$R> := 1..3;
is $*R.VAR.^name, 'Range', '$*X.VAR of a bound Range';
is PROCESS::<$R>.VAR.^name, 'Range', 'PROCESS::<$X>.VAR of a bound Range';
is (my @a = $*R).raku, '[1, 2, 3]', 'my @a = $*X flattens the bound Range';
sub from-sub { (my @b = $*R).raku }
is from-sub(), '[1, 2, 3]', 'also read from another frame';

PROCESS::<$S> = 1..3;
is $*S.VAR.^name, 'Scalar', 'an assigned dynamic is a Scalar';
is (my @c = $*S).raku, '[1..3,]', 'and stays one item in my @c = $*S';

class Ents does Iterable { method iterator { (1, 2, 3).iterator } }
PROCESS::<$E> := Ents.new;
is (my @d = $*E).elems, 3, 'a bound Iterable instance decomposes';

my $*D := 1..3;
is (my @e = $*D).raku, '[1, 2, 3]', 'a my $*D binding behaves the same';

PROCESS::<$I> := 7;
is $*I.VAR.^name, 'Int', 'a bound Int';
is $*I, 7, 'and reads back';

my $stored;
PROCESS::<$P> := Proxy.new(FETCH => sub ($) { $stored }, STORE => sub ($, $v) { $stored = $v });
$*P = 'via proxy';
is $*P, 'via proxy', 'a bound Proxy stays assignable';
