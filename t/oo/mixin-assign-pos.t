use Test;

# `$x[i] = v` / `$x<k> = v` on an object with roles mixed in dispatches the
# role's ASSIGN-POS / ASSIGN-KEY, as on a plain instance. It used to replace
# the variable's object with a fresh Array/Hash.

plan 8;

role Pos {
    has @.log;
    multi method ASSIGN-POS(::?CLASS:D: Int:D $i, \v) { @!log.push("$i=" ~ v); v }
}
role Key {
    has @.log;
    multi method ASSIGN-KEY(::?CLASS:D: Str:D $k, \v) { @!log.push("$k=" ~ v); v }
}
class C { }

my $a = C.new but Pos;
$a[2] = 'z';
is $a.^name, 'C+{Pos}', 'a `but` object keeps its type after an element assignment';
is $a.log, ['2=z'], 'the role ASSIGN-POS ran';
is ($a[0] = 'q'), 'q', 'the assignment yields what ASSIGN-POS returns';

my $b = C.^mixin(Pos).new;
$b[1] = 4;
is $b.^name, 'C+{Pos}', 'an instance of a .^mixin type keeps its type';
is $b.log, ['1=4'], 'and its role ASSIGN-POS ran';

my $h = C.new but Key;
$h<k> = 'v';
is $h.^name, 'C+{Key}', 'a keyed assignment keeps the object';
is $h.log, ['k=v'], 'the role ASSIGN-KEY ran';

# Without a role ASSIGN-POS the object is not replaced by this path either.
role Plain { }
my $p = C.new but Plain;
lives-ok { my %x = a => ($p but Plain) }, 'an object without the protocol is unaffected';
