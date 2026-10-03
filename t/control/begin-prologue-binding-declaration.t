use Test;

# A binding declaration (`my $x := …`) in a unit with a BEGIN-time effect (a
# BEGIN, a `constant`, a `use`) gets a static half in the BEGIN prologue, like
# an assigned declaration does (ADR-0134). Without it a routine the prologue
# took was compiled before the name was declared and lost its capture of the
# binding (#11263).

plan 8;

my $imm := 42;
sub write-imm { $imm = 1 }
sub shadow-imm($imm) { write-imm() }
throws-like { shadow-imm(1) }, Exception, message => 'Cannot assign to an immutable value',
    'an immutable binding refuses a write from a sub whose caller has a same-named param';
is $imm, 42, 'the binding is unchanged';

my $src = 10;
my $alias := $src;
sub bump-alias { $alias++ }
bump-alias();
is $src, 11, 'a sub writes through a container binding';

my @a := [1, 2];
sub push-a { @a.push(3) }
push-a();
is @a.join(','), '1,2,3', 'a sub sees an array binding';

my %h := { k => 'v' };
sub read-h { %h<k> }
is read-h(), 'v', 'a sub sees a hash binding';

my $seen-at-begin;
my $late := 7;
BEGIN $seen-at-begin = $late.raku;
is $seen-at-begin, 'Any', 'BEGIN sees the binding\'s static state';
is $late, 7, 'the binding runs at its own position';

my Int $typed := 5;
sub read-typed { $typed }
is read-typed(), 5, 'a typed binding declaration';

constant $c = 5;
