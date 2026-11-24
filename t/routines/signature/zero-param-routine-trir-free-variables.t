use Test;

# #19136: a parameterless routine runs its TRIR chunk on every cached call,
# free variables and all. A program-scope `sub sv { $s }` reads the program's
# `$s` even after a routine's nested `sub sv { $s }` of the same name, package
# and file has run, and a resuming CONTROL handler's writes are seen on every
# call. Checked against rakudo.

plan 10;

my $s = 'main-s';
sub sv { $s }
is sv(), 'main-s', 'the program-scope sub reads the program variable';
sub mk2 { my $s = 'mk2-s'; my $r = sv(); sub sv { $s }; $r }
is mk2(), 'mk2-s', 'the nested sub of the same name reads its routine variable';
is mk2(), 'mk2-s', '...on each call';
is sv(), 'main-s', 'the program-scope sub still reads the program variable';
is (sv() for ^5).join(','), 'main-s,main-s,main-s,main-s,main-s',
    '...on every cached call';

my $count = 0;
my @seen;
sub cw { warn "cw" }
for 1..5 {
    cw();
    @seen.push: $count;
    CONTROL { when CX::Warn { $count++; .resume } }
}
is @seen.join(','), '1,2,3,4,5', 'a resuming CONTROL handler counts every call';

my $total = 0;
sub bump { $total += 2 }
bump() for ^6;
is $total, 12, 'a parameterless routine writes its free variable on every call';

my @log;
sub logit { @log.push: @log.elems }
logit() for ^4;
is @log.join(','), '0,1,2,3', 'a parameterless routine reads its free array afresh';

# A `warn` raised in the block itself after one raised in a called routine:
# the handler starts from the variable's current value either way.
my $mixed = 0;
my @mixed;
sub cw1($x) { warn "cw" }
for 1..6 {
    $_ == 2 ?? cw1(1) !! warn("w");
    @mixed.push: $mixed;
    CONTROL { when CX::Warn { $mixed++; .resume } }
}
is @mixed.join(','), '1,2,3,4,5,6', 'a direct warn after a warn from a call counts too';

my $alt = 0;
my @alt;
sub cw2(Int $x) { warn "cw" }
for 1..6 {
    $_ %% 2 ?? cw2(1) !! cw2(|(1,));
    @alt.push: $alt;
    CONTROL { when CX::Warn { $alt++; .resume } }
}
is @alt.join(','), '1,2,3,4,5,6', 'calls alternating between call paths count every warn';
