use Test;

# A routine whose signature declares `$_` itself keeps the coerced value when
# called through `.map(&f)` (CSS::Specification::Compiler:
# `sub lit-ws(Str:D() $_) { .&lit'r.&ws }` mapped over numbers).

plan 4;

sub name-of(Str:D() $_) { .^name }
is-deeply (35, 7).map(&name-of).List, ('Str', 'Str'), 'a List source';
my @a = 35, 7;
is-deeply @a.map(&name-of).List, ('Str', 'Str'), 'an Array source';

sub strict(Str:D $s) { $s }
sub via(Str:D() $_) { .&strict }
is-deeply @a.map(&via).List, ('35', '7'), 'the coerced topic reaches a strict callee';

my $b = { $_ = 9 };
my $v = 1;
$b($v);
is $v, 9, 'a bare block still binds $_ to the argument itself';
