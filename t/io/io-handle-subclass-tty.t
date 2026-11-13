use Test;

# An IO::Handle that was never opened is no terminal, and a subclass of
# IO::Handle reaches the handle methods.

plan 5;

is IO::Handle.new.t, False, 'unopened IO::Handle.t';
class MM is IO::Handle { has $.x }
my $m = MM.bless(:x(1));
is $m.t, False, 'unopened subclass .t';
is $m.opened, False, 'unopened subclass .opened';
ok MM.can('lock'), 'subclass can lock';
ok MM.can('spurt'), 'subclass can spurt';
