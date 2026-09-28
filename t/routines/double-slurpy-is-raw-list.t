use Test;

# `**@x is raw` binds an un-itemized List, not an Array, so a later `*@`
# slurpy flattens its sub-lists. Found via Hash::Agnostic's
# `method new(**@values is raw)` (used by BSON::Simple through Hash::Ordered):
# `Hash::Ordered.new(())` handed `STORE(*@values)` a stray `()` and died
# with X::Hash::Store::OddNumber.

plan 6;

sub flat(*@v) { @v.raku }
sub raw(**@values is raw) { @values }
sub plain(**@values) { @values }

is raw(()).raku, '((),)', '**@ is raw binds a List';
is flat(raw(())), '[]', 'a *@ slurpy flattens the raw empty List away';
is flat(raw((1, 2))), '[1, 2]', 'a *@ slurpy flattens the raw sub-list';
my @x = 1, 2;
is flat(raw(@x, 3)), '[1, 2, 3]', 'an Array argument flattens too';

is plain(()).raku, '[(),]', 'plain **@ still binds an Array';
is flat(plain((1, 2))), '[(1, 2),]', 'its itemized elements do not flatten';
