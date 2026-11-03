use Test;

plan 11;

# A custom HOW composing Metamodel::MultipleInheritance carries the `hides`
# state rakudo's role has (#16239).
class K is Metamodel::ClassHOW does Metamodel::MultipleInheritance { }
my $k = K.new;
is-deeply $k.hides(K).WHAT, Mu, 'hides of a fresh HOW is Mu';
is $k.hidden(K), 0, 'hidden is 0 until set_hidden';
is $k.hides_parent(K, Int), 0, 'no parent is hidden on a fresh HOW';
lives-ok { $k.exclude_parent(Int) }, 'exclude_parent takes the parent type';
$k.set_hidden(K);
is $k.hidden(K), 1, 'set_hidden flips hidden';

# Metamodel::Mixins' cache and generate_mixin.
class M is Metamodel::ClassHOW does Metamodel::Mixins { }
my $m = M.new;
lives-ok { $m.setup_mixin_cache(M) }, 'setup_mixin_cache';
lives-ok { $m.flush_cache(M) }, 'flush_cache';
ok $m.can(M, 'generate_mixin'), 'generate_mixin is a method of the HOW';
is $m.is_mixin(M), 0, 'is_mixin is 0 on a fresh HOW';
$m.set_is_mixin;
is $m.is_mixin(M), 1, 'set_is_mixin flips it';
dies-ok { $k.hides_parent }, 'hides_parent needs its arguments';
