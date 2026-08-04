use Test;

# ADR-0019 C5: a declaration's custom-trait argument is lowered by the compiler
# into a declaration-time chunk (or kept as a constant when it is one) and run
# through the VM's re-entrant bytecode entry at registration. Pin that every
# argument shape still reaches `trait_mod:<is>` with the value it evaluates to.

plan 8;

my @log;
multi sub trait_mod:<is>(Routine $r, :$labelled!) { @log.push("sub:{$r.name}:{$labelled}") }
multi sub trait_mod:<is>(Mu:U $t, :$labelled!)    { @log.push("type:{$t.^name}:{$labelled}") }

constant PREFIX = 'p';

sub literal-arg() is labelled('lit') { 1 }
sub computed-arg() is labelled(PREFIX ~ '-computed') { 2 }
sub call-arg() is labelled(join('-', PREFIX, 'called')) { 3 }

class TaggedClass is labelled(PREFIX ~ '-class') { }
role TaggedRole is labelled(PREFIX ~ '-role') { }

is literal-arg(), 1, 'sub with a literal trait argument stays callable';
is computed-arg(), 2, 'sub with a computed trait argument stays callable';
is call-arg(), 3, 'sub with a call-valued trait argument stays callable';

ok @log.grep('sub:literal-arg:lit'), 'literal trait argument reaches trait_mod';
ok @log.grep('sub:computed-arg:p-computed'), 'computed trait argument is evaluated';
ok @log.grep('sub:call-arg:p-called'), 'a call in a trait argument is evaluated';
ok @log.grep('type:TaggedClass:p-class'), 'a class trait argument is evaluated';
ok @log.grep('type:TaggedRole:p-role'), 'a role trait argument is evaluated';
