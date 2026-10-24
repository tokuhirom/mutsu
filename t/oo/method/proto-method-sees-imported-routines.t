use v6;
use Test;
use lib 't/lib-proto-method-imports';
use ProtoImpClass;

# A `proto method` body is compiled in the lexical context of its class: the
# routines the file's `use` imported are visible in it (mutsu#14870,
# Pakku.rakumod's `proto method fly { {*}; CATCH { default { log ... } } }`).
plan 3;

is ProtoImpClass.new.run, 'plain/m:plain/multi-a', 'a proto body calls imported subs around {*}';
is ProtoImpClass.new.guarded, 'caught:plain', 'a CATCH in a proto body calls an imported sub';

class Local {
  proto method p ( | ) { {*} ~ sub-in-main() }
  multi method p ( ) { 'x' }
}
sub sub-in-main { '!' }
is Local.new.p, 'x!', 'a proto body still sees a sub of the main script';
