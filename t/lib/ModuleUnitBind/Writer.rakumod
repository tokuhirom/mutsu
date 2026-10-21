unit module ModuleUnitBind::Writer;
use ModuleUnitBind::Vars;

# The module's mainline writes the variable its own `use` imported.
$mub-value = 'written';

sub writer-reads() is export { $mub-value ~ '/' ~ @mub-list.join(',') }
sub writer-closure() is export { -> { $mub-value ~ '/' ~ MubClass.new.hi } }
sub writer-typed(MubClass $c) is export { $c.hi }
