use v6;
use Test;
use lib 't/lib';

plan 5;

# A closure called after its block exited keeps the sigilless binding that
# shadowed an outer one of the same name, whatever the name's case.
my \U = 'outer';
my &f = do { my \U = 'inner'; -> { U } };
is f(), 'inner', 'a do block\'s uppercase sigilless term';

sub make-closure { my \U = 'in-sub'; -> { U } }
is make-closure()(), 'in-sub', 'a routine\'s uppercase sigilless term';

my \u = 'outer';
sub make-lower { my \u = 'in-sub'; -> { u } }
is make-lower()(), 'in-sub', 'a lowercase one, as before';

my \MubClass = 'caller';
my &c = do { use ModuleUnitBind::Vars; -> { MubClass.^name } };
is c(), 'ModuleUnitBind::Vars::MubClass', 'an imported type bound by a do block\'s use';

my &d;
{ use ModuleUnitBind::Vars; &d = -> { MubClass.^name } }
is d(), 'ModuleUnitBind::Vars::MubClass', 'and by a plain block\'s use';
