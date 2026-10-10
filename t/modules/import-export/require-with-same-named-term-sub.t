use Test;

# IP::Addr: `sub term:<IP::Addr> () { once require IP::Addr }` -- the operand of
# `require` is a module name even when a term of the same name is declared.

plan 2;

use lib 't/lib';

sub term:<RequireTermSub::Target> () { once require RequireTermSub::Target }

is RequireTermSub::Target.^name, 'RequireTermSub::Target', 'term sub requires the module';
is RequireTermSub::Target.new.hi, 'hi', 'loaded class is usable';
