use Test;
use lib $?FILE.IO.parent(3).add('lib').Str;

# From Air::Plugin::Donate (ecosystem parity): it writes `--> Markup()` in its
# own role methods, where `Markup` is a SHORT name imported from
# `Air::Functional`. An imported type name is a lexical of the importing
# compunit, and the module load restores the caller's scope over that `env`
# entry -- so from a routine declared in that compunit the coercion's alias
# lookup found nothing, left the constraint as the unregistered short name, and
# died with X::Coerce::Impossible. The tell was that `Markup ~~ Str` answered
# True from the very same frame: type MATCHING already consulted the surviving
# per-package record of the import, and only the coercion's alias resolution
# did not.

use ImportedCoercionType;
use ImportedCoercionUser;

plan 6;

is wrap('a').Str, '[a]',
    'the declaring module coerces through its own short name';
is from-sub().Str, 'sub',
    'an imported type name resolves as a coercion target in an importing sub';
is ICClass.new.from-method().Str, 'method',
    '... and in a method of a class declared there';
is make-ic-role().from-role-method().Str, 'role',
    '... and in a method of a role declared there';
is name-still-resolves(), 'ImportedCoercionType::Wrapped',
    'the short name resolved to the fully-qualified role all along';
ok from-sub() ~~ Wrapped,
    'the coerced value does the imported role';
