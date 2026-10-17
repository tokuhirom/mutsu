use Test;

# The class methods of `CompUnit::Loader`, `Deprecation` and
# `Rakudo::Internals` are class-method rows (ADR-11276 §9.159).

plan 6;

my $handle = CompUnit::Loader.load-source('our $loaded-x = 5;'.encode);
isa-ok $handle, CompUnit::Handle, 'load-source answers a CompUnit::Handle';
ok CompUnit::Loader.can('load-source'), 'CompUnit::Loader can load-source';
ok CompUnit::Loader.can('load-source-file'), 'and load-source-file';

ok Deprecation.can('report'), 'Deprecation can report';

Rakudo::Internals.REGISTER-DYNAMIC: '$*MUTSU-ROWS-TEST', { PROCESS::<$MUTSU-ROWS-TEST> = 'ok' };
is $*MUTSU-ROWS-TEST, 'ok', 'REGISTER-DYNAMIC installs the dynamic variable';
ok Rakudo::Internals.can('REGISTER-DYNAMIC'), 'Rakudo::Internals can REGISTER-DYNAMIC';
