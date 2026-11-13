use lib $*PROGRAM.parent.parent.add('lib');
use Test;

# A module routine names its own `my package Enums`'s `our enum Status` as
# `Enums::Status`. Called from this program, the routine's scope no longer
# binds `Enums`; the name must still resolve in the routine's own lexical scope,
# as rakudo resolves it at compile time. The program itself never sees the
# module's lexical package. From the KeyNub::LicDongle distribution.

use MyPackageQualifiedMember;

plan 5;

is X::MyPackageQualified.new(code => -1).status, 'Bad',
    'a method reads the module-lexical enum by its qualified name';
is X::MyPackageQualified.new(code => 7).status.^name, 'Status',
    'the enum type object is the fallback for an unknown code';
is status-type().^name, 'Status', 'an exported sub returns the enum type';
is status-name(), 'Status', 'a method call on the qualified name works';

throws-like { EVAL 'Enums::Status' }, Exception,
    "the program cannot name the module's lexical package";
