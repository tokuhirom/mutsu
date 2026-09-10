use v6;
use Test;
use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test::Util;

# A NativeCall prelude helper (`cglobal`, `nativecast`, `nativesizeof`,
# `explicitly-manage`, `refresh` -- see `inject_nativecall_subs_prelude`) is an
# ambient GLOBAL routine: every compunit that so much as mentions NativeCall
# carries its own identical copy, and only the first registration wins.
#
# The block-lexical-sub escape hatch used to take them anyway, storing one
# env-captured copy per module under its reserved key. The *last* module loaded
# then answered a later `cglobal` call with its own closure env, so DBIish's
# mysql driver probed the library through the SQLite driver's scope and threw
#
#     Cannot load native library 'libmariadb.so.0'
#
# where upstream expects a soft failure -- `install-driver` must succeed and the
# absence of the client library show up only in `.version`. It took DBIish's
# whitelisted `01-basic.rakutest` from 35/35 to 27/35 in the bundled-library
# gate, and `make test` never saw it.
#
# Run in a subprocess with the module directories on `-I`, which is how the gate
# invokes the upstream suite: the shape is delicate, and reaching DBIish through
# the bundled-battery path resolves `cglobal` from a different scope and hides
# the bug. The topic-variable `for` is load-bearing for the same reason --
# rewriting it to a named parameter, or putting an assertion between the two
# installs, also hides it.

plan 1;

my $root = $*PROGRAM.parent(2);
my @inc = <modules/DBIish/lib modules/NativeLibs/lib modules/NativeHelpers-Blob/lib>
    .map({ '-I' , $root.add($_).absolute })
    .flat;

my $code = q:to/CODE/;
    use Test;
    plan 2;
    use DBIish;
    for <SQLite mysql> {
        my $drv;
        lives-ok { $drv = DBIish.install-driver($_); }, "install '$_'";
    }
    CODE

is_run $code,
    { status => 0, out => "1..2\nok 1 - install 'SQLite'\nok 2 - install 'mysql'\n" },
    :compiler-args(@inc),
    'a driver loaded after another still probes its own library';
