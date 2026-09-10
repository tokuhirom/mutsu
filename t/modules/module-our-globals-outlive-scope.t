use Test;

# A module's `our` declarations belong to its package, so they must outlive
# whatever scope happened to trigger the load. `our` variables live in the
# global env, and a sub call restores that env wholesale on return, so a module
# loaded by a `require` inside a sub lost its package variables — while
# `loaded_modules` still claimed it was loaded, making a later `use` a no-op that
# could not bring them back.
#
# DBIish hit this through `DBIish.install-driver`, which `require`s a driver
# inside a method: the driver pulls in NativeLibs, and the next driver's
# `use NativeLibs` then could not see `NativeLibs::is-win`.

plan 7;

use lib 't/lib';

sub load-in-a-sub($name) {
    my \M = (require ::($name));
    M;
}

load-in-a-sub('OurGlobalsBase');

# The `use` is a no-op (the module really is loaded), but it must still leave the
# package's `our` symbols reachable.
use OurGlobalsBase;

is OurGlobalsBase::answer, 42, 'an `our constant` survives the loading scope';
is $OurGlobalsBase::greeting, 'hi', 'an `our` scalar survives';
is @OurGlobalsBase::items.join(','), '1,2,3', 'an `our` array survives';
is %OurGlobalsBase::config<mode>, 'fast', 'an `our` hash survives';

# The same shape one level deeper: the second module reads the first one's
# package symbol while its own body runs.
sub load-user() {
    my \M = (require ::('OurGlobalsUser'));
    M;
}
# OurGlobalsUser dies on load if it cannot read the right value, so this covers
# both reachability and correctness. (`require` does not import into the
# compile-time scope, so the sub cannot be called by name from here.)
lives-ok { load-user() },
    'a module that reads another module\'s `our` symbol loads after that module was loaded in a sub';

# A module loaded at the top level was never affected; check it still is not.
{
    my \M = (require ::('OurGlobalsBase'));
    is OurGlobalsBase::answer, 42, 'a top-level require keeps working';
}

# Assignments made after the load are not clobbered by the reinstatement.
$OurGlobalsBase::greeting = 'bye';
use OurGlobalsBase;
is $OurGlobalsBase::greeting, 'bye', 'a later assignment is not overwritten by a re-use';
