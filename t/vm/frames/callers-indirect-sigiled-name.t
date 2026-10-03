use Test;

# `CALLERS::('$_')` -- the indirect spelling of `CALLERS::<$_>` -- reads a
# caller frame's variable, walking the call stack as `$::('CALLERS::_')` does.
# It used to resolve against a stash snapshot that holds no caller variables
# and died "No such symbol 'CALLERS::$_'" (AkeTester's `$cwd //= CALLERS::('$_')`).
# The block reads its topic so rakudo's optimizer does not lower it away.

plan 2;

sub topic-of-caller(:$cwd is copy) { $cwd //= CALLERS::('$_'); $cwd }

given 'dir' {
    my $seen = $_;
    is topic-of-caller(), 'dir', 'CALLERS::("$_") reads the calling block topic';
    is topic-of-caller(:cwd<given>), 'given', 'a defined default is kept';
}

