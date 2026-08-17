use Test;

# `PROCESS::<$X>`'s stash view was built by scanning only `self.env` (the
# CURRENT frame's own dynamic-var store), not the whole dynamic-scope
# caller chain. Since a dynamic var set at the mainline must remain visible
# from a callee sub (that's the whole point of "dynamic"), a value written
# via `PROCESS::<$X> = ...` at the mainline silently vanished once read from
# inside a sub -- Log::Timeline sets its output backend this way
# (`PROCESS::<$LOG-TIMELINE-OUTPUT> = $output`) and every logging call from
# inside the module's subs saw it as undefined, so nothing ever got logged.
# Verified against raku directly.

plan 4;

PROCESS::<$T_FOO> = 42;

sub reader() {
    is PROCESS::<$T_FOO>, 42, 'PROCESS::<$X> set at the mainline is visible from a sub';
    ok PROCESS::<$T_FOO>.defined, '... and .defined agrees';
}
reader();

class Recorder {
    has @.entries;
    method record($x) { @.entries.push($x); }
}

sub log-if-present() {
    return unless PROCESS::<$T_RECORDER>.defined;
    PROCESS::<$T_RECORDER>.record("hi");
}

my $rec = Recorder.new;
PROCESS::<$T_RECORDER> = $rec;
log-if-present();
is $rec.entries.elems, 1, 'a value read via PROCESS:: from a sub can be used, not just seen';
is $rec.entries[0], 'hi', '... with the right recorded value';
