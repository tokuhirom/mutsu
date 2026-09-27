unit module NewlineDie;

#| The shape of the `Die` distribution: a message ending in a newline is
#| printed without a backtrace; every other `die` call must still reach the
#| core routine.
multi sub die(Cool:D $msg where .ends-with: "\n") is export {
    note $msg.chop;
    exit 1;
}
