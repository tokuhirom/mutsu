use Test;

plan 3;

my Signal:D @signals = Signal.enums.values.grep(* > 0).map({ Signal($_) });

unless +@signals {
    skip-rest "no valid signals were found on this platform ({$*KERNEL.osname})";
    exit 1;
}

lives-ok { my Signal:D $one = @signals.head }, 'typed Signal:D scalar accepts enum value';
lives-ok { my Signal:D @copy = @signals }, 'typed Signal:D array accepts enum values';
lives-ok { signal((@signals.head,), @signals.tail) }, 'signal accepts list and scalar signal arguments';
