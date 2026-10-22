module TypedHashLaneBlock {
    proto sub put-any(|) is export {*};
    my Lock $lock .= new;
    my %seen{Any};
    multi sub put-any(Hash:D $h) { $lock.protect: { %seen{$h} //= 1 }; %seen.elems }
    multi sub put-any(List:D $l) { $lock.protect: { %seen{$l} //= 1 }; %seen.elems }
}
