unit module ModuleStateSub;

# A module sub with a scalar `state` variable. Callers dispatch it through the
# captured shared body (compiled_fns expansion) so its `state` cell is shared
# across `start` threads instead of each thread re-OTF-compiling a distinct body
# with its own cell.
our sub bump() is export {
    state $n = 0;
    $n++;
}

# Pre-increment variant returning the new value.
our sub tick() is export {
    state $c = 0;
    return ++$c;
}

# A module sub with an aggregate `state`.
our sub collect($x) is export {
    state @seen;
    @seen.push($x);
    return @seen.elems;
}
