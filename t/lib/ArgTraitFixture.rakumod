unit module ArgTraitFixture;

class Marker is export { }
role FromMarker is export { }
role FromStr is export { }

# Two candidates that only differ by the *value*'s type -- exactly the shape
# Getopt::Long uses for `is option<!>` (an `Argument` vs a `Str` candidate on
# the same external name). Picking the right one requires the real argument
# value to reach dispatch instead of a hardcoded `True`, which matches
# neither type.
multi trait_mod:<is>(Parameter:D $p, Marker:D :$tag!) is export {
    $p does FromMarker;
}
multi trait_mod:<is>(Parameter:D $p, Str:D :$tag!) is export {
    $p does FromStr;
}

# A candidate whose body can fail for a reason that has nothing to do with
# whether the trait is "known" -- proves a real body error is not swallowed
# and reported as "unknown trait".
multi trait_mod:<is>(Parameter:D $p, Str:D :$strict-tag!) is export {
    die "bad strict-tag value: $strict-tag" unless $strict-tag eq 'ok';
    $p does FromStr;
}
