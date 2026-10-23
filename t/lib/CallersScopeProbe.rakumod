unit module CallersScopeProbe;

sub probe is export {
    (so ::CALLERS::<&__scope_marker>, so ::CALLERS::OUTERS::<&__scope_marker>)
}

class Checker is export {
    method check { (so ::CALLERS::<&__declared_here>, so ::CALLERS::OUTERS::<&__declared_here>) }
    method check-with-catch {
        my @r = (so ::CALLERS::<&__declared_here>, so ::CALLERS::OUTERS::<&__declared_here>);
        CATCH { default { .rethrow } }
        @r.List
    }
}

class Seeker is export {
    sub __declared_here { True }

    method ask { Checker.check }
    method ask-with-catch { Checker.check-with-catch }
}

# This unit imports nothing: a routine the program imported is not in its scope.
class Prober is export {
    method ask { probe() }
}
