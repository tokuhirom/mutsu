unit module ClassSeesModuleSubs;

# A `class` declared inside a `module` must be able to call the module's own
# routines by their bare name — the lexical scope of the class body is the
# module's. Shaped after NativeLibs' `class Searcher` calling the module-scope
# `cannon-name`, the blocker behind five DBIish test files.

our sub cannon-name($libname) { "cn:$libname" }

sub lexical-helper($n) { $n * 3 }

our constant MARK = 'mark';

our proto sub pick(|) {*}
multi sub pick(Str $s) { "str:$s" }
multi sub pick(Int $i) { "int:$i" }

class Searcher {
    method try-versions($libname) { cannon-name($libname) }
    method via-lexical($n) { lexical-helper($n) }
    method marked() { MARK }
    method dispatch($x) { pick($x) }
}

class Outer {
    class Inner {
        method deep($libname) { cannon-name($libname) }
    }
}

module Nested {
    our sub inner-sub() { 'inner' }
    class Deep {
        method both() { inner-sub() ~ '/' ~ cannon-name('x') }
    }
}
