unit module PgLikeNative;
use NativeCall;

# Mirrors DBDish::Pg::Native's shape: a CPointer class declared inside a
# `unit module` (so it registers package-qualified), whose native methods
# leave the invocant implicit and which also carries ordinary Raku methods.
class StrHandle is export is repr('CPointer') {
    method strlen(--> size_t) is native { * }
    method is-ok { self.strlen > 0 }
}

sub make-handle(str $s --> StrHandle) is export is native is symbol('strdup') { * }
sub free-handle(StrHandle $h) is export is native is symbol('free') { * }
