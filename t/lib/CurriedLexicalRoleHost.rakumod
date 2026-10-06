unit module CurriedLexicalRoleHost;

# A class whose `^parameterize` curries a lexical parametric role declared in
# its own body, the way upstream NativeCall's `CArray[T]` does.
class Box is export {
    my role Typed[::T] {
        method of-type { T }
    }

    method ^parameterize(Mu:U \box, Mu:U \t) {
        my $what := box.^mixin(Typed[t.WHAT]);
        $what.^set_name(box.^name ~ '[' ~ t.^name ~ ']');
        $what
    }
}

class Maker is export {
    method make(Mu:U \t) { Box[t] }
    method make-str { Box[Str] }
}
