use v6;
use Test;

# Assignment to a typed container ATTRIBUTE (`has Str @!c`, `has Int %!h`)
# keeps the declared element type: the stored container reads back as
# Array[T]/Hash[T] and satisfies a matching return-type constraint, and a
# wrong-typed element is rejected — the declared type lives in the class
# registry, not the name-keyed lexical constraint map (Text::CSV's
# `column_names (*@c --> Array[Str])` returning `has Str @!cnames`).

plan 7;

class WithArr {
    has Str @!cnames;
    method set (*@c --> Array[Str]) {
        @c.elems and @!cnames = @c.map({.Str});
        @!cnames;
    }
    method direct (*@x) { @!cnames = @x; @!cnames.WHAT }
}

my $c = WithArr.new;
is-deeply $c.set("bar", "baz", "foo"), Array[Str].new("bar", "baz", "foo"),
    'typed attr array returned through an Array[T] return constraint';
is $c.set().elems, 3, 'state persists (empty call returns existing)';
is $c.direct("a", "b").raku, 'Array[Str]', 'plain @!attr = @x keeps Array[T]';

class WithHash {
    has Int %!h;
    method set (%v) { %!h = %v; %!h.WHAT }
}
is WithHash.new.set({a => 1}).raku, 'Hash[Int]', '%!attr = %v keeps Hash[T]';

class BadElem {
    has Int @!c;
    method bad { @!c = "x" }
}
throws-like { BadElem.new.bad }, Exception,
    message => /'expected Int but got Str'/,
    'wrong-typed element is rejected';

class Untyped {
    has @!c;
    method set (*@x) { @!c = @x; @!c.WHAT }
}
is Untyped.new.set(1, "b").raku, 'Array', 'untyped attr array stays plain Array';

class MuTyped {
    has Mu @!c;
    method set (*@x) { @!c = @x; @!c.elems }
}
is MuTyped.new.set(1, Any), 2, 'Mu-typed attr accepts anything';
