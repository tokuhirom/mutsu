use Test;

# `%s` puts its argument in string context, so it dispatches `.Str` exactly like
# `~$obj` does. That includes a `.Str` the class only inherits — an Exception
# subclass gets its `Str` from `message` up the MRO, and gating the dispatch on
# an *own* `Str` method printed the raw object instead.

plan 6;

class Plain {
    has $.v;
    method Str { "plain<$!v>" }
}

role X[Str:D $err] is Exception {
    has $.str;
    method message { "$err <<$!str>>" }
}
class X::NotFound does X['not found'] { }

class Wrapper {
    class Inner is Exception {
        has $.what;
        method message { "inner: $!what" }
    }
}

is sprintf('%s', Plain.new(:v<1>)), 'plain<1>', 'an own .Str is used';
is sprintf('%s', X::NotFound.new(:str<abc>)), 'not found <<abc>>',
    'an inherited Exception .Str is used';
is sprintf('%s', Wrapper::Inner.new(:what<x>)), 'inner: x',
    'the same through a package-qualified name';
is sprintf('%s: %s', 'WARN', X::NotFound.new(:str<abc>)), 'WARN: not found <<abc>>',
    'and alongside other directives';
is sprintf('%s', X::NotFound.new(:str<abc>)), ~X::NotFound.new(:str<abc>),
    'sprintf %s agrees with the ~ prefix operator';

class Numish {
    method Int { 42 }
}
is sprintf('%d', Numish.new), '42', 'numeric directives still dispatch .Int';
