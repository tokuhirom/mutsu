# The `Intl::CLDR` shape: a `unit class` that declares a class in its body and
# then inherits from it. See t/oo/class/also-is-body-position.t.
use v6;
unit class AlsoIsBodyUnitNested;

class Selector is Positional {
    method AT-POS($i) { $i * 2 }
    method elems { 3 }
}

also is Selector;
