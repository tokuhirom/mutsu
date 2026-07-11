unit module CoercionReturnOtf;

# Module subs with coercion return types, formerly excluded from OTF
# compilation (def_is_otf_compilable_module_single). Pinned by
# t/module-sub-otf-coercion-return.t.

class Wrapped is export {
    has $.val;
    multi method COERCE($val) { self.new(:$val) }
}

our sub cr-str($x --> Str()) is export { $x }

our sub cr-int($x --> Int()) is export { $x }

our sub cr-str-numeric($x --> Str(Numeric:D)) is export { $x }

our sub cr-custom($x --> Wrapped:D()) is export { $x }

my subset SmallVal of Rat where { .abs < 0.5 };
our sub cr-subset($x --> SmallVal:D()) is export { $x }
