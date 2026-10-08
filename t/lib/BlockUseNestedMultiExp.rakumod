unit module BlockUseNestedMultiExp;
proto sub nested-mexp(|) is export {*}
multi sub nested-mexp(Int $x) { 'int' }
multi sub nested-mexp(Str $x) { 'str' }
