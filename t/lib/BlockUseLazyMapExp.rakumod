unit module BlockUseLazyMapExp;
proto sub lazy-mexp(|) is export {*}
multi sub lazy-mexp(Int $x) { 'int' }
multi sub lazy-mexp(Str $x) { 'str' }
