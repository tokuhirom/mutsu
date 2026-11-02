use Test;

plan 7;

# An `@`/`%` attribute's `is default(...)` survives assignment into it (#15547).
class P {
    has %!dflt is default(7);
    has @!arr is default(5);
    method set(%x) { %!dflt = %x; self }
    method dflt() { %!dflt }
    method seta(@x) { @!arr = @x; self }
    method arr() { @!arr }
}

is P.new.dflt<zzz>, 7, 'a fresh hash attribute answers its default';
is P.new.set({ a => 1 }).dflt<zzz>, 7, 'and still does after %!h = %x';
is P.new.set({ a => 1 }).dflt<a>, 1, 'while stored keys read back';
is P.new.arr[3], 5, 'an array attribute answers its default';
is P.new.seta([1]).arr[5], 5, 'and still does after @!a = @x';

class C is P { }
is C.new.set({ a => 1 }).dflt<zz>, 7, 'an inherited attribute keeps the default of its declaring class';

class N { has %!h; method set(%x) { %!h = %x; self }; method h { %!h } }
is N.new.set({ a => 1 }).h<zz>.raku, 'Any', 'an attribute without a default is unchanged';
