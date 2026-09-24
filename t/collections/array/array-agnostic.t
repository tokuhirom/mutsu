use Test;

plan 11;

ok Whatever eqv Whatever, 'Whatever values compare eqv';

my class X::Test::Agnostic is Exception {
    has $.object;
    has $.method;
    has $.clear;
    method message() {
        my $text = "No implementation of $.method method found for $.object.^name().";
        $!clear ?? "$text\nThis is needed to be able to clear an agnostic array." !! $text
    }
}

role TestAgnostic does Positional {
    method AT-POS($) is rw { ... }
    method elems() { ... }
    method BIND-POS($, $) {
        X::Test::Agnostic.new(object => self, method => 'BIND-POS', clear => False).throw
    }
    method EXISTS-POS($pos) { self.AT-POS($pos).defined }
    method DELETE-POS($) {
        X::Test::Agnostic.new(object => self, method => 'DELETE-POS', clear => $*DEFAULT-CLEAR).throw
    }
    method ASSIGN-POS($pos, \value) is raw { self.AT-POS($pos) = value }
    method STORE(\values) { self.CLEAR; self.ASSIGN-POS(0, values); self }
    method CLEAR() {
        my $*DEFAULT-CLEAR := True;
        self.DELETE-POS($_) for (^self.elems).reverse;
    }
}

my class HoleArray does TestAgnostic {
    has @!array;
    method AT-POS($pos) is rw { @!array.AT-POS($pos) }
    method elems() { @!array.elems }
    method DELETE-POS($pos) { @!array.DELETE-POS($pos) }
    method BIND-POS($pos, \value) is raw { @!array.BIND-POS($pos, value) }
    method shift() { @!array.shift }
    method unshift(\value) { @!array.unshift(value); self }
}

my class NoBindArray does TestAgnostic {
    has @!array;
    method AT-POS($pos) is rw { @!array.AT-POS($pos) }
    method elems() { @!array.elems }
    method DELETE-POS($pos) { @!array.DELETE-POS($pos) }
}

my class NoClearArray does TestAgnostic {
    has @!array;
    method AT-POS($pos) is rw { @!array.AT-POS($pos) }
    method elems() { @!array.elems }
}

my @a is HoleArray;
is((@a[4] = 42), 42, 'sparse assignment returns its value');
nok @a[0]:exists, 'a gap is absent';
nok @a[3]:exists, 'the last gap is absent';
is @a[4], 42, 'assigned value is present';
is @a.shift, Any, 'shifting a gap returns Any';
nok @a[0]:exists, 'a shifted gap stays absent';
is @a.unshift(666), @a, 'unshift returns the agnostic array';
nok @a[1]:exists, 'unshift preserves a gap';

my @bind is NoBindArray = 42;
throws-like { @bind[0] := 99 }, X::Test::Agnostic, method => 'BIND-POS', 'bind uses BIND-POS';

my @clear is NoClearArray = 42;
throws-like { @clear = () }, X::Test::Agnostic,
    message => "No implementation of DELETE-POS method found for NoClearArray.\nThis is needed to be able to clear an agnostic array.",
    'clear preserves its dynamic diagnostic';
