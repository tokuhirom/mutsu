use Test;

# A named sub declared inside a method that rebinds a variable of the method
# with `:=` must change the method's variable, as it does from a plain
# routine. The rebind used to replace the sub's own env entry only, so a
# sibling sub (or the method) read the old binding once the sub was called
# after the method returned. Found by PDF 0.6.17's `rw-accessor`. Expected
# values are the `raku` (v2026.09) output.

plan 5;

class C {
    method pair() { my $val; sub F($) { $val := 42 }; sub G() { $val }; &F, &G }
    method assigned() { my $val; sub F($) { $val = 42 }; sub G() { $val }; &F, &G }
    method fetch() {
        my $val;
        my int $got = 0;
        sub FETCH($) { $got ||= do { $val := self.x; 1 }; $val }
        &FETCH
    }
    has $.x = 7;
}

my ($f, $g) = C.pair;
$f(1);
is $g(), 42, 'a sub declared in a method rebinds its variable (:=)';

($f, $g) = C.assigned;
$f(1);
is $g(), 42, 'and assigns it (=)';

my &fetch = C.new.fetch;
is fetch(1), 7, 'a cached fetch sub binds once';
is fetch(1), 7, '... and the binding survives the second call';

sub top() { my $val; sub F($) { $val := 42 }; sub G() { $val }; &F, &G }
($f, $g) = top();
$f(1);
is $g(), 42, 'a plain routine behaves the same';
