use Test;

plan 4;

# A `return` inside a tap/quit callback returns from the callback's lexically
# enclosing routine (the sub that called `.emit`), matching Rakudo. The
# supplier emit dispatch used to wrap the return signal into a supply-failure
# error ("Attempt to return outside of any Routine"). Cro::HTTP's parser
# tests use this shape in their `refuses` helper.

sub tap-return() {
    my $s = Supplier.new;
    $s.Supply.tap: -> $v { return "from-tap" };
    $s.emit(1);
    return "fell-through";
}
is tap-return(), "from-tap", 'return in a tap callback returns from the enclosing sub';

sub quit-return() {
    my $in = Supplier.new;
    my $out = supply { whenever $in.Supply -> $v { die "boom" } };
    $out.tap: -> $v { }, quit => -> $ex { return "from-quit" };
    $in.emit(1);
    return "fell-through";
}
is quit-return(), "from-quit", 'return in a quit handler returns from the enclosing sub';

# The method-defined supply variant (Cro's transformer shape): the quit
# handler's return must still find the enclosing sub even though the supply
# block was created inside a method of another class.
class TCNR-P {
    method transformer($in) {
        supply { whenever $in -> $v { die "boom2" } }
    }
}
sub method-supply-quit-return() {
    my $in = Supplier.new;
    TCNR-P.new.transformer($in.Supply).tap:
        -> $v { },
        quit => -> $ex { return "from-method-quit" };
    $in.emit(1);
    return "fell-through";
}
is method-supply-quit-return(), "from-method-quit",
    'quit-handler return works when the supply came from a method';

# A method call must not retarget later closures in the caller
# (the ambient callable-id must not leak through the method env merge).
class TCNR-Q { method t() { supply { } } }
sub closure-after-method-call() {
    TCNR-Q.new.t;
    my $cb = -> { return "early" };
    $cb();
    return "late";
}
is closure-after-method-call(), "early",
    'closure return still targets its sub after an intervening method call';
