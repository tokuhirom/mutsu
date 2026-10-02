unit module Issue9925::Mid;
use NativeCall;
use Issue9925::Vars;

# `require` from inside a method: the frame that loads the module is a
# method frame, not the unit body, and is gone by the time the result is used.
class I9925Loader is export {
    method load() {
        require Issue9925::Late;
        $i9925-value ~ '/' ~ I9925Class.new.hi ~ '/' ~ &Issue9925::Late::late()
    }
}

# An EVAL nested in an imported routine sees the routine's unit imports.
sub i9925-eval() is export {
    EVAL q[$i9925-value ~ '/' ~ I9925Class.new.hi ~ '/' ~ i9925-tag('e')]
}

# A closure handed to a native callback: libc's qsort calls back into it,
# and the closure reaches this unit's imported variable and sub.
sub i9925-qsort(CArray[int32], size_t, size_t,
                &cmp (Pointer, Pointer --> int32))
    is native is symbol('qsort') { * }

sub i9925-native-sorted(*@values) is export {
    my $a = CArray[int32].new(@values);
    my @seen;
    i9925-qsort($a, +@values, 4, -> Pointer $x, Pointer $y --> int32 {
        my $l = nativecast(CArray[int32], $x)[0];
        my $r = nativecast(CArray[int32], $y)[0];
        @seen.push: i9925-tag($l);
        $i9925-direction * ($l - $r)
    });
    ((^@values).map({ $a[$_] }).List, so @seen.all.starts-with('<'))
}
