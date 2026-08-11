use Test;

plan 4;

# `$obj[$i] = v` on a user Positional class with no ASSIGN-POS declared must
# fall back to AT-POS as a writable container when AT-POS's body returns an
# indexed attribute element (`@!attr[$i]` / `return-rw @!attr[$i]`).

class ViaIsRw does Positional {
    has @.arr;
    method AT-POS($i) is rw { @!arr[$i] }
}

my $a = ViaIsRw.new;
$a[0] = 5;
is $a[0], 5, 'is rw AT-POS: write then read back';
is $a.raku, 'ViaIsRw.new(arr => [5])', 'is rw AT-POS: .raku reflects the write';

class ViaReturnRw does Positional {
    has @.arr;
    method AT-POS($i) {
        return-rw @!arr[$i];
    }
}

my $b = ViaReturnRw.new;
$b.arr[0] = -1;
$b[1] = 0;
is $b[1], 0, 'return-rw AT-POS (no is rw): write then read back';
is $b.raku, 'ViaReturnRw.new(arr => [-1, 0])', 'return-rw AT-POS: array extends past its previous length';
