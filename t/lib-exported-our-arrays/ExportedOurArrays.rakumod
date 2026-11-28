unit module ExportedOurArrays;

# Uppercase-initial `our` aggregates, the shape Math::Sequences exports by the
# hundred (#19324).
our @Squares is export = (1..*).map: { $_ * $_ };
our @Fib is export = 0, 1, * + * ... *;
our %Memo is export;

# A module sub whose closures read the module's own arrays by their bare name.
sub square-sum($n) is export {
    (^$n).map({ @Squares[$_] }).sum
}

# A gather in a module sub reading and writing module state through closures.
sub fib-pairs($n) is export {
    gather for ^$n -> $i {
        %Memo{$i} //= @Fib[$i];
        take $i => %Memo{$i};
    }
}

# The package-qualified spelling, from inside a closure.
sub qualified-first() is export {
    (1,).map({ @ExportedOurArrays::Squares[0] }).head
}
