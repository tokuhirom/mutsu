unit module EvalImportedClosure;

sub eval-imported-double(Int $value --> Int) is export {
    $value * 2
}
