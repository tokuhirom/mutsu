unit module ModuleSubUserType;

class Result is export {
    has Str:D $.value is required;
}

sub make-result(Str:D $value --> Result:D) is export {
    Result.new(:$value)
}

sub result-value(Result:D $result --> Str:D) is export {
    $result.value
}
