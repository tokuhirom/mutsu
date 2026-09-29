use Test;

plan 6;

sub outer(::T $x) {
    my sub inner(T $y) { $y.^name }
    inner($x)
}
is outer(42), 'Int', 'nested sub signature sees the outer type capture';
is outer('hello'), 'Str', 'the capture resolves separately for each call';

sub outer-multi(::T $x) {
    my multi sub inner(T $y) { $y.^name }
    inner($x)
}
is outer-multi(42), 'Int', 'a nested multi signature sees the outer capture';

sub outer-deep(::U $x) {
    my sub middle {
        my sub inner(U $y) { $y.^name }
        inner($x)
    }
    middle()
}
is outer-deep('hello'), 'Str', 'the capture crosses another nested sub';

sub outer-return(::R $x) {
    my sub inner(R $y --> R) { $y }
    inner($x)
}
is outer-return(42), 42, 'a nested return type sees the outer capture';

throws-like { EVAL 'sub unrelated(T $x) { $x }' }, X::Parameter::InvalidType,
    'the capture is not visible in an unrelated signature';
