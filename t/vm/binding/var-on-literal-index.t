use Test;

plan 8;

# `.VAR` chained after a subscript of a *literal* (not a named variable) used to
# hit "Interpreter stack underflow in CallMethod": the compiler routed it to the
# element-variable metadata path, which emitted no value for a literal target, so
# the following chained method underflowed the stack. It must fall through to the
# ordinary method dispatch instead.

lives-ok { [1, 2, 3][1].VAR.^name }, '.VAR.^name on an array-literal subscript does not crash';
lives-ok { (1, 2, 3)[1].VAR.^name }, '.VAR.^name on a list-literal subscript does not crash';
lives-ok { [1, 2, 3][1].VAR.Str }, '.VAR.Str on an array-literal subscript does not crash';

is [1, 2, 3][1].VAR, 2, '.VAR on a literal subscript yields the element value';
is (1, 2, 3)[1].VAR, 2, '.VAR on a list-literal subscript yields the element value';

# a list element is not containerized, so its .VAR is the value's own type
is (1, 2, 3)[1].VAR.^name, 'Int', 'list element .VAR.^name is the value type';

# the named-variable path is unaffected
{
    my @a = 1, 2, 3;
    is @a[1].VAR.^name, 'Scalar', '.VAR.^name on a named array element is Scalar';
}
{
    my %h = a => 1;
    is %h<a>.VAR.^name, 'Scalar', '.VAR.^name on a named hash element is Scalar';
}
