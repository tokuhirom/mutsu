use Test;

plan 6;

# A `constant` whose initializer references an undeclared bareword is
# X::Undeclared::Symbols.
throws-like 'constant foo = bar', X::Undeclared::Symbols,
    'constant initialized from an undeclared name';

# Valid constant initializers still work.
{
    constant x = 42;
    is x, 42, 'literal constant';
}
{
    constant z = 1 + 2 * 3;
    is z, 7, 'expression constant';
}
{
    constant w = "hello";
    is w, 'hello', 'string constant';
}
{
    sub g() { 5 }
    constant y = g();
    is y, 5, 'constant from a declared sub call';
}
{
    constant p = pi;
    ok p > 3 && p < 4, 'constant from a built-in term';
}
