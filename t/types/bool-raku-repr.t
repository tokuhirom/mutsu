use Test;

# Bool renders as `Bool::True` / `Bool::False` in `.raku` / `.perl`,
# both at top level and nested inside arrays and hashes. Hash values are
# rendered in the colon-pair form `:key(Bool::True)`, not the adverbial
# `:key` / `:!key` form.

plan 16;

is True.raku,  'Bool::True',  'True.raku';
is False.raku, 'Bool::False', 'False.raku';
is True.perl,  'Bool::True',  'True.perl';

is [True, False].raku, '[Bool::True, Bool::False]', 'Bool inside array';
is (True, False).raku, '(Bool::True, Bool::False)', 'Bool inside list';

{
    my %h = a => True;
    is %h.raku, '{:a(Bool::True)}', 'True hash value uses :a(Bool::True)';
}
{
    my %h = a => False;
    is %h.raku, '{:a(Bool::False)}', 'False hash value uses :a(Bool::False)';
}
{
    my %h = a => True, b => 2;
    is %h.raku, '{:a(Bool::True), :b(2)}', 'mixed Bool / Int hash values';
}
{
    my %h = a => True;
    is %h.perl, '{:a(Bool::True)}', '.perl matches .raku for Bool value';
}
{
    my %h = :a, :!b;
    is %h.raku, '{:a(Bool::True), :b(Bool::False)}', 'adverbial pairs render expanded';
}

# Non-ident key with a Bool value
{
    my %h = "1" => True;
    is %h.raku, '{"1" => Bool::True}', 'non-ident key Bool value';
}

# Bool inside a nested (itemized) hash value
{
    my %h; %h<a><b> = True;
    is %h.raku, '{:a(${:b(Bool::True)})}', 'Bool in nested hash value';
}

# A standalone Pair with a Bool value uses the adverbial form (unlike a hash
# value, which expands to :key(Bool::True)).
is (:foo).raku,         ':foo',           'Pair with True is :foo';
is (:!bar).raku,        ':!bar',          'Pair with False is :!bar';
is (foo => True).raku,  ':foo',           'fat-arrow Pair True';
is (bar => False).raku, ':!bar',          'fat-arrow Pair False';
