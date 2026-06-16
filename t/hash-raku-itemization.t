use Test;

# In Raku every hash value lives in a Scalar container, so an aggregate
# (Array / List / Hash / Seq) value is itemized in `.raku` / `.perl`:
#   {:a($[1, 2])}, {:a(${:b(1)})}, {:a($(1, 2, 3))}
# Scalars (Int / Str / Range / Pair / Set ...) are rendered as-is.

plan 16;

# Hash value that is itself a Hash
{
    my %h; %h<a><b> = 1;
    is %h.raku, '{:a(${:b(1)})}', 'nested autoviv hash value is itemized';
}
{
    my %h; %h<a><b><c> = 1;
    is %h.raku, '{:a(${:b(${:c(1)})})}', 'deep nested autoviv hash values';
}
{
    my %h; %h{1}{2} = 3;
    is %h.raku, '{"1" => ${"2" => 3}}', 'non-ident key nested hash value';
}
{
    my %h = a => {b => 1};
    is %h.raku, '{:a(${:b(1)})}', 'hash literal value';
}

# Hash value that is an Array / List / Seq
{
    my %h = a => [1, 2, 3];
    is %h.raku, '{:a($[1, 2, 3])}', 'array value is itemized';
}
{
    my %h = a => (1, 2, 3);
    is %h.raku, '{:a($(1, 2, 3))}', 'list value is itemized';
}
{
    my %h = a => (1,);
    is %h.raku, '{:a($(1,))}', 'one-element list value';
}
{
    my %h = a => [1, 2].Seq;
    is %h.raku, '{:a($((1, 2).Seq))}', 'Seq value is itemized';
}
{
    my @a = 1, 2;
    my %h = x => @a;
    is %h.raku, '{:x($[1, 2])}', '@-array value is itemized';
}

# Already-itemized values are not double-itemized
{
    my %h = a => $[1, 2];
    is %h.raku, '{:a($[1, 2])}', 'explicit $[...] value not doubled';
}

# Scalar / non-aggregate values are NOT itemized
{
    my %h = a => 1, b => 2;
    is %h.raku, '{:a(1), :b(2)}', 'Int values unchanged';
}
{
    my %h = a => "x";
    is %h.raku, '{:a("x")}', 'Str value unchanged';
}
{
    my %h = a => (1..3);
    is %h.raku, '{:a(1..3)}', 'Range value unchanged';
}
{
    my %h = a => (1 => 2);
    is %h.raku, '{:a(1 => 2)}', 'Pair value unchanged';
}

# .perl matches .raku
{
    my %h = a => [1, 2];
    is %h.perl, '{:a($[1, 2])}', '.perl matches .raku';
}

# Array elements are NOT itemized (only hash values are)
is [[1, 2], [3, 4]].raku, '[[1, 2], [3, 4]]', 'arrays inside an array are not itemized';
