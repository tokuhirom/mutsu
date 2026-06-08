use Test;

plan 4;

# The default (Mu) constructor only accepts named arguments; positional
# arguments throw X::Constructor::Positional carrying the offending type.
class Foo { };
throws-like 'Mu.new(1)', X::Constructor::Positional, type => Mu,
    'Mu.new(1) throws X::Constructor::Positional';
throws-like 'class Foo { }; Foo.new(1, 2, 3)', X::Constructor::Positional, type => Foo,
    'Foo.new(positionals) throws X::Constructor::Positional';

# Valid named construction and positional-accepting built-ins are unaffected.
{
    my $f = Foo.new;
    ok $f.defined, 'Foo.new (no args) still works';
}
{
    my @a = Array.new(1, 2, 3);
    is @a.elems, 3, 'Array.new(positionals) still works';
}
