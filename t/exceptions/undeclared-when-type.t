use Test;

plan 6;

# `when SomeUndeclaredType` where the type is undeclared is a compile-time
# X::Undeclared (an X::Comp subtype) whose message names the type.
throws-like 'given 42 { when SomeUndeclaredType { 1 }; default { 0 } }',
    X::Comp::Group, 'undeclared type in a when clause',
    :message(/SomeUndeclaredType/);

# Valid `when` forms still work.
{
    my $r;
    given 5 { when Int { $r = 'int' }; default { $r = 'no' } }
    is $r, 'int', 'when against a built-in type';
}
{
    class Foo { }
    my $r;
    given Foo.new { when Foo { $r = 'foo' } }
    is $r, 'foo', 'when against a declared class';
}
{
    my $r;
    given 5 { when 5 { $r = 'five' }; when * > 3 { $r = 'big' } }
    is $r, 'five', 'when against a literal';
}
{
    my $r;
    given 'ab' { when /a/ { $r = 'match' } }
    is $r, 'match', 'when against a regex';
}
{
    lives-ok { EVAL 'given 1 { when Int { } }' }, 'EVAL of a valid when lives';
}
