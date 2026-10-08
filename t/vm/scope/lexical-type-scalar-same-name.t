use Test;

plan 6;

# A lexical type and a same-named scalar are different symbols (#12109).
{
    my class foo { has $.x = 16 }
    my $foo = 5;
    is EVAL('foo.^name'), 'foo', 'EVAL sees the lexical type, not the scalar';
    is EVAL('foo.new.x'), 16, 'EVAL can instantiate the lexical type';
    is $foo, 5, 'the scalar keeps its value';
    is foo.new.x, 16, 'compiled bare word still names the type';
}

{
    my role bar { method m { 'role' } }
    my $bar = 7;
    is EVAL('bar.^name'), 'bar', 'EVAL sees a lexical role';
    is $bar, 7, 'scalar next to a lexical role keeps its value';
}
