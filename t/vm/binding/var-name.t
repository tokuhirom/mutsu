use Test;

plan 15;

# .VAR.name on scalar variables
{
    my $a;
    is $a.VAR.name, '$a', '.VAR.name on uninitialized scalar';
    $a = 42;
    is $a.VAR.name, '$a', '.VAR.name on initialized scalar';
}

# .VAR.name on array variables
{
    my @b;
    is @b.VAR.name, '@b', '.VAR.name on uninitialized array';
    @b.push(1);
    is @b.VAR.name, '@b', '.VAR.name on initialized array';
}

# .VAR.name on hash variables
{
    my %c;
    is %c.VAR.name, '%c', '.VAR.name on uninitialized hash';
    %c<x> = 1;
    is %c.VAR.name, '%c', '.VAR.name on initialized hash';
}

# .VAR.name on code variables
{
    my &d;
    is &d.VAR.name, '&d', '.VAR.name on uninitialized code var';
    is &d.VAR.^name, 'Sub', '.VAR.^name on uninitialized code var';
    &d = -> { 99 };
    is &d.VAR.name, '&d', '.VAR.name on initialized code var';
    is &d.VAR.^name, 'Sub', '.VAR.^name on initialized code var';
}

# .VAR on non-container is identity
{
    my $x = 42;
    is $x.VAR.VAR.name, '$x', '.VAR.VAR.name chains correctly';
}

# Method dispatch modifiers
{
    my $a = 1;
    is $a.VAR.?name, '$a', '.?name modifier returns value on success';
    is-deeply $a.VAR.+name, ('$a',), '.+name modifier wraps in list';
    is-deeply $a.VAR.*name, ('$a',), '.*name modifier wraps in list';
}

# .?method returns Nil on missing method
{
    my $x = 42;
    is $x.?nonexistent, Nil, '.?method returns Nil for missing method';
}
