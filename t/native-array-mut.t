use Test;

# Native VM dispatch for mutating list methods on plain untyped @-arrays
# (ledger §1: native receiver dispatch lowered into the VM). These must stay
# behavior-identical to the interpreter path they replace.

plan 31;

# --- append ---
{
    my @a = 1, 2, 3;
    my $r = @a.append(4, 5);
    is @a.raku, '[1, 2, 3, 4, 5]', 'append multiple scalars';
    is $r.raku, '[1, 2, 3, 4, 5]', 'append returns the array';
}
{
    # one-arg rule: a single list argument is flattened
    my @a = 1, 2;
    @a.append((3, 4, 5));
    is @a.raku, '[1, 2, 3, 4, 5]', 'append one-arg list flattening';
}
{
    # multiple args: each appended as-is, no recursive flatten
    my @a = 1;
    @a.append(2, (3, 4));
    is @a.raku, '[1, 2, (3, 4)]', 'append multi-arg no recursive flatten';
}

# --- prepend ---
{
    my @a = 5, 6;
    my $r = @a.prepend((1, 2));
    is @a.raku, '[1, 2, 5, 6]', 'prepend flattening';
    is $r.raku, '[1, 2, 5, 6]', 'prepend returns the array';
}

# --- unshift ---
{
    my @a = 3;
    my $r = @a.unshift(1, 2);
    is @a.raku, '[1, 2, 3]', 'unshift multiple scalars';
    is $r.raku, '[1, 2, 3]', 'unshift returns the array';
}

# --- pop ---
{
    my @a = 1, 2, 3;
    my $x = @a.pop;
    is $x, 3, 'pop returns last element';
    is @a.raku, '[1, 2]', 'pop mutates the array';
}
{
    my @a;
    my $x = @a.pop;
    nok $x.defined, 'pop on empty array returns undefined (Failure)';
    ok $x ~~ Failure, 'pop on empty array returns a Failure';
}

# --- shift ---
{
    my @a = 1, 2, 3;
    my $x = @a.shift;
    is $x, 1, 'shift returns first element';
    is @a.raku, '[2, 3]', 'shift mutates the array';
}
{
    my @a;
    my $x = @a.shift;
    nok $x.defined, 'shift on empty array returns undefined (Failure)';
    ok $x ~~ Failure, 'shift on empty array returns a Failure';
}

# --- interleaved sequence (hot-loop shape) ---
{
    my @q;
    @q.append($_) for 1..5;
    is @q.raku, '[1, 2, 3, 4, 5]', 'repeated append builds the array';
    my @out;
    @out.push(@q.shift) while @q;
    is @out.raku, '[1, 2, 3, 4, 5]', 'repeated shift drains in order';
    is @q.elems, 0, 'array empty after draining';
}

# --- [...] literal array (also ArrayKind::Array) ---
{
    my @a = [10, 20];
    @a.append(30);
    is @a.raku, '[10, 20, 30]', 'append on [...]-initialised array';
    my $x = @a.pop;
    is $x, 30, 'pop on [...]-initialised array';
}

# --- typed array falls through to interpreter (typed empty Failure) ---
{
    my Int @t = 1, 2, 3;
    @t.append(4);
    is @t.elems, 4, 'typed array append still works (interpreter path)';
    my Int @u;
    my $x = @u.pop;
    ok $x ~~ Failure, 'typed empty pop is a Failure';
}

# --- pop/shift do not lose other elements; return value decont ---
{
    my @a = 'a', 'b', 'c';
    is @a.pop, 'c', 'string pop value';
    is @a.shift, 'a', 'string shift value';
    is @a.raku, '["b"]', 'middle element retained';
}

# --- autoviv: append on undeclared-value array ---
{
    my @a;
    @a.append(1);
    @a.prepend(0);
    is @a.raku, '[0, 1]', 'append+prepend from empty';
}

# --- elems / boolification after mutation ---
{
    my @a = 1, 2;
    @a.append(3);
    is @a.elems, 3, 'elems after append';
    @a.shift;
    @a.shift;
    is @a.elems, 1, 'elems after shifts';
    ok @a.Bool, 'truthy with one element';
    @a.pop;
    nok @a.Bool, 'falsy when empty';
}
