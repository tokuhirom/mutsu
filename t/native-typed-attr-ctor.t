use Test;

# Native default construction for native-typed scalar attributes
# (`has int $.x` / `has num $.y` / `has str $.z`). These default to a native
# zero (0 / 0e0 / "") rather than a type object, and a provided value is
# stored as-is (the constructor neither coerces nor type-checks it).

plan 18;

# --- defaults ---
{
    class C0 { has int $.x; has num $.y; has str $.z }
    my $c = C0.new;
    is $c.x, 0,       'int default is 0';
    is $c.x.^name, 'Int', 'int default name is Int';
    is $c.y, 0,       'num default is 0';
    is $c.y.^name, 'Num', 'num default name is Num';
    is $c.z, '',      'str default is empty string';
    is $c.z.^name, 'Str', 'str default name is Str';
}

# --- provided values stored as-is ---
{
    class C1 { has int $.x; has str $.z }
    my $c = C1.new(x => 5, z => 'hi');
    is $c.x, 5,    'provided int kept';
    is $c.z, 'hi', 'provided str kept';
}

# --- sized native ints default to 0 ---
{
    class C2 { has int8 $.a; has uint16 $.b; has int64 $.c }
    my $c = C2.new;
    is $c.a, 0, 'int8 default 0';
    is $c.b, 0, 'uint16 default 0';
    is $c.c, 0, 'int64 default 0';
}

# --- native scalar coexisting with a class-typed scalar ---
{
    class C3 { has int $.n; has Int $.i }
    my $c = C3.new;
    is $c.n, 0, 'native int defaults to 0';
    is $c.i.^name, 'Int', 'class-typed scalar defaults to its type object';
    nok $c.i.defined, 'class-typed scalar default is undefined';
}

# --- native scalar with an explicit default expression ---
{
    class C4 { has int $.a = 9; has num $.c }
    my $c = C4.new;
    is $c.a, 9, 'native int explicit default honoured';
    is $c.c, 0, 'native num default still 0';
}

# --- inheritance with native-typed attributes ---
{
    class A5 { has int $.x }
    class B5 is A5 { has str $.y }
    my $b = B5.new(x => 3);
    is $b.x, 3,  'inherited native int provided value kept';
    is $b.y, '', 'native str default in subclass';
}
