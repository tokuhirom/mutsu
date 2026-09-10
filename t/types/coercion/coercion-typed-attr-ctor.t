use Test;

# Construction of coercion-typed scalar attributes (`has Int() $.x`).
# - An uninitialized attribute defaults to the *target* type object (undefined).
# - A provided value is coerced through the target type.
# - An explicit default is coerced too.
# Built-in coercion targets (Int/Num/Rat/Str/Bool/...) are constructed natively;
# a user-class target with a COERCE method falls through to the interpreter but
# must produce the same result.

plan 23;

# --- uninitialized: target type object, undefined ---
{
    class C0 { has Int() $.x }
    my $c = C0.new;
    is $c.x.^name, 'Int', 'uninit coercion attr is the target type object';
    nok $c.x.defined, 'uninit coercion attr is undefined';
}

# --- provided value coerced ---
{
    class C1 { has Int() $.x }
    my $c = C1.new(x => "42");
    is $c.x, 42, 'provided Str coerced to Int';
    is $c.x.^name, 'Int', 'provided value has target type';
}

# --- Num() / Rat() / Bool() targets ---
{
    class C2 { has Num() $.y }
    my $c = C2.new(y => 3);
    is $c.y, 3, 'Int coerced to Num';
    is $c.y.^name, 'Num', 'Num() target type';
}
{
    class C3 { has Rat() $.r }
    my $c = C3.new(r => "1.5");
    is $c.r, 1.5, 'Str coerced to Rat';
    is $c.r.^name, 'Rat', 'Rat() target type';
}
{
    class C4 { has Bool() $.b }
    my $c = C4.new(b => 1);
    is $c.b, True, 'Int coerced to Bool';
    is $c.b.^name, 'Bool', 'Bool() target type';
}

# --- explicit default is coerced ---
{
    class C5 { has Int() $.x = "42" }
    my $c = C5.new;
    is $c.x, 42, 'explicit Str default coerced to Int';
    is $c.x.^name, 'Int', 'explicit default has target type';
}

# --- provided value overrides (and coerces) the default ---
{
    class C6 { has Int() $.x = "42" }
    my $c = C6.new(x => "99");
    is $c.x, 99, 'provided value overrides default';
    is $c.x.^name, 'Int', 'override is coerced';
}

# --- coercion attr coexisting with native / class / untyped attributes ---
{
    class C7 { has int $.n; has Int() $.c; has Str $.s; has @.a }
    my $o = C7.new(c => "7", a => [1, 2]);
    is $o.n, 0,            'native int default still 0';
    is $o.c, 7,            'coercion attr coerced';
    is $o.c.^name, 'Int',  'coercion attr target type';
    is $o.s.^name, 'Str',  'class-typed scalar default is its type object';
    is $o.a, [1, 2],       'untyped array attr provided';
}

# --- user-class coercion target uses its COERCE method ---
{
    class Temp { has $.c; method COERCE($v) { self.new(c => $v) } }
    class W { has Temp() $.t }
    my $w = W.new(t => 5);
    is $w.t.^name, 'Temp', 'user COERCE target produces the class';
    is $w.t.c, 5,          'user COERCE received the value';
}

# --- Numeric() coerces a string through the numeric grammar ---
{
    class C8 { has Numeric() $.n }
    my $c = C8.new(n => "3.5");
    is $c.n, 3.5,         'Numeric() coerces "3.5"';
    is $c.n.^name, 'Rat', 'Numeric() of "3.5" is Rat';
}
