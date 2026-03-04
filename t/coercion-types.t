use Test;

plan 14;

# Int() coercion type on parameters
{
    sub to-int(Int() $x) { $x }
    is to-int("42"), 42, 'Int() coerces "42" to 42';
    is to-int("42").WHAT.gist, '(Int)', 'Int() result is Int type';
    is to-int(42), 42, 'Int() passes through Int value';
}

# Str() coercion type on parameters
{
    sub to-str(Str() $x) { $x }
    is to-str(42), "42", 'Str() coerces 42 to "42"';
    is to-str(42).WHAT.gist, '(Str)', 'Str() result is Str type';
    is to-str("hello"), "hello", 'Str() passes through Str value';
}

# Num() coercion type on parameters
{
    sub to-num(Num() $x) { $x }
    is to-num(42).WHAT.gist, '(Num)', 'Num() coerces Int to Num';
    is to-num("3.14").WHAT.gist, '(Num)', 'Num() coerces Str to Num';
}

# Int(Str) coercion type - accepts only Str, coerces to Int
{
    sub int-from-str(Int(Str) $x) { $x }
    is int-from-str("42"), 42, 'Int(Str) coerces "42" to 42';
    is int-from-str("42").WHAT.gist, '(Int)', 'Int(Str) result is Int type';
}

# Coercion type on variable declarations
{
    my Int() $x = "42";
    is $x, 42, 'Int() on my variable coerces "42" to 42';
    is $x.WHAT.gist, '(Int)', 'Int() on my variable produces Int type';
}

# Type smiley constraints on parameters
{
    sub defined-int(Int:D $x) { $x }
    is defined-int(42), 42, 'Int:D accepts defined Int';

    sub undef-int(Int:U $x) { $x.gist }
    is undef-int(Int), '(Int)', 'Int:U accepts type object';
}
