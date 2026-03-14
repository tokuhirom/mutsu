use Test;

plan 6;

# samewith in multi subs
{
    multi foo(Int $x) { samewith($x.Str) }
    multi foo(Str $x) { $x }
    is foo(42), "42", 'samewith dispatches to Str candidate from Int candidate';
}

# samewith in multi methods
{
    class C {
        multi method m(Int $x) { samewith($x.Str) }
        multi method m(Str $x) { $x }
    }
    is C.new.m(10), "10", 'samewith works in multi methods on instance';
    is C.m(10), "10", 'samewith works in multi methods on type object';
}

# samewith as recursive factorial
{
    multi fact($n) {
        $n <= 1 ?? 1 !! $n * samewith($n - 1)
    }
    is fact(5), 120, 'samewith recursive factorial';
}

# samewith from nested closure
{
    multi bar($n) {
        { $n ?? $n * samewith($n - 1) !! 1 }()
    }
    is bar(4), 24, 'samewith from nested closure';
}

# { ... }() as last statement returns its value
{
    sub baz($n) { { $n * 2 }() }
    is baz(5), 10, 'closure call as last statement returns value';
}
