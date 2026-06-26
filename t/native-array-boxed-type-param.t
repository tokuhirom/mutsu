use Test;

# A native `array[T]` may only be parameterized with a *native* element type
# (int/uint/num/str/...). A boxed type (`array[Int]`) cannot back a native array,
# so the parameterization fails at compile time (X::Comp::BeginTime). The boxed
# `Array[Int]` (capital A) is unaffected.

plan 6;

throws-like 'sub x(array[Int]) { }', X::Comp::BeginTime,
    'array[Int] (boxed) param is X::Comp::BeginTime';
throws-like 'sub x(array[Str] @a) { }', X::Comp::BeginTime,
    'array[Str] (boxed) param is X::Comp::BeginTime';

# Native element types parameterize fine.
lives-ok { EVAL 'sub a(array[int] @x) { }' }, 'array[int] native param lives';
lives-ok { EVAL 'sub b(array[num] @x) { }' }, 'array[num] native param lives';
lives-ok { EVAL 'sub c(array[str] @x) { }' }, 'array[str] native param lives';

# Boxed Array (capital A) with a boxed element is valid.
lives-ok { EVAL 'sub d(Array[Int] @x) { }' }, 'Array[Int] (boxed) param lives';
