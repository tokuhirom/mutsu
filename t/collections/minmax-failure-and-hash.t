use Test;

plan 6;

is (1,2 minmax 9,8), 1..9, "minmax binds lower than comma lists";

fails-like "min +'a'      ", X::Str::Numeric, "min with one Failure sinks as X::Str::Numeric";
fails-like "max +'a'      ", X::Str::Numeric, "max with one Failure sinks as X::Str::Numeric";

is-deeply max({ :2b, :1c, :3a }), "c" => 1, "&max() on hash returns key-based maximum pair";
is-deeply min({ :2b, :1c, :3a }), "a" => 3, "&min() on hash returns key-based minimum pair";

my &by = *.value;
is-deeply max({ :2b, :1c, :3a }, :&by), "a" => 3, "&max(:by(*.value)) on hash uses value comparator";
