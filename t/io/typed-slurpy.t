use Test;

# X::Parameter::TypedSlurpy: a slurpy parameter may not carry an explicit type
# constraint (even Mu/Any). See roast/S06-signature/slurpy-params.t lines 267-273.
#
# NOTE: only the single-star forms (`Type *@`, `Type *%`) are exercised here.
# mutsu's parser cannot yet parse a type constraint before a `**`/`+` slurpy
# (`Type **@a`, `Type +@a` fail to parse outright -- a separate pre-existing
# limitation), so those never reach the validation below. The check in
# parser/stmt/sub.rs already covers `**`/`+` for when the parser is fixed.

plan 10;

# positional single-star slurpy
throws-like 'sub f(Int *@a) { }', X::Parameter::TypedSlurpy, kind => 'positional',
    '*@ with type constraint';
throws-like 'sub f(Mu *@a) { }', X::Parameter::TypedSlurpy, kind => 'positional',
    'even Mu is rejected on a slurpy';
throws-like 'sub f(Cool *@a) { }', X::Parameter::TypedSlurpy, kind => 'positional',
    'Cool *@ with type constraint';

# named single-star slurpy
throws-like 'sub f(Int *%h) { }', X::Parameter::TypedSlurpy, kind => 'named',
    '*% with type constraint';

# untyped slurpies are fine
lives-ok { EVAL 'sub f(*@a) { }' }, 'untyped *@ ok';
lives-ok { EVAL 'sub f(*%h) { }' }, 'untyped *% ok';
lives-ok { EVAL 'sub f(**@a) { }' }, 'untyped **@ ok';
lives-ok { EVAL 'sub f(+@a) { }' }, 'untyped +@ ok';

# a type constraint on a non-slurpy positional is fine
lives-ok { EVAL 'sub f(Int $x, *@a) { }' }, 'typed required + untyped slurpy ok';

# implicit slurpy placeholders are not typed
{
    sub uses-args { @_ }
    is uses-args(1, 2, 3).elems, 3, 'implicit @_ slurpy is not rejected';
}
