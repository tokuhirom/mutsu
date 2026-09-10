use v6;
use Test;

plan 8;

# `given EXPR -> \ex { }` binds the raw value; an object invocant must keep
# its instance identity (URI's `given X::URI::Path::Invalid.new(...) -> \ex`).
class K { has $.v }

given K.new(v => 3) -> \ex {
    is ex.^name, 'K', 'sigilless given param has the instance type';
    is ex.v, 3, 'attribute access works on the bound instance';
    is ex.raku, 'K.new(v => 3)', '.raku renders the instance';
    ok ex === ex, 'repeated reads give the same value';
}

given X::AdHoc.new(payload => 'boom') -> \ex {
    is ex.^name, 'X::AdHoc', 'exception instance binds correctly';
    throws-like { ex.throw }, X::AdHoc, 'ex.throw throws the bound exception';
}

# Non-instance values keep working.
given 42 -> \n { is n, 42, 'Int topic still binds' }
given 'str' -> \s { is s.^name, 'Str', 'Str topic still binds' }
