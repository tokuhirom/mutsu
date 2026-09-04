use Test;

# An `is Array`/`is List` subclass instance answers through its backing
# `__mutsu_array_storage` no matter HOW the call is spelled.
#
# The decision used to live in three places and be absent from a fourth: the
# `CallMethod` opcode delegated by default, `CallMethodMut` delegated through a
# short allowlist, `CallMethodDynamic` probed the native fast path FIRST, and the
# interpreter's own `call_method_with_values` never delegated at all. So the same
# method answered differently depending on the spelling — and `.elems` reached
# through a runtime method name did not merely answer wrongly, it recursed:
# `builtin_elems` is defined as `$x.elems` and `dispatch_elems_method` answers
# `.elems` by calling `builtin_elems`, so an Instance neither could serve looped
# between them until the stack overflowed.

plan 27;

class R is Array {}

# --- 1. the crash: a runtime method name on a subclass instance -------------
{
    my $m = "elems";
    my $v = R.new(1, 2);
    is $v."$m"(), 2, 'a runtime method name reaches the backing storage';
    is R.new(1, 2)."$m"(), 2, '... from a chained receiver too';
}

# --- 2. every spelling of one call agrees ----------------------------------
for <Str gist elems end join keys values sum sort reverse List Bool> -> $name {
    my $chained = R.new(1, 2)."$name"().gist;
    my $v = R.new(1, 2);
    my $var = $v."$name"().gist;
    is $var, $chained, "`\$v.$name` agrees with the chained spelling";
}

# --- 3. ... and matches what the elements answer ----------------------------
{
    my $v = R.new(1, 2);
    is $v.Str, '1 2', 'Str renders the elements';
    is $v.join, '12', 'join joins the elements';
    is $v.end, 1, 'end is the last index';
    is $v.Numeric, 2, 'Numeric is the element count';
    is $v.Int, 2, 'Int is the element count';
}

# --- 4. ... while construction and identity stay with the CLASS -------------
{
    my $v = R.new(1, 2);
    is $v.clone.^name, 'R', 'clone keeps the subclass';
    is-deeply $v.clone.List, (1, 2), 'and its elements';
    is R.new(1, 2).clone.^name, 'R', 'chained clone keeps it too';
    is $v.^name, 'R', '^name is the subclass';
    ok $v.isa(R), 'isa(the subclass)';
}

# --- 5. mutators still mutate the instance, not a copy ----------------------
{
    my $v = R.new(1, 2);
    $v.push(3);
    is $v.elems, 3, 'push through the static spelling grows the instance';
    my $m = "push";
    $v."$m"(4);
    is $v.elems, 4, 'and so does push through a runtime name';
    is $v.^name, 'R', 'the instance is still the subclass afterwards';
}
