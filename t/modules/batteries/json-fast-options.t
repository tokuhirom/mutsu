use v6;
use Test;

# JSON::Fast option parity in the native implementation:
# :immutable, :enums-as-value, $*JSON_NAN_INF_SUPPORT, import-list defaults,
# code-object dispatch (&from-json / .&from-json), uppercase surrogate escapes,
# and Real-instance (Duration) serialization.

{
    use JSON::Fast <immutable !pretty>;

    is to-json((1, 2, 3).List), "[1,2,3]", "import-list !pretty default";
    is to-json((1, 2, 3).List, :pretty), "[\n  1,\n  2,\n  3\n]",
        "named :pretty overrides import default";

    is-deeply from-json("[1,2,3]"), (1, 2, 3).List,
        "import-list immutable default decodes arrays as List";
    is-deeply from-json("[1,2,3]", :!immutable), [1, 2, 3],
        "named :!immutable overrides import default";
}

{
    use JSON::Fast;

    is to-json((1, 2, 3).List), "[\n  1,\n  2,\n  3\n]",
        "plain use restores pretty default";
    is-deeply from-json("[1,2,3]"), [1, 2, 3],
        "plain use restores mutable default";
    is-deeply from-json("[1,2,3]", :immutable), (1, 2, 3).List,
        "named :immutable decodes arrays as List";
    is-deeply from-json('{"a": 1}', :immutable), Map.new((a => 1)),
        "named :immutable decodes objects as Map";

    enum Bloop <Squee Moo Meep>;
    is to-json(Moo), '"Moo"', "enum serializes as short name by default";
    is to-json(Moo, :enums-as-value), '1', "enums-as-value uses the payload";
    enum Blerp (One => "Eins");
    is to-json(One, :enums-as-value), '"Eins"', "string-valued enum payload";

    is to-json(Inf), "null", "Inf is null by default";
    {
        my $*JSON_NAN_INF_SUPPORT = 1;
        is to-json(Inf), "Inf", 'dynamic var enables Inf';
        is to-json(-Inf), "-Inf", 'dynamic var enables -Inf';
        is to-json(NaN), "NaN", 'dynamic var enables NaN';
    }

    is '["x"]'.&from-json[0], "x", 'code-object .&from-json dispatches';
    my &decode = &from-json;
    is decode('[7]')[0], 7, '&from-json is a callable Routine';

    # Astral chars escape as UPPERCASE surrogate pairs (JSON::Fast .base(16)).
    is to-json("\x[1D11E]", :!pretty), '"' ~ 92.chr ~ 'uD834' ~ 92.chr ~ 'uDD1E"',
        'surrogate pair escapes are uppercase';

    is to-json(Duration.new(57), :!pretty), '57e0',
        'Duration serializes as a Num';
}

done-testing;
