[![Build Status](https://travis-ci.org/timo/json_fast.svg?branch=master)](https://travis-ci.org/timo/json_fast)

JSON::Fast
==========

A naive imperative JSON parser in pure Raku (but with direct access to `nqp::` ops), to evaluate performance against `JSON::Tiny`. It is a drop-in replacement for `JSON::Tiny`’s from-json and to-json subs, but it offers a few extra features.

Currently it seems to be about 4x faster and uses up about a quarter of the RAM JSON::Tiny would use.

This module also includes a very fast to-json function that tony-o created and lizmat later completely refactored.

SYNOPSIS
--------

        use JSON::Fast;
        my $storage-path = $*SPEC.tmpdir.child("json-fast-example-$*PID.json");
        say "using path $storage-path for example";
        for <recreatable monascidian spectrograph bardiest ayins sufi lavanga Dachia> -> $word {
            say "- loading json file";
            my $current-data = from-json ($storage-path.IO.slurp // "\{}");
            # $current-data now contains a Hash object populated with what was in the file
            # (or an empty hash in the very first step when the file didn't exsit yet)

            say "- adding entry for $word";
            $current-data{$word}{"length"} = $word.chars;
            $current-data{$word}{"first letter"} = $word.substr(0,1);

            say "- saving json file";
            $storage-path.IO.spurt(to-json $current-data);
            # to-json gives us a regular string, so we can plop that
            # into the file with the spurt method

            say "json file is now $storage-path.IO.s() bytes big";
            say "===";
        }
        say "here is the entire contents of the json file:";
        say "====";
        say $storage-path.IO.slurp();
        say "====";
        say "deleting storage file ...";
        $storage-path.IO.unlink;

Exported subroutines
--------------------

### to-json

        my $*JSON_NAN_INF_SUPPORT = 1; # allow NaN, Inf, and -Inf to be serialized.
        say to-json [<my Raku data structure>];
        say to-json [<my Raku data structure>], :!pretty;
        say to-json [<my Raku data structure>], :spacing(4);

        enum Blerp <Hello Goodbye>;
        say to-json [Hello, Goodbye]; # ["Hello", "Goodbye"]
        say to-json [Hello, Goodbye], :enums-as-value; # [0, 1]

Encode a Raku data structure into JSON. Takes one positional argument, which is a thing you want to encode into JSON. Takes these optional named arguments:

#### pretty

`Bool`. Defaults to `True`. Specifies whether the output should be "pretty", human-readable JSON. When set to `False`, will output json in a single line.

#### spacing

`Int`. Defaults to `2`. Applies only when `pretty` is `True`. Controls how much spacing there is between each nested level of the output.

#### sorted-keys

Specifies whether keys from objects should be sorted before serializing them to a string or if `$obj.keys` is good enough. Defaults to `False`. Can also be specified as a `Callable` with the same type of argument that the `.sort` method accepts to provide alternate sorting methods.

#### enum-as-value

`Bool`, defaults to `False`. Specifies whether `enum`s should be json-ified as their underlying values, instead of as the name of the `enum`.

### from-json

        my $x = from-json '["foo", "bar", {"ber": "bor"}]';
        say $x.raku;
        # outputs: $["foo", "bar", {:ber("bor")}]

Takes one positional argument that is coerced into a `Str` type and represents a JSON text to decode. Returns a Raku datastructure representing that JSON.

#### immutable

`Bool`. Defaults to `False`. Specifies whether `Hash`es and `Array`s should be rendered as immutable datastructures instead (as `Map` / `List`. Creating an immutable data structures is mostly saving on memory usage, and a little bit on CPU (typically around 5%).

This also has the side effect that elements from the returned structure can now be iterated over directly because they are not containerized.

        my %hash := from-json "META6.json".IO.slurp, :immutable;
        say "Provides:";
        .say for %hash<provides>;

#### allow-jsonc

`Bool`. Defaults to `False`. Specifies whether commmands adhering to the [JSONC standard](https://changelog.com/news/jsonc-is-a-superset-of-json-which-supports-comments-6LwR) are allowed.

Additional features
-------------------

### Adapting defaults of "from-json"

In the `use` statement, you can add the string `"immutable"` to make the default of the `immutable` parameter to the `from-json` subroutine `True`, rather than `False`.

        use JSON::Fast <immutable>;  # create immutable data structures by default

### Adapting defaults of "to-json"

In the `use` statement, you can add the strings `"!pretty"`, `"sorted-keys"` and/or `"enums-as-value"` to change the associated defaults of the `to-json` subroutine.

        use JSON::FAST <!pretty sorted-keys enums-as-value>;

### Strings containing multiple json pieces

When the document contains additional non-whitespace after the first successfully parsed JSON object, JSON::Fast will throw the exception `X::JSON::AdditionalContent`. If you expect multiple objects, you can catch that exception, retrieve the parse result from its `parsed` attribute, and remove the first `rest-position` characters off of the string and restart parsing from there.

