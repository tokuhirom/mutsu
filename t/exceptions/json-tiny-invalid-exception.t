use v6;
use JSON::Tiny;
use Test;

# `use JSON::Tiny` loads the real vendored module (modules/JSON-Tiny/), so its
# `from-json` throws the module's OWN X::JSON::Tiny::Invalid on a parse
# failure. mutsu used to answer this `use` from a native Rust implementation
# and guess the exception shape from which module names had been `use`d
# (#8183); nothing guesses now, because the module's own code runs.
#
# The class is declared inside `unit module JSON::Tiny;`, so its composed name
# is `JSON::Tiny::X::JSON::Tiny::Invalid` -- matching raku.

throws-like { from-json '' }, X::JSON::Tiny::Invalid,
    'empty input throws X::JSON::Tiny::Invalid';

throws-like { from-json 'not json' }, X::JSON::Tiny::Invalid,
    'malformed input throws X::JSON::Tiny::Invalid';

is X::JSON::Tiny::Invalid.^name, 'JSON::Tiny::X::JSON::Tiny::Invalid',
    'the exception is the module-composed class, not a native stand-in';

{
    from-json 'nope';
    CATCH {
        when X::JSON::Tiny::Invalid {
            is .source, 'nope', '.source carries the original text';
            is .message, 'Input (4 characters) is not a valid JSON string',
                '.message matches the upstream format';
        }
    }
}

# Clean input still parses without any exception.
is-deeply from-json('[1, 2]'), [1, 2], 'clean parse unaffected';

done-testing;
