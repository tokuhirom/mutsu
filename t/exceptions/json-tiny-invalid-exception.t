use v6;
use JSON::Tiny;
use Test;

# The real JSON::Tiny's `from-json` throws X::JSON::Tiny::Invalid (carrying
# the original source string) on a parse failure, instead of JSON::Fast's
# plain X::AdHoc `die`. mutsu's native from-json backs both modules, so it
# must pick the exception shape based on which module was `use`d
# (see t/json-additional-content.t for the JSON::Fast side).

throws-like { from-json '' }, X::JSON::Tiny::Invalid,
    'empty input throws X::JSON::Tiny::Invalid';

throws-like { from-json 'not json' }, X::JSON::Tiny::Invalid,
    'malformed input throws X::JSON::Tiny::Invalid';

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
