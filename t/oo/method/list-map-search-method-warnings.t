use v6;
use Test;

# Rakudo warns when List/Array use Cool's string-search candidates and when
# Map/Hash use Map's contains/index methods. Each warning resumes with the same
# result the stringified search would otherwise have returned.

plan 26;

sub warned-result(&code) {
    my $message = '';
    my $result;
    {
        CONTROL { when CX::Warn { $message = .message; .resume } }
        $result = code();
    }
    ($message, $result)
}

my ($message, $result) = warned-result { (1, 2, 3).contains(2) };
is $message, q[Calling '.contains' on a List, did you mean '$item (elem) @list'?],
    'List.contains warns with the elem suggestion';
is $result, True, 'List.contains keeps its stringified-search result';

($message, $result) = warned-result { [1, 2, 3].contains(2) };
is $message, q[Calling '.contains' on a Array, did you mean '$item (elem) @list'?],
    'Array.contains identifies the concrete receiver';
is $result, True, 'Array.contains keeps its stringified-search result';

($message, $result) = warned-result { (1, 2, 3).index(2) };
is $message, q[Calling '.index' on a List, did you mean '.first( ..., :k)'?],
    'List.index warns with the first(:k) suggestion';
is $result, 2, 'List.index keeps its stringified-search result';

($message, $result) = warned-result { [1, 2, 3].index(2) };
is $message, q[Calling '.index' on a Array, did you mean '.first( ..., :k)'?],
    'Array.index identifies the concrete receiver';
is $result, 2, 'Array.index keeps its stringified-search result';

($message, $result) = warned-result { (1, 2, 3).rindex(2) };
is $message, q[Calling '.rindex' on a List, did you mean '.first( ..., :k, :end)'?],
    'List.rindex warns with the first(:k, :end) suggestion';
is $result, 2, 'List.rindex keeps its stringified-search result';

($message, $result) = warned-result { [1, 2, 3].rindex(2) };
is $message, q[Calling '.rindex' on a Array, did you mean '.first( ..., :k, :end)'?],
    'Array.rindex identifies the concrete receiver';
is $result, 2, 'Array.rindex keeps its stringified-search result';

($message, $result) = warned-result { Map.new(a => 1).contains('a') };
is $message,
    q[Applying '.contains' to a Map will look at its .Str representation. Did] ~
        "\n" ~ q[you mean 'Map{needle}:exists'?],
    'Map.contains warns with the Map lookup suggestion';
is $result, True, 'Map.contains keeps its stringified-search result';

($message, $result) = warned-result { %(a => 1).contains('a') };
is $message,
    q[Applying '.contains' to a Hash will look at its .Str representation. Did] ~
        "\n" ~ q[you mean 'Hash{needle}:exists'?],
    'Hash.contains warns with the Hash lookup suggestion';
is $result, True, 'Hash.contains keeps its stringified-search result';

($message, $result) = warned-result { Map.new(a => 1).index('a') };
is $message,
    q[Applying '.index' to a Map will look at its .Str representation. Did] ~
        "\n" ~ q[you mean 'Map{needle}:exists'?],
    'Map.index warns with the Map lookup suggestion';
is $result, 0, 'Map.index keeps its stringified-search result';

($message, $result) = warned-result { %(a => 1).index('a') };
is $message,
    q[Applying '.index' to a Hash will look at its .Str representation. Did] ~
        "\n" ~ q[you mean 'Hash{needle}:exists'?],
    'Hash.index warns with the Hash lookup suggestion';
is $result, 0, 'Hash.index keeps its stringified-search result';

($message, $result) = warned-result { Map.new(a => 1).rindex('a') };
is $message, '', 'Map.rindex keeps the un-worried Cool behavior';
is $result, 0, 'Map.rindex keeps its stringified-search result';

($message, $result) = warned-result { %(a => 1).rindex('a') };
is $message, '', 'Hash.rindex keeps the un-worried Cool behavior';
is $result, 0, 'Hash.rindex keeps its stringified-search result';

($message, $result) = warned-result { 'abc'.contains('b') };
is $message, '', 'Str.contains is unaffected';
is $result, True, 'Str.contains keeps its ordinary result';
