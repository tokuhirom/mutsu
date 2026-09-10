use v6;
use JSON::Fast;
use Test;

# from-json with trailing content must throw X::JSON::AdditionalContent
# carrying .parsed / .parsed-length / .rest-position, so multi-document
# consumers can resume from the rest position (JSON::Fast t/10-multidocument.t).

my $input = q«[1, 2, 3]{"a": 99, "b": 123} "foo"»;
my @results;
my $rounds = 0;

loop {
    $rounds++;
    last if $rounds > 100;

    @results.push: from-json($input);

    CATCH {
        when X::JSON::AdditionalContent {
            @results.push: .parsed;
            $input = $input.substr(.rest-position)
        }
    }
    last
};

is $rounds, 3, "right number of parses";
is-deeply @results[0], $[1, 2, 3], "first result";
is-deeply @results[1], ${"a" => 99, "b" => 123}, "second result";
is-deeply @results[2], "foo", "third result";

# Attribute details: parsed-length counts chars before trailing whitespace,
# rest-position is the index of the next non-whitespace character.
{
    from-json(q«[1]   42»);
    CATCH {
        when X::JSON::AdditionalContent {
            is .parsed-length, 3, "parsed-length is chars parsed";
            is .rest-position, 6, "rest-position skips whitespace";
        }
    }
}

# Clean input still parses without any exception.
is-deeply from-json("[4, 5]"), $[4, 5], "clean parse unaffected";

done-testing;
