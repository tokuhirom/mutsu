unit class ExportedMethodNested;

grammar Pair2 { token TOP { <a> }; token a { x } }

# An exported method is importable as a sub that forwards to it; the method body
# must run as an ordinary method, not under the sub's container-returning context.
method parse-from($class = $?CLASS: Str:D :$text!) is export(:parse) {
    my $m = Pair2.parse($text);
    $m.from
}
