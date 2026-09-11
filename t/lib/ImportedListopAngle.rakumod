# A module in the `String::Utils` shape, exported through the ordinary
# `is export` trait (the common case) rather than a run-time `sub EXPORT`
# hook (pinned separately by `runtime-export-listop-parse.t`).
#
# `root` is String::Utils' own routine: the longest common prefix of its
# arguments. The point of the fixture is the *call shape* the importer uses,
# `root <abcd abce abde>` -- which only parses as a call if the parser
# already knows the imported name is a routine.

unit module ImportedListopAngle;

sub root(*@strings) is export {
    return "" unless @strings;
    my $first = @strings[0];
    my $len = $first.chars;
    for @strings -> $s {
        my $i = 0;
        $i++ while $i < $len && $i < $s.chars
                   && $s.substr($i, 1) eq $first.substr($i, 1);
        $len = $i;
    }
    $first.substr(0, $len)
}

sub tally(*@words) is export { @words.elems }

sub joined(*@words) is export(:extra) { @words.join("-") }
