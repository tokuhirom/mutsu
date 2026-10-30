use Test;

# IO::Glob (Archive::SimpleZip's dependency) writes `token match:sym<%> {
# <sym> { make rx/.*?/ } }`. The scan for the end of a regex's `{ ... }` code
# block took the first `/` of `rx/.*?/` for a division and the closing `/`
# (after the `?`) for the opener of a regex literal, so it never found the
# block's `}` and the whole declaration lost its source tree.

plan 4;

grammar Globs {
    token TOP { <term>+ }
    proto token term {*}
    token term:sym<%> { <sym> { make rx/.*?/ } }
    token term:sym<_> { <sym> { make rx/./ } }
    token term:sym<alt> { 'p' { make rx/a|b?/ } }
}

my $m = Globs.parse('%_');
ok $m, 'a grammar whose code blocks hold rx// literals ending in an operator parses';
is $m<term>[0].made.gist, 'rx/.*?/', 'rx/.*?/ is the made value of the first term';
is $m<term>[1].made.gist, 'rx/./', 'rx/./ is the made value of the second term';
is Globs.parse('p')<term>[0].made.gist, 'rx/a|b?/', 'rx/a|b?/ in a code block';
