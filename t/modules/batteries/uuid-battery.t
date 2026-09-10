use Test;

# UUID is bundled (BATTERIES.md §7, docs/batteries/uuid.md), so it must load
# and work with a plain `use` -- no `-I`, no install. This pins the
# zero-config resolution and a smoke slice of the API; the exhaustive
# behaviour check is the release-time gate that runs the full upstream suite
# (scripts/battery-testsuite.sh).

plan 6;

use UUID;

my $u = UUID.new(:version(4));
ok $u, 'UUID.new(:version(4)) constructs';
is $u.version, 4, '.version reports 4';
isa-ok $u.Blob, Buf, '.Blob returns a Buf';
is $u.Blob.elems, 16, 'a UUID is 16 bytes';
ok $u.Str ~~ /^ <[0..9a..f]> ** 8 '-' <[0..9a..f]> ** 4 '-' <[0..9a..f]> ** 4 '-'
    <[0..9a..f]> ** 4 '-' <[0..9a..f]> ** 12 $/,
    '.Str formats as 8-4-4-4-12 hex groups';

my $v = UUID.new(:version(4));
isnt $u.Str, $v.Str, 'two freshly generated UUIDs differ';
