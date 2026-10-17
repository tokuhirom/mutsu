use v6.e.PREVIEW;
use Test;

# ADR-11276 §9.146: the methods language revision 6.e adds (`Any.snip`,
# `Any.snitch`, `Supply.snip`, `Cool.nomark`, `Str.nomark`, `IO::Path.stem`)
# are `REVISION_E` rows: present under 6.e, absent under 6.d
# (`revision-d-method-rows.t`).

plan 20;

is-deeply (1, 5, 2, 8).snip(* < 3), ((1,), (5, 2, 8)), 'Any.snip';
is-deeply (1, 5, 2, 8).snip(* < 3, * < 6), ((1,), (5, 2), (8,)), 'Any.snip with conditions in turn';
is (1, 2).snitch(-> $ { }).elems, 2, 'Any.snitch answers its invocant';
my @seen;
(1, 2).snitch({ @seen.push($_) });
is @seen.elems, 1, 'Any.snitch hands the invocant to the snitcher';

is "Ångström".nomark, 'Angstrom', 'Str.nomark';
is "ÀÉ".nomark, 'AE', 'Str.nomark, several marks';
is 42.nomark, '42', 'Cool.nomark';

is "a.tar.gz".IO.stem, 'a', 'IO::Path.stem drops every extension';
is "a.tar.gz".IO.stem(1), 'a.tar', 'IO::Path.stem(1)';
is "a.tar.gz".IO.stem(5), 'a', 'IO::Path.stem keeps the first part';
is "dir/x.y".IO.stem, 'x', 'IO::Path.stem reads the basename';
is ".bashrc".IO.stem, '', 'a dot file has an empty stem';
is "noext".IO.stem, 'noext', 'no extension';
is "a..b".IO.stem(1), 'a.', 'an empty extension counts';
is "a.b.c.d".IO.stem(2..3), 'a.b', 'a Range counts its elements';

ok Any.^can('snip'), 'Any.^can("snip")';
ok Any.^can('snitch'), 'Any.^can("snitch")';
ok Str.^can('nomark'), 'Str.^can("nomark")';
ok IO::Path.^can('stem'), 'IO::Path.^can("stem")';
ok Supply.^can('snip'), 'Supply.^can("snip")';
