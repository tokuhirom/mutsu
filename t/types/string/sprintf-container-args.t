use Test;

# A hash slice yields its elements' item containers. `sprintf('%d', ...)`
# formatted such a container as 0 (`%s` was fine). Reduced from
# Timezones::ZoneInfo's t/99-former-bugs.rakutest:
# `sprintf('%04d-%02d-...', %t<Y M D h m s>)`.

plan 6;

my %t = Y => 2022, M => 3, D => 13;
is sprintf('%04d-%02d-%02d', %t<Y M D>), '2022-03-13', 'hash slice args to %d';
is sprintf('%04d-%02d', |%t<Y M>), '2022-03', 'slipped hash slice';
my $l = %t<Y M>;
is sprintf('%d %d', |$l), '2022 3', 'slipped itemized slice';
is sprintf('%.1f', %t<D D>[0]), '13.0', '%f on a slice element';
is sprintf('%x', %t<Y Y>[0]), '7e6', '%x on a slice element';
my @a = 1, 2;
is sprintf('%d-%d', @a[0, 1]), '1-2', 'array slice args';
