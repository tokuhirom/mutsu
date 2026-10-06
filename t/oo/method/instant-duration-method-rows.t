use Test;

plan 82;

# Instant's and Duration's methods are built-in method rows (ADR-11276 slice
# 3D). Both do Real and keep their seconds in one number, so the same checks run
# on both. Every expectation below was checked against Rakudo.
my $dur = Duration.new(7.25);
my $neg = Duration.new(-2.5);
my $zero = Duration.new(0);
my $inst = Instant.from-posix(1709596800.5);

# Truthiness: a zero Duration is false (#11989); an Instant is not zero-false.
is-deeply $zero.Bool, False, 'a zero Duration is false';
is-deeply $dur.Bool, True, 'a nonzero Duration is true';
is-deeply Duration.new(0.5).Bool, True, 'a fractional Duration is true';
is-deeply Instant.from-posix(0).Bool, True, 'an Instant is true even at the POSIX epoch';
ok !$zero, 'prefix ! of a zero Duration';
is ($zero ?? 'yes' !! 'no'), 'no', 'a zero Duration is false in a ternary';

# The numeric coercions read the seconds.
is-deeply $dur.Int, 7, 'Duration.Int truncates';
is-deeply $neg.Int, -2, 'Duration.Int truncates toward zero';
is-deeply $dur.Num, 7.25e0, 'Duration.Num';
is-deeply $dur.Rat, 7.25, 'Duration.Rat';
is-deeply $dur.FatRat, FatRat.new(29, 4), 'Duration.FatRat';
is-deeply $dur.Complex, <7.25+0i>, 'Duration.Complex has a zero imaginary part';
is-deeply $dur.Bridge, 7.25e0, 'Duration.Bridge is a Num';
is-deeply $inst.Int, 1709596837, 'Instant.Int counts TAI seconds';
is-deeply $inst.Complex, <1709596837.5+0i>, 'Instant.Complex';

# Numeric and Real are the object itself.
is-deeply $dur.Numeric, $dur, 'Duration.Numeric is the Duration';
is-deeply $dur.Real, $dur, 'Duration.Real is the Duration';
is-deeply $inst.Numeric, $inst, 'Instant.Numeric is the Instant';
is-deeply $dur.conj, $dur, 'Duration.conj is the Duration';
is (+$dur).^name, 'Duration', 'prefix + keeps the Duration';
is (+$inst).^name, 'Instant', 'prefix + keeps the Instant';

# abs, succ and pred keep the type.
is-deeply $neg.abs, Duration.new(2.5), 'Duration.abs is a Duration';
is $neg.abs.^name, 'Duration', 'Duration.abs keeps the type';
is-deeply $inst.abs, $inst, 'Instant.abs is the Instant';
is-deeply $dur.succ, Duration.new(8.25), 'Duration.succ is a second later';
is-deeply $dur.pred, Duration.new(6.25), 'Duration.pred is a second earlier';
is $dur.succ.^name, 'Duration', 'Duration.succ keeps the type';
is $inst.succ.^name, 'Instant', 'Instant.succ keeps the type';
is-deeply Instant.from-posix(5).succ, Instant.from-posix(6), 'Instant.succ';

# narrow, tai, to-nanos, isNaN.
is-deeply $dur.narrow, 7.25, 'Duration.narrow of a fractional Duration';
is-deeply Duration.new(7).narrow, 7, 'Duration.narrow of a whole Duration is an Int';
is-deeply $dur.tai, 7.25, 'Duration.tai';
is-deeply $dur.to-nanos, 7250000000, 'Duration.to-nanos';
is-deeply $inst.to-nanos, 1709596837500000000, 'Instant.to-nanos';
is-deeply $dur.isNaN, False, 'Duration.isNaN';

# Renderings.
is $dur.Str, '7.25', 'Duration.Str';
is $dur.gist, '7.25', 'Duration.gist';
is $dur.raku, 'Duration.new(7.25)', 'Duration.raku';
is $zero.raku, 'Duration.new(0.0)', 'Duration.raku of a whole number keeps the point';
is $inst.raku, 'Instant.from-posix(1709596800.5)', 'Instant.raku';
is Instant.from-posix(1).raku, 'Instant.from-posix(1.0)', 'Instant.raku of a whole number';

# Instant's own methods.
is Instant.from-posix(5).to-posix[0], 5, 'Instant.to-posix';
is-deeply Instant.from-posix(5).to-posix[1], False, 'Instant.to-posix says it is not a leap second';
is $inst.Instant.raku, $inst.raku, 'Instant.Instant is the Instant';
is-deeply Instant.Instant, Instant, 'the type object answers Instant.Instant';
is-deeply Instant.from-posix(5).DateTime, DateTime.new(1970, 1, 1, 0, 0, 5), 'Instant.DateTime';
is-deeply Instant.from-posix(86400 * 365).Date, Date.new(1971, 1, 1), 'Instant.Date';

# Cool's numeric methods read the seconds, through the shape's MRO.
is-deeply $dur.floor, 7, 'floor';
is-deeply $dur.ceiling, 8, 'ceiling';
is-deeply $dur.round, 7, 'round';
is-deeply $dur.truncate, 7, 'truncate';
is-deeply $neg.floor, -3, 'floor of a negative Duration';
is-deeply $neg.sign, -1, 'sign of a negative Duration';
is-deeply $dur.sign, 1, 'sign of a positive Duration';
is-deeply $zero.sign, 0, 'sign of a zero Duration';
is-deeply Duration.new(4).sqrt, 2e0, 'sqrt';
is-deeply $zero.exp, 1e0, 'exp';
is-deeply Duration.new(1).log, 0e0, 'log';
is-deeply $zero.sin, 0e0, 'sin';
is-deeply $zero.cos, 1e0, 'cos';
is-deeply $zero.cis, <1+0i>, 'cis';
is-deeply $inst.floor, 1709596837, 'Instant.floor';
is-deeply $inst.ceiling, 1709596838, 'Instant.ceiling';
is-deeply $inst.sign, 1, 'Instant.sign';

# Any's methods see one item.
is $dur.elems, 1, 'elems';
is $dur.end, 0, 'end';
is-deeply $dur.min, $dur, 'min';
is-deeply $dur.max, $dur, 'max';
is $dur.sort.elems, 1, 'sort';
is $inst.list.elems, 1, 'list';

# The native integer coercions read the seconds too.
is-deeply $dur.int, 7, 'int';
is-deeply $dur.uint8, 7, 'uint8';
is-deeply $inst.int32, 1709596837, 'Instant.int32';

# Arithmetic and comparison still work on the objects.
is Duration.new(4) + Duration.new(3), 7, 'Duration + Duration';
is Instant.from-posix(8) - Instant.from-posix(5), 3, 'Instant - Instant is a Duration';
ok Duration.new(4) == Duration.new(4), 'Duration == Duration';
ok Duration.new(3) < Duration.new(4), 'Duration < Duration';
is-deeply Instant.from-posix(5) <=> Instant.from-posix(8), Order::Less, 'Instant <=> Instant';
is sprintf('%.2f', $neg), '-2.50', 'sprintf formats the seconds of a Duration';
is $neg.fmt('%.1f'), '-2.5', 'fmt formats the seconds of a Duration';
is-deeply (Duration.new(4), Duration.new(-1)).max, Duration.new(4), 'max of Durations';
is-deeply Date.new(2024, 3, 5) == DateTime.new(2024, 3, 5, 0, 0, 0), False, 'a Date and a DateTime are different values';
