use Test;

# `.^can` / `.^methods` on a built-in type read the native row catalog's
# DECLARED / INTROSPECTABLE bits. Methods added to the native dispatch after
# the catalog was generated had no row, so introspection denied methods that
# dispatch fine (#11271). Each case: the call works AND introspection agrees,
# with the answer Rakudo gives.

plan 20;

sub works-and-can($obj, Str $name, &call) {
    lives-ok &call, "{$obj.^name}.$name dispatches";
    ok $obj.^can($name).Bool, "{$obj.^name}.^can('$name')";
}

works-and-can (1/3), 'numerator',   { (1/3).numerator };
works-and-can (1/3), 'denominator', { (1/3).denominator };
works-and-can 1.5e0, 'isNaN',       { NaN.isNaN };
works-and-can 5, 'FatRat',          { 5.FatRat };
works-and-can Instant.from-posix(1), 'Bridge', { Instant.from-posix(1).Bridge };
works-and-can Duration.new(1.5), 'Bridge',    { Duration.new(1.5).Bridge };
works-and-can Map.new((a => 1)), 'Bool',      { Map.new((a => 1)).Bool };
works-and-can \(1, a => 2), 'hash',           { \(1, a => 2).hash };

# The type object answers the same way.
ok Rat.^can('numerator').Bool, 'Rat.^can on the type object';
ok Num.^can('isNaN').Bool, 'Num.^can on the type object';

# `.^methods` lists them as Rakudo does.
ok Rat.^methods.map(*.name).grep('numerator'), 'Rat.^methods has numerator';
ok Int.^methods.map(*.name).grep('FatRat'), 'Int.^methods has FatRat';

# vim: expandtab shiftwidth=4
