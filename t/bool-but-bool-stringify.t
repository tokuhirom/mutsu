use Test;

plan 10;

# `Bool but Bool` (`True but False`) mixes in a Bool override. `Bool.Str` is
# `self ?? 'True' !! 'False'`, so Str/gist follow the *effective* `.Bool`, not
# the underlying base bool. `.raku` renders as `Bool::True` / `Bool::False`.

my $tf = True but False;
is ?$tf, False, 'True but False is falsy';
is ~$tf, 'False', 'True but False stringifies as False (~)';
is $tf.Str, 'False', 'True but False .Str is False';
is $tf.gist, 'False', 'True but False .gist is False';
is $tf.raku, 'Bool::False', 'True but False .raku is Bool::False';

my $ft = False but True;
is ?$ft, True, 'False but True is truthy';
is ~$ft, 'True', 'False but True stringifies as True';
is $ft.gist, 'True', 'False but True .gist is True';

# An explicit Str mixin still wins over the Bool-derived string.
my $custom = True but False but role { method Str { 'custom' } };
is $custom.Str, 'custom', 'explicit Str mixin overrides Bool-derived string';

# A non-Bool `but` mixin is unaffected (delegates to inner / Str mixin).
is (1 but "x").Str, 'x', 'non-Bool but-mixin unaffected';
