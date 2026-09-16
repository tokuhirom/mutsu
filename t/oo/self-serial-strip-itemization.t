use Test;

# Issue #8490: `.self` (and `.serial`, which shares the same "hand back the
# invocant's value" model) hands out the *value*, not the container -- so an
# itemized Array/Hash/Slip must lose its `$` itemization marker in `.raku`,
# exactly like `$x<>` already does. mutsu's "self"/"serial" dispatch arms
# (src/builtins/methods_0arg/dispatch_core_coerce.rs) used to return the
# invocant verbatim, keeping the itemization for every container kind.
#
# t/oo/method/self-method-decontainerizes.t already pins the identity
# (`=:=`) side of `.self`; this file pins the itemization-marker side.

plan 8;

my $h = {a => 1};
is $h.self.raku, '{:a(1)}', '.self strips the $ marker from an itemized Hash';
is $h.serial.raku, '{:a(1)}', '.serial does the same';

my @c = 1, 2;
my $a = @c;
is $a.self.raku, '[1, 2]', '.self strips the $ marker from an itemized Array';
is $a.serial.raku, '[1, 2]', '.serial does the same';

my $x = slip(5, 6);
is $x.self.raku, 'slip(5, 6)', '.self strips the $ marker from an itemized Slip';
is $x.serial.raku, 'slip(5, 6)', '.serial does the same';

# Non-itemized values are unaffected.
is (1).self.raku, '1', '.self on a plain Int is unaffected';
my @arr = 1, 2;
is @arr.self.raku, '[1, 2]', '.self on an already-unitemized Array is unaffected';
