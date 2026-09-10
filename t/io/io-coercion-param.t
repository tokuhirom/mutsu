use Test;

# IO(Cool) coercion parameter and the IO role.
# Regression for File::Directory::Tree, whose subs all use `IO(Cool) $io`.

plan 14;

# IO::Path does the IO role.
ok "foo".IO ~~ IO, 'IO::Path smartmatches IO role';
ok "foo".IO.does(IO), 'IO::Path .does(IO)';
nok IO::Handle ~~ IO, 'IO::Handle does not do the IO role';

# Str -> IO coercion via an IO(Cool) parameter.
sub from-cool(IO(Cool) $io) { $io }
isa-ok from-cool("a/b/c"), IO::Path, 'IO(Cool) coerces a Str to IO::Path';
is from-cool("a/b/c").basename, 'c', 'coerced IO::Path is usable';

# An IO(Cool) parameter also accepts a value that is already IO.
my $p = "a/b/c".IO;
isa-ok from-cool($p), IO::Path, 'IO(Cool) accepts an IO::Path directly';
is from-cool($p).basename, 'c', 'passed-through IO::Path is usable';

# IO() (no explicit source type) form.
sub from-any(IO() $io) { $io }
isa-ok from-any("x/y"), IO::Path, 'IO() coerces a Str';
isa-ok from-any("x/y".IO), IO::Path, 'IO() accepts an IO::Path';

# A coercion-typed `my` declaration.
my IO(Cool) $decl = "p/q";
isa-ok $decl, IO::Path, 'my IO(Cool) coerces the initializer';
$decl = "r/s".IO;
isa-ok $decl, IO::Path, 'reassigning an IO::Path to a my IO(Cool) var works';

# `.=` method-assign re-coerces into a coercion-typed `is copy` parameter.
sub walk-up(IO(Cool) $io is copy) {
    my @seen;
    until $io.basename eq '.' || @seen.elems >= 3 {
        @seen.push: $io.basename;
        $io .= parent;
    }
    @seen;
}
is-deeply walk-up("a/b/c").List, ("c", "b", "a"), '.=parent on IO(Cool) is copy';

# The non-coercion `IO` parameter still accepts an IO::Path.
sub from-io(IO $io) { $io.basename }
is from-io("z/w".IO), 'w', 'plain IO parameter accepts IO::Path';

# Coercing an IO::Path to IO is the identity (no spurious double coercion).
my IO(Cool) $id = "k".IO;
is $id.basename, 'k', 'IO::Path -> IO(Cool) is identity';
