use Test;

# An element store into a routine's own variable writes back only the
# routine's own binding: a caller's same-named variable, reached past the
# frame root, never replaces it (#19186, Rakuish's Arrow `as-bits`).

plan 4;

class Foo { }

sub as-bits(@flags) {
    my Buf $b = Buf.new(0, 0);
    for ^@flags.elems -> $i { $b[$i div 8] += 1 +< ($i % 8) if @flags[$i] }
    $b;
}

my $b = Foo.new;
my &keep = sub { $b };   # binds the mainline's $b in the env

is-deeply as-bits([True, False, True]), Buf.new(5, 0),
    "a sub's own Buf survives element stores while a caller binds \$b";
isa-ok keep(), Foo, "the caller's \$b is untouched";

sub hash-store() {
    my %h;
    my $o = %h;
    $o<k> = 1 for ^3;
    $o
}
my $o = Foo.new;
my &keep-o = sub { $o };
is-deeply hash-store().Hash, %(k => 1), 'a scalar-held Hash store keeps its own container';
isa-ok keep-o(), Foo, "the caller's \$o is untouched";
