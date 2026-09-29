use Test;

# From the Test::Builder distribution (t/02-is-isnt.rakutest): `.open` on an
# `IO::Handle` subclass answers the subclass instance, attributes intact.

plan 4;

class H is IO::Handle {
    has $.data = Buf.new;
    method WRITE(IO::Handle:D: Blob:D $buf --> True) { $!data.append: $buf }
}

my $opened = H.new(path => $*SPEC.devnull).open(:w);
is $opened.^name, 'H', '.open returns the subclass';
isa-ok $opened.data, Buf, 'subclass attribute survives .open';

my $anon := class :: is IO::Handle { has $.data = Buf.new }.new(path => $*SPEC.devnull).open: :w;
isa-ok $anon.data, Buf, 'anonymous subclass keeps its attribute';
ok $anon ~~ IO::Handle, 'and is still an IO::Handle';
