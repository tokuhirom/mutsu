use Test;

plan 6;

class D {
    has $.w = 10;
    has $.r is rw = 10;
    has $!v = 10;
    method pub  { my $p := $.w; $p = 11; $!w }
    method rw   { my $p := $.r; $p = 11; $!r }
    method priv { my $p := $!v; $p = 11; $!v }
    method copy-value { my $p := $.w; $p = 12; $p }
    method reads { my $p := $.w; $p }
}

is D.new.pub, 10, 'binding a non-rw $.w keeps a copy';
is D.new.rw, 11, 'binding an is-rw $.r aliases the attribute';
is D.new.priv, 11, 'binding $!v aliases the attribute';
is D.new.copy-value, 12, 'the bound copy is itself assignable';
is D.new.reads, 10, 'the bound copy reads the accessor value';
my $d = D.new(w => 3);
is $d.reads, 3, 'a constructor-set value is read';
