use Test;

# An explicit `*%h` on a method binds on the fast binder (#18267). It keeps
# raku's semantics: a variable argument stays the caller's container.
class C {
    has $.i;
    method m(*%h) { $!i := %h<a>; self }
    method n(*%h) { %h<a> = 3 }
    method k(:$z, *%h) { %h.keys.sort.join(",") }
    method e(*%h) { %h.elems }
    method w(*%h) { %h = (q => 1); %h<r> = 2; %h.sort.Str }
    method nm(*%h) { %h.VAR.name }
    method fwd(*%h) { self.k(|%h) }
    method raw(*%h) { %h<a>.VAR.^name }
}
my $y = 1; my $c = C.new.m(:a($y)); $y = 5; is $c.i, 5, 'a bound hash element aliases the caller variable';
C.new.n(:a($y)); is $y, 3, 'assigning a hash element writes the caller variable';
is C.new.k(:z(1), :b(2), :c(3)), 'b,c', 'explicit named params are not collected';
is C.new.e(), 0, 'no named args, empty hash';
is C.new.w(:x(9)), "q\t1 r\t2", 'the slurpy hash is assignable';
is C.new.nm(:x(1)), 'element', 'the slurpy hash is an anonymous container';
is C.new.fwd(:z(1), :p(2)), 'p', 'forwarding through |%h';
is C.new.raw(:a(5)), 'Int', 'a value argument arrives as a value';
my $obj = C.new;
for ^3 { my $v = $_; $obj.n(:a($v)); is $v, 3, "loop variable $_ written through the hash" }
sub raku-name { '%h' }
done-testing;
