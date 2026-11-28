use Test;

# `bless(*%attrinit)` collects its named arguments into a hash: of several
# arguments naming one attribute only the last is assigned, so an earlier one
# is neither stored nor type-checked. A provided argument is coerced once, for
# both the early seeding and the final store.

plan 9;

class F {
    has Int $.v;
    has $.w;
    method new(*%_) { self.bless(|%_, :v(3)) }
}
is F.new(:v<bad>).v, 3, 'a later argument replaces an earlier one unchecked';
is F.new(:w(1)).w, 1, 'an ordinary argument';

class G {
    has $.a;
    method new() { self.bless(:a(1), :a(2), :a(3)) }
}
is G.new.a, 3, 'the last of several arguments wins';

class D {
    has %.p;
    has @.d;
    has Str() $.s;
    has Int $.n = 5;
    method new(*%_) { self.bless(|%_) }
}
my $d = D.new(p => (a => 1, b => 2), d => (1, 2), s => 42, n => Nil);
is-deeply $d.p, %(a => 1, b => 2), 'a list of Pairs becomes the Hash attribute';
is-deeply $d.d, [1, 2], 'a List becomes the Array attribute';
is $d.s, '42', 'a coercion type coerces';
is $d.n, Int, 'Nil resets to the type object, not the default';
throws-like { D.new(n => 'str') }, X::TypeCheck::Assignment, 'the last argument is checked';

class H {
    has $.size;
    has @.buffer;
    method new(*%_) { self.bless(|%_) }
    submethod TWEAK { $!size = @!buffer.elems }
}
is H.new(buffer => (1, 2, 3)).size, 3, 'TWEAK sees the provided aggregate';
