use Test;

plan 5;

# A class body's `my method` called by name takes its invocant from the first
# argument. A variable argument arrives as a VarRef, which must be unwrapped to
# the object so the method's `self` and `$!attr` are that object.
class C {
    has $!v = 10;
    my method m($x) { "$!v $x" }
    my method who { self.WHICH }
    method by-temp   { m(C.new, 1) }
    method by-var    { my $o = C.new; m($o, 2) }
    method by-self   { m(self, 3) }
    method by-sigilless { my \o = C.new; m(o, 4) }
    method same-object { my $o = C.new; who($o) eq $o.WHICH }
}

is C.new.by-temp, '10 1', 'invocant given as a fresh object';
is C.new.by-var, '10 2', 'invocant given as a scalar variable';
is C.new.by-self, '10 3', 'invocant given as self';
is C.new.by-sigilless, '10 4', 'invocant given as a sigilless variable';
ok C.new.same-object, 'self inside is the very object, not a wrapper';
