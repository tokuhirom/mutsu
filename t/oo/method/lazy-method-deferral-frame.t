use Test;

# A multi method's deferral frame is built only when a deferral builtin
# first asks for it (#10108). These pin that the late build sees the same
# frame the call itself would have built, wherever the deferral runs from.

plan 11;

sub helper-callsame() { callsame }
sub helper-nextcallee() { nextcallee }

class A {
    multi method body(Int $x)   { "int(" ~ callsame() ~ ")" }
    multi method body(Any $x)   { "any" }
    multi method helper(Int $x) { "int(" ~ helper-callsame() ~ ")" }
    multi method helper(Any $x) { "any" }
    multi method closure(Int $x) { my &c = -> { callsame }; "int(" ~ c() ~ ")" }
    multi method closure(Any $x) { "any" }
    multi method chain(Int $x)  { "int," ~ nextsame }
    multi method chain(Real $x) { "real," ~ nextsame }
    multi method chain(Any $x)  { "any" }
    multi method last(Int $x)   { lastcall; "int(" ~ (callsame() // 'nil') ~ ")" }
    multi method last(Any $x)   { "any" }
    multi method callee(Int $x) { my &n = helper-nextcallee(); "int(" ~ n(self, $x) ~ ")" }
    multi method callee(Any $x) { "any" }
}

is A.new.body(1),    'int(any)', 'callsame in the winning candidate body';
is A.new.helper(1),  'int(any)', 'callsame in a sub called from the body';
is A.new.closure(1), 'int(any)', 'callsame in a closure called from the body';
is A.new.chain(1),   'any',      'nextsame chains through every candidate';
is A.new.last(1),    'int(nil)', 'lastcall empties a frame built late';
is A.new.callee(1),  'int(any)', 'nextcallee from a helper sub';

# A method call nested inside the winner, made before the deferral, is
# already gone when the outer frame is built: the outer call's frame answers.
class B {
    multi method inner(Int $x) { "inner-int" }
    multi method inner(Any $x) { "inner-any" }
    multi method outer(Int $x) { my $i = self.inner($x); "$i/" ~ callsame }
    multi method outer(Any $x) { "outer-any" }
}
is B.new.outer(1), 'inner-int/outer-any', 'an earlier nested call leaves the outer frame intact';

# A deferral inside a nested multi method call builds both frames; each
# answers for its own call.
class C {
    multi method inner(Int $x) { "inner-int(" ~ callsame() ~ ")" }
    multi method inner(Any $x) { "inner-any" }
    multi method outer(Int $x) { self.inner($x) ~ "/" ~ callsame }
    multi method outer(Any $x) { "outer-any" }
}
is C.new.outer(1), 'inner-int(inner-any)/outer-any', 'nested frames built late stay in order';

# A frame built late that turns out to have nothing to defer to is no
# frame at all: the deferral answers Nil, as it would for an eager push.
class D {
    multi method only(Int $x) { callsame() // 'nil' }
    multi method only(Str $x) { 'str' }
}
is D.new.only(1), 'nil', 'a frame with no remaining candidate answers Nil';

# A deferral three calls deep builds every pending frame at once; the
# middle call's turns out empty and is popped without disturbing the
# outer frame below it.
class G {
    multi method deep(Int $x)  { callsame }
    multi method deep(Any $x)  { "deep-any" }
    multi method inner(Int $x) { self.deep($x) }
    multi method inner(Str $x) { 'str' }
    multi method outer(Int $x) { self.inner($x) ~ "/" ~ callsame }
    multi method outer(Any $x) { "outer-any" }
}
is G.new.outer(1), 'deep-any/outer-any', 'an empty middle frame pops cleanly above a built one';

# `is rw` candidates keep an eagerly built frame: their match reads the
# call site's source variables.
class E {
    multi method bump(Int $x is rw) { $x = $x + 1; nextsame }
    multi method bump($x is rw)     { $x = $x + 1000 }
}
my $v = 10;
E.new.bump($v);
is $v, 1011, 'an is-rw candidate chain still writes through';
