use v6.e.PREVIEW;
use MONKEY-TYPING;
use Test;

# ADR-0067, the argument producer for a callee with NO compile-time name.
#
# The returned-container-consumers slice made an attribute-accessor argument
# hand back its container, gated on the callee's *name*
# (`OpCode::MarkRwArgRefContext`). Two spellings have no usable name, and both
# died with "expects a writable container" where raku writes 9:
#
#   class Sink { method take($y is rw) { $y = 9 } }
#   Sink.new.take($c.v);          # the invocant's class is not a compile-time fact
#   my $r = &g; $r($c.v);         # the callee is a runtime value
#
# Neither needs one. Measured with `--dump-bytecode`, every such spelling pushes
# its callee -- the method's invocant, or the code object itself -- BEFORE its
# arguments, so at the instant the marker runs the real callee is already on the
# stack and the gate asks its actual signature
# (`OpCode::MarkRwArgRefContextCallee`). `&g(...)` is the third row: it has a
# compile-time name that the old gate simply never saw.
#
# Byte-identical under `raku` and `mutsu`.

plan 38;

class C { has $.v is rw }
class Pln { has $.v }

sub fresh { C.new(v => 42) }

# ---------------------------------------------------------------- method callee
class Sink {
    method take($y is rw)   { $y = 9 }
    method takeraw($y is raw) { $y = 9 }
    method takesig(\y)      { y = 9 }
    method takeat($a, $y is rw) { $y = 9 }
    method takenamed($y is rw, :$k) { $y = 9 }
    method plain($y)        { $y }
    method copies($y is copy) { $y = 9 }
}

{
    my $c = fresh;
    Sink.new.take($c.v);
    is $c.v, 9, 'a method-call argument binds the accessor container (chained receiver)';
}
{
    my $c = fresh;
    my $s = Sink.new;
    $s.take($c.v);
    is $c.v, 9, 'the same through a variable receiver';
}
{
    my $c = fresh;
    Sink.new.takeraw($c.v);
    is $c.v, 9, 'an `is raw` method parameter -- was a SILENT drop for a relayed call';
}
{
    my $c = fresh;
    Sink.new.takesig($c.v);
    is $c.v, 9, 'a sigil-less method parameter';
}
{
    my $c = fresh;
    Sink.new.takeat(1, $c.v);
    is $c.v, 9, 'the second positional parameter';
}
{
    my $c = fresh;
    Sink.new.takenamed($c.v, :k(1));
    is $c.v, 9, 'a trailing named argument does not shift the positional index';
}
{
    my $c = fresh;
    Sink.new.takenamed(:k(1), $c.v);
    is $c.v, 9, 'a LEADING named argument does not shift the positional index either';
}
{
    my $c = fresh;
    my $s = Sink.new;
    $s."take"($c.v);
    is $c.v, 9, 'the quoted method-name spelling';
}
{
    my $c = fresh;
    Sink.take($c.v);
    is $c.v, 9, 'a type-object invocant';
}
{
    my $c = fresh;
    Sink.new.take($c.v);
    Sink.new.take($c.v);
    is $c.v, 9, 'the same accessor bound twice through two calls';
}

class Derived is Sink { }
{
    my $c = fresh;
    Derived.new.take($c.v);
    is $c.v, 9, 'a method inherited from a parent class';
}

role Taker { method rtake($y is rw) { $y = 9 } }
class Composed does Taker { }
{
    my $c = fresh;
    Composed.new.rtake($c.v);
    is $c.v, 9, 'a method composed from a role';
}

class Sub2 { submethod stake($y is rw) { $y = 9 } }
{
    my $c = fresh;
    Sub2.new.stake($c.v);
    is $c.v, 9, 'a submethod';
}

class MultiSink {
    multi method mtake(Int $y is rw) { $y = 9 }
    multi method mtake(Str $y)       { }
}
{
    my $c = fresh;
    MultiSink.new.mtake($c.v);
    is $c.v, 9, 'a multi candidate selected by the argument type';
}

class Wherer { method wtake($y is rw where * > 0) { $y = 9 } }
{
    my $c = fresh;
    Wherer.new.wtake($c.v);
    is $c.v, 9, 'a `where`-constrained rw parameter';
}

class Priv {
    method !ptake($y is rw) { $y = 9 }
    method go($z is raw)    { self!ptake($z) }
}
{
    my $c = fresh;
    Priv.new.go($c.v);
    is $c.v, 9, 'relayed through an `is raw` parameter into a private method';
}

class Relay { method pass($z is rw) { Sink.new.take($z) } }
{
    my $c = fresh;
    Relay.new.pass($c.v);
    is $c.v, 9, 'relayed through two method frames';
}

class Inner { has $.v is rw }
class Outer { has Inner $.i is rw }
{
    my $o = Outer.new(i => Inner.new(v => 1));
    Sink.new.take($o.i.v);
    is $o.i.v, 9, 'an accessor reached through another accessor';
}

augment class Int { method mutsuArgTake($y is rw) { $y = 9 } }
{
    my $c = fresh;
    1.mutsuArgTake($c.v);
    is $c.v, 9, 'an `augment`ed method on a native receiver';
}

class SelfCaller {
    method take($y is rw) { $y = 9 }
    method go($o)         { self.take($o.v) }
}
{
    my $c = fresh;
    SelfCaller.new.go($c);
    is $c.v, 9, 'a `self.` call inside a method body';
}

# ------------------------------------------------------------ code-value callee
sub g($y is rw) { $y = 9 }
sub graw(\y)    { y = 9 }
sub gat($a, $y is rw) { $y = 9 }

{
    my $c = fresh;
    my $r = &g;
    $r($c.v);
    is $c.v, 9, 'a call through a code variable';
}
{
    my $c = fresh;
    my $r = &g;
    $r.($c.v);
    is $c.v, 9, 'the `.( )` spelling';
}
{
    my $c = fresh;
    my $r = sub ($y is rw) { $y = 9 };
    $r($c.v);
    is $c.v, 9, 'an anonymous sub held in a scalar';
}
{
    my $c = fresh;
    my $r = -> $y is rw { $y = 9 };
    $r($c.v);
    is $c.v, 9, 'a pointy block held in a scalar';
}
{
    my $c = fresh;
    my $r = &graw;
    $r($c.v);
    is $c.v, 9, 'a sigil-less parameter through a code variable';
}
{
    my $c = fresh;
    my @r = (&g,);
    @r[0]($c.v);
    is $c.v, 9, 'a code value read out of an array element';
}
{
    my $c = fresh;
    my %h = f => &g;
    %h<f>($c.v);
    is $c.v, 9, 'a code value read out of a hash element';
}
{
    my $c = fresh;
    my $r = &gat;
    $r(1, $c.v);
    is $c.v, 9, 'the second positional parameter of a code value';
}
{
    my $c = fresh;
    &g($c.v);
    is $c.v, 9, 'the `&g(...)` spelling -- a compile-time name the old gate never saw';
}
{
    my $c = fresh;
    sub outer($cb) { $cb($c.v) }
    outer(&g);
    is $c.v, 9, 'a code value arriving as a plain parameter';
}
{
    my $c = fresh;
    sub outer2(:&cb) { cb($c.v) }
    outer2(cb => &g);
    is $c.v, 9, 'a `&`-sigil named parameter invoked by name';
}

# -------------------------------------------------------------------- controls
{
    my $p = Pln.new(v => 42);
    dies-ok { Sink.new.take($p.v) },
        'a NON-rw attribute accessor still refuses -- no container to hand over';
}
{
    my $c = fresh;
    dies-ok { Sink.new.take(42) }, 'a literal argument still refuses';
}
{
    my $c = fresh;
    Sink.new.copies($c.v);
    is $c.v, 42, 'an `is copy` parameter still copies';
}
{
    my $c = fresh;
    is Sink.new.plain($c.v), 42, 'a read-only parameter still sees the value';
    is $c.v, 42, '... and the caller is untouched';
}
{
    my $c = fresh;
    my @a;
    @a.push($c.v);
    $c.v = 7;
    is @a[0], 42, 'a native method argument is still a value copy (`@a.push($c.v)`)';
}
{
    my $c = fresh;
    is Sink.new.take($c.v).VAR.^name, 'Int',
        'the call still returns a value, not the container';
}
