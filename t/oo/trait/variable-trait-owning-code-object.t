use v6.d;
use Test;

plan 10;

# ADR-12131 section 6: a variable trait handler is a compile-time effect that
# runs once, whatever kind of code object owns the declaration, and
# Variable.block reports that code object's type.
my @handled;
my @entered;

multi trait_mod:<is>(Variable:D $v, :$foo!) {
    @handled.push($v.block.^name);
    $v.block.add_phaser("ENTER", { @entered.push("enter") });
}

class C {
    method m { my $x is foo; 1 }
    submethod sm { my $y is foo; 1 }
}
role R {
    method rm { my $z is foo; 1 }
}
class K does R { }
sub s { my $a is foo; 1 }
my &anon = sub { my $b is foo; 1 };
my &blk = { my $c is foo; 1 };

# Every handler has already run: nothing above was called yet.
is-deeply @handled.sort.List, <Block Method Method Sub Sub Submethod>.sort.List,
    'each handler ran once before any code object was called, with its owner type';
is @entered.elems, 0, 'no ENTER phaser ran yet';

@handled = ();
C.m; C.m;
is @handled.elems, 0, 'calling a method does not run its variable trait again';
is @entered.elems, 2, '... but its ENTER phaser runs on every call';

@entered = ();
C.new.sm; C.new.sm; K.new.rm; K.new.rm;
is @handled.elems, 0, 'a submethod and a role method do not rerun the handler either';
is @entered.elems, 4, '... and their ENTER phasers run on every call';

@entered = ();
s(); anon(); blk();
is @handled.elems, 0, 'a sub, an anonymous sub and a block do not rerun it';
is @entered.elems, 3, '... and their ENTER phasers run';

# The owner type, one handler per declaration.
my @owner;
multi trait_mod:<is>(Variable:D $v, :$who!) { @owner.push($v.block.^name) }
sub owned-by-sub { my $q is who; }
class D { method owned-by-method { my $q is who; } }
is-deeply @owner.sort.List, <Method Sub>.List, 'Variable.block.^name is the owning code type';

# A method body's seeded variable keeps the ENTER phaser's value per call.
multi trait_mod:<is>(Variable:D $v, :$seeded!) {
    $v.block.add_phaser("ENTER", { $v.var = 42 without $v.var });
}
class E { method get { my $z is seeded; $z } }
is E.get, 42, 'a phaser seeds a method body variable';
