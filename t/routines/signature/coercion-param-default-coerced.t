use Test;

plan 6;

class CL {
    has $.v;
    multi method COERCE(Whatever) { self.new(v => 'star') }
    multi method COERCE($x) { self.new(v => $x) }
}

# A coercion-typed parameter coerces its default like a supplied argument.
sub f(CL(Any) $c = *) { $c.^name ~ ':' ~ $c.v }
is f(), 'CL:star', 'positional default runs the target COERCE';
is f(5), 'CL:5', 'a supplied argument still coerces';

sub g(Int(Str) $n = "42") { $n.^name }
is g(), 'Int', 'builtin coercion of a default';

sub h(Int() $x = "9") { $x + 1 }
is h(), 10, 'Int() coerces its default';

sub i(Int(Any) $x = Int) { $x.^name }
is i(), 'Int', 'a type-object default is left alone';

sub j(Str(Int) :$n = 3) { $n.^name }
is j(), 'Str', 'named default is coerced too';
