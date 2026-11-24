use Test;

# #19047: rakudo's `dd` is `note prefix ~ $var.raku`, so the value part is
# whatever `.raku` answers: a typed Array, a Block's signature, a user `raku`
# method. A `dd` the program declares still takes the call.

plan 9;

my $captured = '';
my class Capture {
    method print(*@parts) { $captured ~= @parts.join; True }
    method say(*@parts)   { $captured ~= @parts.join ~ "\n"; True }
}
sub dd-of(&code) {
    $captured = '';
    { my $*ERR = Capture.new; code() }
    $captured.chomp
}

my Int @ti = 1;
is dd-of({ dd @ti }), 'Int  = Array[Int].new(1)', 'a typed Array';
my &cb = { 1 };
is dd-of({ dd &cb }), 'Callable &cb = ' ~ &cb.raku, 'a Block shows its signature';
class G { method raku { "custom-G" } }
is dd-of({ dd G.new }), 'custom-G', 'a user raku method';
class F { has $.x }
is dd-of({ dd F.new(x => 1) }), 'F.new(x => 1)', 'a default instance .raku';
is dd-of({ dd (1..3).map(* + 1) }), '(2, 3, 4).Seq', 'a Seq';
my $s = "a";
is dd-of({ dd $s, 5 }), qq{\$s = "a"\n5}, 'several arguments keep their prefixes';
my @a = 1, 2;
is dd-of({ dd @a[0] }), 'element of @a = 1', 'an element';

my $r;
dd-of({ $r = dd 7 });
nok $r.defined, 'dd returns Nil';

{
    sub dd($x) { "mine $x" }
    is dd(3), 'mine 3', 'a declared dd takes the call';
}
