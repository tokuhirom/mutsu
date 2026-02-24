use Test;

plan 5;

sub same'proto(::T, T $a, T $b) { $a.WHAT === $b.WHAT };
my &infix:<same-in-Int> = &same'proto.assuming(Int);
throws-like { 42 same-in-Int "42" }, X::TypeCheck::Binding,
    "infix alias from assuming preserves type-capture checks";

sub abc123 (| ($a,$b,$c,$o,$t,$th)) { $a,$b,$c,$o,$t,$th; }
my $primed = &abc123.assuming("a", "b", "c");
my \call = \(1, 2, 3);
my @got = $primed(|call);
is @got.raku, "a b c 1 2 3",
  "capture slipping into primed subsignature keeps argument order";

sub foo(::T $a, T @b) { "$a:@b[]" }
is foo(42, my Int @ = 666,137), "42:666 137",
  "typed generic works on array parameters";

sub bar(::T $a, T %h) { "$a:%h<a b>" }
is bar(42, my Int % = :666a, :137b), "42:666 137",
  "typed generic works on hash parameters";

sub bar2(::T $a, Associative[T] $h) { "$a:$h<a b>" }
is bar2(42, my Int % = :666a, :137b), "42:666 137",
  "typed generic works on associative parameters";
