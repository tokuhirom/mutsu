use Test;

# A subscript on a type name is a type application, not a subscript, so a
# colonpair after it is refused at compile time: X::Syntax::Adverb, "You can't
# adverb Array[Int]" (rakudo 2026.11), not a run-time postcircumfix dispatch
# with a `:D` named argument (#18323).

plan 8;

for ｢Array[Int]:D｣                 => 'Array[Int]',
    ｢Hash[Int,Str]:exists｣         => 'Hash[Int,Str]',
    ｢Array[Array[Int]]:D｣          => 'Array[Array[Int]]',
    ｢Array[Int]:!D｣                => 'Array[Int]',
    ｢Array[Int] :D｣                => 'Array[Int]',
    ｢role R[::T] { }; R[Int]:D｣    => 'R[Int]' -> (:key($code), :value($what)) {
    throws-like $code, X::Syntax::Adverb, :what($what), message => "You can't adverb $what",
        "rejected: $code";
}

my @a = 1, 2;
is @a[0]:exists, True, 'an adverb on a real subscript still works';
is Array[Int].^name, 'Array[Int]', 'and the type application alone is fine';
