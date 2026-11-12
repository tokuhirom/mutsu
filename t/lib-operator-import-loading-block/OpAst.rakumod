# An `infix:<eq>` candidate for a query-like type, handed out by a `sub EXPORT`
# hook, as Red exports Red::Operators.
my class OpAst { has $.name }
my multi infix:<eq>(OpAst $a, Str $b) { "{$a.name} = '$b'" }
sub EXPORT { Map.new('&infix:<eq>' => &infix:<eq>, 'OpAst' => OpAst) }
