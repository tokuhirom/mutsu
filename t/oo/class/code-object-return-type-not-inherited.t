use Test;

plan 3;

# A routine's code object (`&name`, `$x.&name`) carries its OWN return
# constraint, never the one of the routine that happened to be running when
# the code object was built. App::Lorea calls `$path.&normalise($root)` inside
# a gather that is pulled from `watch-recursive(... --> Supply)`, and
# normalise's `return $name` failed "expected Supply but got Str".

sub nm($p) { with $p { return "n:$p" } }
sub fd($root) { gather { take $root.&nm } }
sub wr($root --> Int) { my @r = fd($root); @r.elems }

lives-ok { 1.&wr }, 'a callee reached as .&name keeps its own (absent) return type';
is 1.&wr, 1, '... and the caller still returns normally';

sub typed(--> Int) { return 5 }
sub caller-str(--> Str) { &typed() }
throws-like { caller-str() }, X::TypeCheck::Return,
    'the callee\'s own return type is still enforced where it is declared';
