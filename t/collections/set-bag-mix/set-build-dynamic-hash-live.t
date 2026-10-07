use Test;

# #12279: a closure called through the interpreter path (an Attribute.set_build
# callback run by `.new`) must read a dynamic hash from the live caller chain,
# not from a snapshot pinned by its first call.
plan 4;

multi sub trait_mod:<is>(Attribute $a, :$env) {
    $a.set_build( -> |c { %*ENV.keys.sort.join(",") } );
}
sub s(&c) { c() }
s({ temp %*ENV = ( :K1<1> ); 1 });
class B { has $.y is env; }
my @got;
s({ temp %*ENV = ( :K2<1> ); @got.push: B.new.y; });
s({ temp %*ENV = ( :K3<1> ); @got.push: B.new.y; });
is @got[0], "K2", "first call sees the live %*ENV";
is @got[1], "K3", "second call sees the new %*ENV, not the first call's";
{ temp %*ENV = ( :K4<1> ); is B.new.y, "K4", "plain temp block"; }
%*ENV = ( :K5<1> );
is B.new.y, "K5", "plain assignment";

done-testing;
