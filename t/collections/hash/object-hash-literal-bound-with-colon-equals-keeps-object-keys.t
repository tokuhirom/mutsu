use v6;
use Test;

plan 9;

# An object-hash literal bound with `:=` (`my %h := :{}`) carries no `{Mu}` on
# its declaration, but the hash itself is an object hash: a later element store
# keys by the object, not by its stringification.
class AST { }
my $k = AST.new;

my %a := :{};
%a{$k} = 1;
is %a.keys.map(*.^name).join(","), "AST", "an empty literal bound with :=";
is %a{$k}, 1, "and the object key reads back";

my %b := :{ 5 => 6 };
%b{$k} = 1;
is %b.keys.map(*.^name).sort.join(","), "AST,Int", "a non-empty literal keeps both key types";

our %g := :{};
%g{$k} = 2;
is %g.keys.map(*.^name).join(","), "AST", "an our variable";

my %*D := :{};
%*D{$k} = 3;
is %*D.keys.map(*.^name).join(","), "AST", "a dynamic variable";

sub inner {
    my %*F := :{};
    %*F{$k} = 4;
    %*F.keys.map(*.^name).join(",")
}
is inner(), "AST", "a dynamic variable inside a sub";

# The typed declaration keeps working.
my %t{Mu};
%t{$k} = 5;
is %t.keys.map(*.^name).join(","), "AST", "a declaration typed with a Mu key";

# A plain hash still stringifies its keys.
my %p;
%p{$k} = 6;
is %p.keys.map(*.^name).join(","), "Str", "a plain hash keeps Str keys";

# Iterating the bound hash with a typed key parameter accepts the object key.
my $seen = 0;
for %a.kv -> AST $key, $value { $seen++ }
is $seen, 1, "kv yields the object key";
