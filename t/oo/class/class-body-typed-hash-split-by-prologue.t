use Test;

# #15247 (Path::Finder): a class body whose subs declare a nested class is split
# by the BEGIN prologue, so `my %as{Any:U} = (...)` is a declaration there and
# an assignment here. The assignment must still see the object-hash key type;
# without it the type-object keys were stringified with "Use of uninitialized
# value" warnings.

plan 5;

my @warnings;
{
    CONTROL { when CX::Warn { @warnings.push(.message); .resume } }
    EVAL q:to/CODE/;
        class P {
            my %as{Any:U} = ((Str) => 1, (IO::Path) => 2);
            my Int %n{Any:U} = (Int) => 5;
            method as-count { %as.elems }
            method n-count { %n.elems }
            method as-has-str { %as{Str}:exists }
            sub f() { class Entry { }; 1 }
        }
        CODE
}
is @warnings.elems, 0, "no uninitialized-value warning while the class body runs";
is EVAL('P.as-count'), 2, "both type-object keys are kept";
is EVAL('P.n-count'), 1, "a value-typed object hash too";
ok EVAL('P.as-has-str'), "a type object is a key";
is EVAL('P.as-count'), 2, "stable on a second call";
