use Test;
plan 17;

# .WHICH returns ObjAt
isa-ok 42.WHICH, ObjAt, "Int.WHICH returns ObjAt";
isa-ok "hello".WHICH, ObjAt, "Str.WHICH returns ObjAt";
isa-ok True.WHICH, ObjAt, "Bool.WHICH returns ObjAt";
isa-ok 3.14e0.WHICH, ObjAt, "Num.WHICH returns ObjAt";

# .WHICH on type objects
isa-ok Int.WHICH, ObjAt, "Int type object .WHICH returns ObjAt";
isa-ok Str.WHICH, ObjAt, "Str type object .WHICH returns ObjAt";

# Same value gives same WHICH
is 42.WHICH, 42.WHICH, "same Int gives same WHICH";
is "hello".WHICH, "hello".WHICH, "same Str gives same WHICH";

# Different values give different WHICH
isnt 42.WHICH, 43.WHICH, "different Ints give different WHICH";
isnt "hello".WHICH, "world".WHICH, "different Strs give different WHICH";

# ::($expr) indirect type lookup
my $class = "Int";
is ::($class).raku, "Int", "indirect lookup .raku works";
is ::($class).gist, "(Int)", "indirect lookup .gist works";
isa-ok ::($class).WHICH, ObjAt, "indirect lookup .WHICH returns ObjAt";

# Qualified name handling
is ::("IO::Path").gist, "(Path)", "qualified name .gist returns short name";
is ::("IO::Path").raku, "IO::Path", "qualified name .raku returns full name";

# .raku on type objects
is Int.raku, "Int", "Int.raku returns 'Int'";
is Str.raku, "Str", "Str.raku returns 'Str'";
