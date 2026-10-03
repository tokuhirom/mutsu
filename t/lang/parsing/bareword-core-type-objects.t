use Test;

plan 3;

# Every core type name is a term yielding its type object, never the name as a Str.
is (Macro, CompUnit, Grammar, Routine).map(*.^name).join(" "), "Macro CompUnit Grammar Routine",
    "Macro and friends resolve to type objects";
isa-ok Macro, Macro, "Macro is its own type object";
is PROCESS.^name, "PROCESS", "PROCESS resolves to the PROCESS package";
