use Test;

# A bare identifier inside `is export(...)` is a term reference, not an export
# tag (`:FOO` is the tag form). An undeclared bare name there is
# X::Undeclared::Symbols, matching rakudo ("Undeclared name: WTF").

plan 4;

throws-like 'sub foo() is export(WTF) { }', X::Undeclared::Symbols,
    'bare word in export(...) is X::Undeclared::Symbols';

# The valid forms still parse and run.
lives-ok { EVAL 'sub a() is export { }' }, 'is export (no args) lives';
lives-ok { EVAL 'sub b() is export(:MANDATORY) { }' }, 'is export(:tag) lives';
lives-ok { EVAL 'sub c() is export(:DEFAULT, :ALL) { }' }, 'is export with multiple :tags lives';
