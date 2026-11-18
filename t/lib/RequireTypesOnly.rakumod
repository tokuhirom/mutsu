# A module file that declares only a type named apart from the file; its
# body counts how often it runs.
PROCESS::<$REQUIRE-TYPES-ONLY-RUNS> = (PROCESS::<$REQUIRE-TYPES-ONLY-RUNS> // 0) + 1;
class RequireTypesOnlyThing is export { method hi { 'hi' } }
