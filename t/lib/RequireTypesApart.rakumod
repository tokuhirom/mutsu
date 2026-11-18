# A module file that declares only a type named apart from the file; its
# body counts how often it runs.
PROCESS::<$REQUIRE-TYPES-APART-RUNS> = (PROCESS::<$REQUIRE-TYPES-APART-RUNS> // 0) + 1;
class RequireTypesApartThing is export { method hi { 'hi' } }
