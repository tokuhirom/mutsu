# Fixture for t/modules/import-export/imported-type-and-term-in-my-stash.t: a
# custom `sub EXPORT` handing out types, a constant and a mixin type object.
my class HookClass { }
my role HookRole { }
my constant HOOK-VALUE = 42;
my sub EXPORT(*@names) {
    Map.new:
        'HookClass'  => HookClass,
        'HookRole'   => HookRole,
        'HOOK-VALUE' => HOOK-VALUE,
        'HookStr'    => (Str but HookRole)
}
