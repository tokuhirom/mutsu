use Test;

plan 9;

# `has` cannot be used as the scope for routine/package-style declarations.
throws-like 'has sub a() { }', X::Declaration::Scope,
    scope => 'has', declaration => 'sub';
throws-like 'has package a { }', X::Declaration::Scope,
    scope => 'has', declaration => 'package';
throws-like 'has class a { }', X::Declaration::Scope,
    scope => 'has', declaration => 'class';
throws-like 'has module a { }', X::Declaration::Scope,
    scope => 'has', declaration => 'module';
throws-like 'has role a { }', X::Declaration::Scope,
    scope => 'has', declaration => 'role';
throws-like 'has grammar a { }', X::Declaration::Scope,
    scope => 'has', declaration => 'grammar';

# `has multi` is X::Declaration::Scope::Multi, not the plain variant.
throws-like 'has multi a() { }', X::Declaration::Scope::Multi,
    scope => 'has';

# `our multi` candidates are also illegal.
throws-like 'our multi a() { }', X::Declaration::Scope::Multi,
    scope => 'our';

# `has method` is perfectly valid and must not be rejected.
lives-ok { EVAL 'class C { has method m() { 42 } }' },
    'has method is allowed';
