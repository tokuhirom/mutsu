unit module HookOperatorFamily::Ops;
class Thing is export {}
multi infix:<+>(Thing $a, Numeric() $b) is export { "imported" }
multi prefix:<++>(Thing $a) is export { "++thing" }
multi postfix:<++>(Thing $a) is export { "thing++" }
