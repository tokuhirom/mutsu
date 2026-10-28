class UserPrefixNot::Thing is export { }

multi prefix:<not>(UserPrefixNot::Thing $a) is export { "imported-not" }
