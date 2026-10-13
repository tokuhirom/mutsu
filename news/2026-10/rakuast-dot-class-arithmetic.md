# RakuAST retains dot-based character-class arithmetic

The regex source tree now recognizes a leading `.` in character-class
arithmetic such as `<.-:letter-[x]>`. Rakudo omits that implicit all-character
base from `RakuAST::Regex::Assertion::CharClass`, so mutsu does too while
retaining the signed property and enumeration terms. RakuAST conversion,
lowering, and matching are covered for chained subtraction and a later
positive term (#7564).
