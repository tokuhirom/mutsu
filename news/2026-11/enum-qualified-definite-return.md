# `--> Enum::Key` is a definite return value

`method precedence(--> Precedence::Termlike) {}` returned `Nil`: the runtime classified any
package-qualified return spec as a type, so only the bare spelling of an enum value (`--> Termlike`)
was a definite return. A qualified `Enum::Key` naming a key of a declared enum is now the same
definite return (#16760), as in rakudo.
