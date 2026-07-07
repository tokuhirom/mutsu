unit module ImportedExceptionType;

# A top-level (our-scoped) exception class in the X:: namespace, declared outside
# any `is export` — exactly the shape Zef uses for
# `class X::Zef::UnsatisfiableDependency is Exception`.
class X::Imported::Boom is Exception {
    method message() { "boom" }
}
