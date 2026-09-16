use Test;

plan 4;

# Rakudo's core `X::Wrapper` role (added ~2023.10) lets an exception class
# wrap a lower-level exception it was constructed from and expose a
# formatted message for it. mutsu used to register it as an EMPTY marker
# role (like the many genuinely-empty `X::*` category roles it sits next
# to), so a class composing it and calling its private methods died with
# X::Method::NotFound instead of loading -- this is what blocked
# AttrX::Mooish from loading at all (github.com/tokuhirom/mutsu#8573).

class Boom does X::Wrapper {
    method wrapped-message { self!wrappee-message(:concise) }
    method wrapped-details { self!wrappee-message(:details) }
}

my $inner = X::AdHoc.new(payload => "inner boom");
my $b = Boom.new(exception => $inner);

ok Boom ~~ X::Wrapper, "a class composing X::Wrapper does the role";
is $b.exception.^name, "X::AdHoc", "X::Wrapper.exception exposes the wrapped exception";
is $b.wrapped-message, "inner boom", "X::Wrapper's private !wrappee-message resolves and reports the inner message";
ok $b.wrapped-details.contains("inner boom"), "the :details form includes the inner message too";
