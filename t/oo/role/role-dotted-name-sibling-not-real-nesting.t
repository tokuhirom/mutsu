use Test;

plan 1;

# A role's fully-qualified name is not always real lexical (block) nesting --
# `role Foo::Bar::Baz { ... }` is frequently just a dotted name chosen by
# convention, with no enclosing `class Foo::Bar { ... }` anywhere. Resolving a
# role method's private nested type by blindly climbing the role's own
# qualified-name prefixes (as if every `::` segment were a real enclosing
# scope) risks matching an unrelated, same-named type declared elsewhere
# under that prefix.
#
# This is the shape that regressed Cro::HTTP's `http-session-inmemory`
# battery test while fixing https://github.com/tokuhirom/mutsu/issues/8565:
# `Cro::HTTP::Middleware::RequestResponse`'s private `my class Request` was
# shadowed by the unrelated top-level `Cro::HTTP::Request` once a naive
# upward walk reached the `Cro::HTTP` prefix -- even though `Cro::HTTP` and
# `Cro::HTTP::Middleware` are never actually declared as their own
# class/role, only used as dotted-name prefixes.

# Stands in for the unrelated top-level `Cro::HTTP::Request`: shares the
# "Request" short name via naming convention only, not real nesting.
class Demo::HTTP::Request {
    method who() { 'WRONG-real-request' }
}

role Demo::HTTP::Middleware::Pair {
    method base() { 'pair' }
}

role Demo::HTTP::Middleware::RequestResponse does Demo::HTTP::Middleware::Pair {
    my class Request {
        method who() { 'inner-request' }
    }
    method request() { Request.new.who }
}

# A parametric role composing the plain role above, mirroring
# `Cro::HTTP::Session::InMemory[::TSession] does Cro::HTTP::Middleware::RequestResponse`.
role Demo::HTTP::Session::InMemory[::T] does Demo::HTTP::Middleware::RequestResponse {
    method use-type() { T.^name }
}

class Consumer does Demo::HTTP::Session::InMemory[Int] { }

is Consumer.new.request, 'inner-request',
    'a role-in-role private nested type is not shadowed by an unrelated dotted-name sibling several segments up';
