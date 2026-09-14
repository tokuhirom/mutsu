use Test;

plan 4;

# Rakudo accepts a pipe before declarations.  Grok 0.0.3 uses `|# ...` before
# a method; this used to make mutsu parse the method as a detached block.
class PipePrefixedMethod {
    |# The answer to everything.
    method answer { 42 }
}

class PipePrefixedAttribute {
    |# The answer stored in an attribute.
    has $.answer = 42;
}

is PipePrefixedMethod.new.answer, 42, '`|#` before a method keeps the declaration';
is PipePrefixedAttribute.new.answer, 42, '`|#` before an attribute keeps the declaration';

class PlainPipePrefix {
    | method answer { 24 }
}

is PlainPipePrefix.new.answer, 24, '`|` before a method keeps the declaration';
ok True, 'the parser reaches the following declaration normally';
