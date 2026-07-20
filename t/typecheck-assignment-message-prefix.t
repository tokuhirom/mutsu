use v6;
use Test;

# An X::TypeCheck::Assignment exception's `.message` / `.Str` / `.gist` must NOT
# embed the `X::TypeCheck::Assignment: ` class-name prefix (raku puts the class
# name nowhere in the message). Baking it in doubled the class name when user
# code prints `.^name, ': ', .Str` -- the shape of the doc example in
# Language/typesystem.rakudoc.

plan 6;

# Reassigning a wrong type to a declared typed scalar.
{
    my Int $x = 42;
    $x = "hello";
    CATCH {
        default {
            is .message,
                'Type check failed in assignment to $x; expected Int but got Str ("hello")',
                'scalar reassignment .message has no class prefix';
        }
    }
}

# Subset-constrained scalar (the doc example [18]).
{
    subset Positive of Int where * > -1;
    my Positive $i = 1;
    $i = -42;
    CATCH {
        default {
            is .^name, 'X::TypeCheck::Assignment', 'subset typecheck .^name';
            is .message,
                'Type check failed in assignment to $i; expected Positive but got Int (-42)',
                'subset typecheck .message has no class prefix';
            # `.^name, ': ', .Str` must not double the class name.
            is .^name ~ ': ' ~ .Str,
                'X::TypeCheck::Assignment: Type check failed in assignment to $i; expected Positive but got Int (-42)',
                'name + Str reads exactly once';
        }
    }
}

# Typed array element assignment.
{
    my Int @a;
    @a[0] = "hi";
    CATCH {
        default {
            is .message,
                'Type check failed for an element of @a; expected Int but got Str ("hi")',
                'element .message has no class prefix';
        }
    }
}

# `.Str` equals `.message` (no prefix on either).
{
    my Int $y = 1;
    $y = "x";
    CATCH {
        default {
            is .Str, .message, '.Str equals .message';
        }
    }
}

done-testing;
