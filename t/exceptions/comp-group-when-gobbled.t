use Test;

# An undeclared bareword used as a `when` matcher and immediately followed by a
# block is treated by raku as a function call gobbling the block. This leaves
# `when` without its required block and produces an X::Comp::Group bundling an
# X::Syntax::BlockGobbled sorrow (with `.what` naming the function) and an
# X::Syntax::Missing (block) panic.
# https://github.com/Raku/old-issue-tracker/issues/1076
# The X::/CX:: cases below are the reserved exception namespaces; the general
# case is pinned by t/when-undeclared-type-gobbles-block.t.

throws-like 'CATCH { when X::Y {} }', X::Comp::Group,
    sorrows => sub (@s) { @s[0] ~~ X::Syntax::BlockGobbled && @s[0].what ~~ /'X::Y'/ },
    panic => sub ($p) { $p ~~ X::Syntax::Missing && $p.what ~~ /^block/ };

# Same in a plain given/when.
throws-like 'given 1 { when CX::Nope {} }', X::Comp::Group,
    sorrows => sub (@s) { @s[0] ~~ X::Syntax::BlockGobbled && @s[0].what ~~ /'CX::Nope'/ };

# Declared / known types as `when` matchers must keep working: not every
# X::/CX:: name is undeclared.
my $known = "no";
given X::AdHoc.new(payload => "x") {
    when X::AdHoc { $known = "matched" }
    default       { $known = "default" }
}
is $known, "matched", 'known builtin exception type in when still matches';

# A user-declared X:: exception is a valid matcher (not gobbled).
class X::Mine is Exception { method message { "mine" } }
my $user = "no";
given X::Mine.new {
    when X::Mine { $user = "user" }
    default      { $user = "default" }
}
is $user, "user", 'user-declared X:: exception in when still matches';

# CX::Warn control exception in a CONTROL handler is a valid matcher.
my $warned = False;
{
    CONTROL { when CX::Warn { $warned = True; .resume } }
    warn "oops";
}
ok $warned, 'CX::Warn in CONTROL handler still matches';

# A package-qualified enum value is a complete term, not a routine call: the
# head names the declared enum type and the tail one of its values.
enum Day <Mon Tue Wed>;
my $day = "no";
given Mon {
    when Day::Mon { $day = "mon" }
    default       { $day = "default" }
}
is $day, "mon", 'enum-qualified value in when is not flagged';

done-testing;
