use Test;
use lib 't/lib';

# Rakudo rejects a call to a routine declared nowhere in a required module's
# own compilation unit at CHECK time (X::Undeclared::Symbols), before the
# module body runs. mutsu used to silently treat such a bareword as a plain
# Str, so `require` on a module whose source does not compile still
# succeeded -- see https://github.com/tokuhirom/mutsu/issues/8986.
#
# t/lib/RequireUndeclaredBareword.rakumod contains:
#   unit class RequireUndeclaredBareword;
#   dead

plan 3;

throws-like { require RequireUndeclaredBareword }, X::Undeclared::Symbols,
    message => /'dead'/,
    'require rejects a module whose mainline calls an undeclared routine';

throws-like { require ::("RequireUndeclaredBareword") }, X::Undeclared::Symbols,
    message => /'dead'/,
    'indirect require ::(Name) rejects the same undeclared routine';

# A required module that does everything right must still load normally.
{
    require RequireGreeter;
    my $g = RequireGreeter.new(name => "Undeclared-sibling");
    is $g.greet, "Hello, Undeclared-sibling!",
        'a well-formed required module still loads fine';
}

# vim: expandtab shiftwidth=4
