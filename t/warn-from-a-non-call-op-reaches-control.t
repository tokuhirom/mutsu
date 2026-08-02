use Test;

# A resumable warning raised from a plain opcode (`"x" x Int`) must reach an
# enclosing `CONTROL { when CX::Warn { ... .resume } }` handler, and the
# handler's writes to the installing frame's lexicals must survive — including
# when the raise happens inside a closure that makes no calls of its own. A leaf
# closure's return path skips the caller-writeback env scan on the grounds that
# "no call was made, so nothing outward can have changed"; an inline CONTROL
# handler breaks that assumption. This is what `Test::Util`'s `warns-like` does
# (roast/S03-operators/repeat.t test 56).

plan 6;

sub in-leaf-closure(&code) {
    my $did-warn = False;
    my $message = '';
    &code();
    CONTROL { when CX::Warn { $did-warn = True; $message = .message; .resume } }
    ($did-warn, $message);
}

{
    my ($warned, $message) = in-leaf-closure { "x" x Int };
    ok $warned, 'string repeat with a type-object count reaches the CONTROL handler';
    like $message, /uninitialized/, 'and the handler keeps the message';
}

# The same raise in the CONTROL-installing frame itself (no intervening closure)
# already worked; keep it pinned so a fix for one shape cannot break the other.
{
    my $warned = False;
    my $ignored = "x" x Int;
    CONTROL { when CX::Warn { $warned = True; .resume } }
    ok $warned, 'a same-frame raise still reaches the handler';
}

# A method-call raise site through the same leaf-closure shape.
{
    my ($warned, $message) = in-leaf-closure { Int.Numeric };
    ok $warned, 'a native-method warning reaches the handler through a closure';
    like $message, /uninitialized/, 'and keeps its message';
}

# `quietly` still suppresses the repeat-count warning entirely.
{
    my $out = quietly { "x" x Int };
    is $out, '', 'quietly still swallows the repeat-count warning';
}
