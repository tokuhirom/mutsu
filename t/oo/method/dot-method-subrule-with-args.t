use Test;

# From the Stomp distribution: `<.malformed('invalid command')>` calls a grammar
# method with arguments from inside a token; its `die` must propagate.

plan 9;

grammar Failing {
    token TOP { x || <.boom('reason')> }
    method boom($r) { die "boom $r" }
}
throws-like { Failing.parse("y") }, X::AdHoc, message => 'boom reason',
    'die inside a method subrule with arguments propagates';
ok Failing.parse("x"), 'first alternative still matches';

grammar Typed {
    token TOP { <maybe> }
    token maybe { <[A..Z]>**0..3 $ || <.malformed('invalid command')> }
    method malformed($reason) { die X::AdHoc.new(payload => "bad: $reason") }
}
throws-like { Typed.parse("FOOBAR") }, X::AdHoc, message => 'bad: invalid command',
    'nested fallback alternative dies';
ok Typed.parse("FOO"), 'short input parses';

grammar Args {
    token TOP { <.note-it(1, 'two')> 'a' }
    my @seen;
    method note-it($n, $s) { @seen.push("$n-$s"); self }
    method seen { @seen }
}
ok Args.parse("a"), 'method subrule returning self matches zero-width';
is-deeply Args.seen, ['1-two'], 'positional arguments reach the method';

# From Cro::Core's URI grammar: a ratcheted `[ $ || <.panic(...)> ]` after a
# subrule must not run the panic branch when `$` matched.
grammar Ratchet {
    token TOP { <URI> }
    token URI { <scheme> [ ':' || <.panic('colon') > ] <rest> [ $ || <.panic('end')> ] }
    token scheme { <[a..z]>+ }
    token rest { <[a..z]>* }
    method panic($reason) { die "panic $reason" }
}
ok Ratchet.parse('a:b'), 'zero-width $ branch commits the ratcheted ||';
throws-like { Ratchet.parse('a:b!') }, X::AdHoc, message => 'panic end',
    'the panic branch still runs when $ fails';

my @fired;
grammar CodeBlock {
    token TOP { <URI> }
    token URI { <scheme> ':' <rest> [ $ || { @fired.push('end') } <!> ] }
    token scheme { <[a..z]>+ }
    token rest { <[a..z]>* }
}
CodeBlock.parse('a:b');
is @fired.elems, 0, 'later || branch with side effects is not run once $ matched';
