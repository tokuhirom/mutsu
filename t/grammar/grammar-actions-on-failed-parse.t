use Test;

plan 8;

# Rakudo dispatches a grammar action the moment its rule matches (at reduce
# time) and never un-dispatches it when the enclosing pattern backtracks past
# it. So a `.parse` that FAILS overall still leaves behind the effects of every
# subrule that did match. mutsu walks the finished match tree instead, so this
# used to run no actions at all on a failed parse — which broke
# HTTP::Header.parse (its TOP is `[ <message-header> \r?\n ]*`, so a header
# block without a trailing newline makes TOP match "" and the parse fail, yet
# raku has already populated the header object).

grammar Headers {
    token TOP { [ <message-header> \r?\n ]* }
    token message-header { $<field-name>=[ <-[:]>+ ] ':' \h* $<field-value>=[ \N* ] }
}

my @fired;
class HeaderActions {
    method message-header($/) { @fired.push: ~$<field-name> ~ '=' ~ ~$<field-value> }
}

# The whole input matches TOP, so the parse succeeds.
@fired = ();
ok Headers.parse("ETag: abc\n", :actions(HeaderActions)).defined, 'parse with trailing newline succeeds';
is-deeply @fired, ['ETag=abc'], 'action ran once on the successful parse';

# No trailing newline: the `*` commits zero iterations, TOP matches "" and the
# parse fails — but `message-header` DID match, so its action must have run.
@fired = ();
nok Headers.parse("ETag: abc", :actions(HeaderActions)).defined, 'parse without trailing newline fails';
is-deeply @fired, ['ETag=abc'], 'action still ran for the subrule that matched';

# The start rule's own action fires too when it matched but the parse stops
# short of the end of the input.
grammar Words {
    token TOP { <word>+ }
    token word { \w }
}
my @seen;
class WordActions {
    method word($/) { @seen.push: "w:$/" }
    method TOP($/)  { @seen.push: "TOP:$/" }
}
@seen = ();
nok Words.parse("xy!", :actions(WordActions)).defined, 'parse stopping short of the end fails';
is-deeply @seen, ['w:x', 'w:y', 'TOP:xy'], 'the partial tree dispatched bottom-up, start rule included';

# The start rule fails outright: it still reduced `<body>` on the way, so that
# action ran, but the start rule's own did not.
grammar Tail {
    token TOP { <body> 'ZZZ' }
    token body { \w+ }
}
my @ran;
class TailActions {
    method body($/) { @ran.push: "body:$/" }
    method TOP($/)  { @ran.push: "TOP:$/" }
}
@ran = ();
nok Tail.parse("qq", :actions(TailActions)).defined, 'start rule that cannot match fails';
is-deeply @ran, ['body:qq'], 'the subrule that reduced ran its action, the start rule did not';
