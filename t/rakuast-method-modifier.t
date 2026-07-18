use v6;
use Test;

# RakuAST Phase 2 slice 15 (ADR-0011): method-call modifiers `.?` / `.+` / `.*`.
# A modifier adds a `dispatch => ".?"` (etc.) field to the Call::Method, after
# the name and any args. Expected strings captured verbatim from Rakudo; this
# file passes under BOTH mutsu and raku.

plan 4;

# --- `.?` maybe-dispatch ----------------------------------------------------
my $maybe = Q[my $x; $x.?foo].AST.gist;
ok $maybe.contains('postfix => RakuAST::Call::Method.new(')
    && $maybe.contains('name     => RakuAST::Name.from-identifier("foo")')
    && $maybe.contains('dispatch => ".?"'),
    '$x.?foo -> Call::Method with dispatch ".?"';

# --- `.+` all-dispatch ------------------------------------------------------
is Q[my $x; $x.+foo].AST.gist.contains('dispatch => ".+"'), True, '$x.+foo -> dispatch ".+"';

# --- `.*` all-dispatch (zero or more) ---------------------------------------
is Q[my $x; $x.*foo].AST.gist.contains('dispatch => ".*"'), True, '$x.*foo -> dispatch ".*"';

# --- a modifier call with arguments keeps args before dispatch --------------
my $with-args = Q[my $x; $x.?foo(1)].AST.gist;
ok $with-args.contains('args     => RakuAST::ArgList.new(')
    && $with-args.contains('dispatch => ".?"')
    && $with-args.index('args') < $with-args.index('dispatch'),
    'modifier call keeps args, then dispatch';
