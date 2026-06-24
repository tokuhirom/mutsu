use Test;
use lib 't/lib';
use ShadowBuiltin;

# An imported sub whose name collides with a core builtin (here `get`, the IO
# line-reader, and `close`) must shadow the builtin in the importing scope when
# called with matching arguments. This is what lets Sinatra/Bailador-style web
# frameworks export `get`/`post`/`close` route declarators that override the
# same-named core builtins.

plan 5;

# Parenthesized call dispatches to the imported sub, not the builtin `get`
# (which would expect an IO::Handle and fail).
is get("/a", sub { "x" }), "GET /a -> x", 'paren call hits imported get';

# No-paren listop call also dispatches to the imported sub.
is (get "/b", sub { "y" }), "GET /b -> y", 'listop call hits imported get';

# `close` (also a builtin) resolves to the imported sub.
is close("db"), "closed db", 'imported close shadows builtin close';

# The imported handler block actually runs.
is get("/c", sub { 1 + 1 }), "GET /c -> 2", 'imported get runs the handler block';

# Unshadowed builtins keep working in the same scope.
is "Hello".uc, "HELLO", 'unshadowed builtins still work';
