use v6;
use JSON::Fast;
use Test;

# A native-module routine (from-json/to-json) called in STATEMENT position
# with a named argument compiles to ExecCallPairs -> exec_call, which had no
# native-JSON dispatch and died "Unknown call: from-json". This is the
# JSON::Fast t/01-parse.t shape (mzef Test phase):
#     try { from-json($t, :$immutable); $parsed = True; }

plan 4;

my $parsed = False;
try {
    from-json(Q<{ "a": 1 }>, :immutable(False));
    $parsed = True;
}
ok $parsed, 'statement-position from-json with a named arg runs';

sub run-one($t, Bool :$immutable) {
    my Bool $ok = False;
    try {
        from-json($t, :$immutable);
        $ok = True;
    }
    $ok;
}
ok run-one(Q<[1, 2]>), 'the forwarded-named-arg sub shape works';
nok run-one(Q<[1, 2>), 'a genuinely broken document still fails';

try {
    to-json({ a => 1 }, :pretty(False));
    $parsed = 'to-json-ok';
}
is $parsed, 'to-json-ok', 'statement-position to-json with a named arg runs';
