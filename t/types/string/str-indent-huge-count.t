use Test;

plan 4;

# A huge indent count must throw rakudo's repeat-count error (one shared
# repeat primitive), not abort on allocation failure.
my $died = False;
my $msg = '';
try { "abc".indent(99999999999); CATCH { default { $died = True; $msg = .message } } }
ok $died, 'indent with a huge count throws';
like $msg, /'Repeat count (99999999999) cannot be greater than max allowed number of graphemes 4294967295'/,
    'message matches rakudo';

is "a\n b".indent(2), "  a\n   b", 'ordinary indent unchanged';
is "\ta".indent(8), "\t\ta", 'tab-led indent by a tabstop multiple adds a tab';
