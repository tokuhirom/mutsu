use lib 't/lib';

# `&ok` taken inside a block that imports Test stays Test's multi `ok`
# even after a same-named single `ok` is imported into the outer scope.
my ($ok, $plan, $is) = do { use Test; (&ok, &plan, &is) };

use LaterImportedOk;

$plan(3);
$ok(1, 'the captured dispatcher calls its own candidates');
$is(ok(5), 'later:5', 'the later import answers the bare name');
$ok(True, 'two-argument call still reaches Test::ok');
