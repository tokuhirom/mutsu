use v6;
use Test;
use JSON::Fast;

# :allow-jsonc — JSONC comments (JSON::Fast t/14-comments.t parity).

my $json = Q:to/JSON/;
{
  /* block comment */
  "foo": "bar",  // line comment
  "n": 42,
  // "gone": 1,
  "array": [1, 2, /* 4, */ 3]
}
JSON

is-deeply from-json($json, :allow-jsonc),
    { :foo("bar"), :n(42), :array($[1, 2, 3]) },
    'comments are skipped with :allow-jsonc';

dies-ok { from-json($json) }, 'comments are rejected without :allow-jsonc';

dies-ok { from-json(Q<{"k": /*/ 1}>, :allow-jsonc) },
    '/*/ is not a complete block comment';

dies-ok { from-json(Q<{"k": /* unterminated 1}>, :allow-jsonc) },
    'unterminated block comment is rejected';

is-deeply from-json("[1] // trailing\n", :allow-jsonc), $[1],
    'trailing line comment after the document';

is-deeply from-json("[1] /* trailing */", :allow-jsonc), $[1],
    'trailing block comment at end of input';

# A line comment running into end of input (no newline) is rejected by
# JSON::Fast 0.21.0 on rakudo as well: its comment skipper steps one past the
# end, so the "additional text" check fires. mutsu runs the module verbatim and
# must agree with rakudo here.
dies-ok { from-json("[1] // trailing", :allow-jsonc) },
    'line comment at end of input without a newline dies, as on rakudo';

done-testing;
