# The `core` repository's `.candidates` finds the bundled core distribution by a provided module

Rakudo's core repository holds one distribution whose `provides` names every core module, so
`$core.candidates( CompUnit::DependencySpecification.new( short-name => 'Test' ) )` returns it.
mutsu's `core` repo listed the bundled Rakudo-Core battery in `.installed` but ignored it in
`.candidates`. It is matched now like any installed distribution (by name or a `provides` key, with
the auth/ver/api matchers). Whether the repo should also answer to the name `rakudo` is an identity
question and stays open (mutsu#14907).
