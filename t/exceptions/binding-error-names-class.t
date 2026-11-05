use Test;

plan 8;

class W { }

# The class of the rejected value is named, not the generic `Any`.
try { my %w := W.new; CATCH { default { is .message, 'Type check failed in binding; expected Associative but got W', 'Associative binding names the class'; isa-ok $_, X::TypeCheck::Binding; } } }
try { my @w := W.new; CATCH { default { is .message, 'Type check failed in binding; expected Positional but got W (W.new)', 'Positional binding names the class and shows .raku'; isa-ok $_, X::TypeCheck::Binding; } } }
try { my %w := 42; CATCH { default { is .message, 'Type check failed in binding; expected Associative but got Int', 'Int into %'; } } }
try { my @w := 'x'; CATCH { default { is .message, 'Type check failed in binding; expected Positional but got Str ("x")', 'Str into @'; } } }
try { my %w := W.new; CATCH { default { ok .got ~~ W, '.got is the object'; is .expected.^name, 'Associative', '.expected' } } }
