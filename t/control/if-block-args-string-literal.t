use Test;

# A bare `if` block binds its condition as `@_` only when the block really
# uses `@_`/`%_`. The probe used to count a string literal that merely spells
# them (`<$_ @_ %_>`, from LLM::Graph's `when $name ∈ <$_ @_ %_>`), and the
# no-`else` form then popped the duplicated condition on the TAKEN path too,
# eating the caller's stack slot: `$_ => f()` came back as `r => Nil`.

plan 6;

my $c = 1;

sub lit() { if $c { my $h = '%_' }; 'r' }
$_ = 'm';
is-deeply ($_ => lit()), (m => 'r'), 'a "%_" literal in an if block leaves the caller stack intact';

sub words($name) { my $r = do given $name { when $name ∈ <$_ @_ %_> { 'topic' }; default { 'other' } }; $r }
is-deeply <a $_>.map({ $_ => words($_) }).List, (a => 'other', '$_' => 'topic'),
    'a <$_ @_ %_> word list inside given/when keeps the map pair keys';

class G {
    method call(&f) { if $c { given 'q' { when '%_' { } } }; f() }
    method run { <m n>.map({ $_ => self.call({ 'out' }) }).List }
}
is-deeply G.new.run, (m => 'out', n => 'out'), 'the same through a method';

# A block that does assign `@_` still gets the condition bound, and must not
# unbalance the stack either (the taken branch consumes the duplicate).
sub assigns() { if $c { @_ = 7 }; 'r' }
is-deeply ($_ => assigns()), (m => 'r'), 'an `@_ =` in an if block leaves the caller stack intact';

sub untaken() { if 0 { @_ = 7 }; 'r' }
is-deeply ($_ => untaken()), (m => 'r'), 'the untaken branch pops its duplicate';

is-deeply (1, (if $c { my $s = '@_'; 'v' }), 2), (1, 'v', 2), 'value position with a "@_" literal';
