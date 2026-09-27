use Test;

# A bare `if` block binds its condition as `@_` only when the block really
# uses `@_`/`%_`. The probe used to count a string literal that merely spells
# them (`<$_ @_ %_>`, from LLM::Graph's `when $name ∈ <$_ @_ %_>`), and the
# no-`else` form then popped the duplicated condition on the TAKEN path too,
# eating the caller's stack slot: `$_ => f()` came back as `r => Nil`.

plan 14;

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

# A real `@_` read binds the condition as the branch's OWN `@_`; the
# enclosing routine's `@_` is back once the branch ends (#9979).
sub reads { if $c { is-deeply @_, [1], 'the branch sees the condition' }; @_ }
is-deeply reads(5, 6), [5, 6], 'the routine\'s @_ is restored after the branch';
sub pushes { if $c { @_.push(9) }; @_ }
is-deeply pushes(5, 6), [5, 6], 'mutating the branch\'s @_ leaves the routine\'s alone';
is-deeply (1, (if 42 { @_ }), 2), (1, [42], 2), 'value position binds @_';
my @a = 3, 4;
if @a { is-deeply @_, [3, 4], 'an array condition flattens into @_' }
if 0 { } elsif 1, 2 { is-deeply @_, [1, 2], 'elsif binds its condition too' }

# A statement-modifier `if` has no block of its own: its `@_` is the routine's.
sub gcd { return gcd(@_[0] - @_[1], @_[1]) if @_[0] > @_[1]; return gcd(@_[0], @_[1] - @_[0]) if @_[0] < @_[1]; @_[0] }
is gcd(12, 18), 6, 'a statement-modifier if leaves the routine @_ alone';
sub modval { my $x = (@_ if 1); $x }
is-deeply modval(3, 4), [3, 4], 'the same in value position';
