use Test;

# A method literal's sigilless named invocant (`anon method (\SELF: |)`, the
# OO::Monitors / Timer::Stopwatch POPULATE idiom) names the receiver. It read
# as `(Any)` in a program that had loaded no module: the parser bound it
# without the sigilless marker, so the body's bare `SELF` compiled to a
# bare-word lookup that only found the binding when the env happened to be
# synced -- and loading any module (this file's own `use Test`) synced it.
# So the cases run in a child process that loads nothing.

plan 5;

sub run-bare(Str $code) {
    my $p = run $*EXECUTABLE, '-e', $code, :out, :err;
    my $out = $p.out.slurp(:close).trim;
    $p.err.slurp(:close);
    $out
}

is run-bare('class B { }; my $m := anon method (\SELF: |) { SELF.^name }; say $m(B.new)'),
    'B', '(\SELF: |) called directly';
is run-bare('class B { }; my $m := anon method (Mu \SELF:) { SELF.^name }; say $m(B.new)'),
    'B', '(Mu \SELF:) called directly';
is run-bare('class B { }; my $m := anon method (\SELF: |) { SELF.^name ~ "/" ~ self.^name }; B.^add_method("m", $m); say B.new.m'),
    'B/B', 'installed with ^add_method, self still works too';
is run-bare('class B { has $.x = 7 }; my $m := anon method (\SELF: $n) { SELF.x + $n }; say $m(B.new, 1)'),
    '8', 'with a following positional';
is run-bare('class B { }; my $m := anon method ($s: |) { $s.^name }; say $m(B.new)'),
    'B', 'a sigiled named invocant is unaffected';
