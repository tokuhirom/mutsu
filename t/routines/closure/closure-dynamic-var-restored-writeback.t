use Test;

# A closure's call must propagate a dynamic-variable write made by a routine it
# calls, even when the written value equals what the variable held when the
# closure was CREATED (#10996: Template::HAML's `- tab-up 2` / `- tab-down 2`).

plan 5;

sub up(Int $n)  { $*OFF = $*OFF + $n }
sub dn(Int $n)  { $*OFF = $*OFF - $n }
sub reset-off() { $*OFF = 0 }

sub same-amount() {
    my Int $*OFF = 0;
    my $a = -> { up 2 };
    my $b = -> { dn 2 };
    $a();
    $b();
    $*OFF;
}
is same-amount(), 0, 'tab-down N after tab-up N returns to the start value';

sub other-amount() {
    my Int $*OFF = 0;
    my $a = -> { up 3 };
    my $b = -> { dn 3 };
    $a();
    my $mid = $*OFF;
    $b();
    "$mid $*OFF";
}
is other-amount(), '3 0', 'the intermediate value is visible and the reset propagates';

sub reset-to-capture() {
    my $*OFF = 0;
    my $b = -> { reset-off() };
    $*OFF = 5;
    $b();
    $*OFF;
}
is reset-to-capture(), 0, 'a nested write back to the capture-time value is not dropped';

sub evald() {
    my Int $*OFF = 0;
    my $a = EVAL '-> { up 2 }';
    my $b = EVAL '-> { dn 2 }';
    $a();
    $b();
    $*OFF;
}
is evald(), 0, 'EVAL-built blocks propagate the same way';

sub lexical-control() {
    my $x = 0;
    my $b = -> { $x = 0 };
    $x = 5;
    $b();
    $x;
}
is lexical-control(), 0, 'a lexical free variable written back to its capture value';
