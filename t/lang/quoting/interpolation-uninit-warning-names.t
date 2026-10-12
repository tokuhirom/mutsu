use Test;

plan 5;

sub warning-of(&code) {
    my $msg = '';
    { code(); CONTROL { when CX::Warn { $msg = .message.lines[0]; .resume } } }
    $msg;
}

my %h; my @a; my $s;
is warning-of({ my $x = "%h<k>" }),
    "Use of uninitialized value %h\{'k'} of type Any in string context.",
    'a hash element in a string names the container and key';
is warning-of({ my $x = "@a[3]" }),
    'Use of uninitialized value @a[3] of type Any in string context.',
    'an array element in a string names the container and index';
is warning-of({ my $x = "v=$s" }),
    'Use of uninitialized value $s of type Any in string context.',
    'a scalar variable in a string is named';
my %g = a => 1;
is warning-of({ my $x = "%g<b>" }),
    "Use of uninitialized value %g\{'b'} of type Any in string context.",
    'a missing key of a populated hash is named';
is warning-of({ my $x = "a{Int}b" }),
    'Use of uninitialized value of type Int in string context.',
    'a block part is unnamed';
