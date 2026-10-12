use Test;

plan 5;

sub warning-of(&code) {
    my $msg;
    {
        CONTROL { when CX::Warn { $msg = .message.lines[0]; .resume } }
        code();
    }
    $msg;
}

my %h; my @a; my $x;

is warning-of({ ~%h<k> }), "Use of uninitialized value %h\{'k'\} of type Any in string context.",
    'prefix ~ names a hash element';
is warning-of({ ~@a[3] }), 'Use of uninitialized value @a[3] of type Any in string context.',
    'prefix ~ names an array element';
is warning-of({ ~$x }), 'Use of uninitialized value $x of type Any in string context.',
    'prefix ~ names a scalar';
is warning-of({ ~Int }), 'Use of uninitialized value of type Int in string context.',
    'an unnameable operand stays unnamed';
is warning-of({ "{ ~$x }" }), 'Use of uninitialized value $x of type Any in string context.',
    'prefix ~ inside an interpolation block';

done-testing;
