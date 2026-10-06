use Test;

# `.join` stringifies every undefined element and raises one resumable warning
# for each, naming the aggregate when its container descriptor is available.

plan 7;

sub warnings-of(&code) {
    my @warnings;
    code();
    CONTROL {
        when CX::Warn {
            @warnings.push: .message.lines.head;
            .resume;
        }
    }
    @warnings
}

my @b = 1, Nil, Any;
my $joined;
my @warnings = warnings-of({ $joined = @b.join('-') });
is $joined, '1--', 'join keeps its empty string result for undefined elements';
is-deeply @warnings,
    [
        'Use of uninitialized value @b of type Any in string context.',
        'Use of uninitialized value @b of type Any in string context.',
    ],
    'each undefined element warns and names the array';

@warnings = warnings-of({ $joined = @b.join });
is $joined, '1', 'zero-argument join keeps its empty separator';
is-deeply @warnings,
    [
        'Use of uninitialized value @b of type Any in string context.',
        'Use of uninitialized value @b of type Any in string context.',
    ],
    'zero-argument join also warns once per undefined element';

my @c;
@c[3] = 1;
@warnings = warnings-of({ $joined = @c.join('|') });
is $joined, '|||1', 'an array hole still joins as an empty string';
is-deeply @warnings,
    ['Use of uninitialized value of type Any in string context.'],
    'sparse holes produce one unnamed warning';

@warnings = warnings-of({ $joined = (Any, Nil).join(',') });
is-deeply @warnings,
    [
        'Use of uninitialized value of type Any in string context.',
        'Use of Nil in string context',
    ],
    'an unnamed List warns for each kind of undefined value';
