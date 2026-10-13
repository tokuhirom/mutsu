use Test;

plan 9;

# Every stringification of a code object warns "<Type> object coerced to
# string" and answers the name (#12762).
sub foo {}
class C { method m {} }

sub warnings(&code) {
    my @w;
    {
        code();
        CONTROL { when CX::Warn { @w.push(.message); .resume } }
    }
    @w
}

my $s;
my @w = warnings({ $s = ~&foo });
is $s, 'foo', 'prefix ~ answers the name';
is @w.elems, 1, 'prefix ~ warns once';
like @w[0], /^ 'Sub object coerced to string'/, '... naming Sub';

@w = warnings({ $s = "x{&foo}" });
is $s, 'xfoo', 'interpolation answers the name';
is @w.elems, 1, 'interpolation warns once';

@w = warnings({ $s = &foo ~ 'y' });
is $s, 'fooy', 'infix ~ answers the name';
is @w.elems, 1, 'infix ~ warns once';

@w = warnings({ $s = C.^lookup("m").Str });
is $s, 'm', 'Method.Str answers the name';
like @w[0], /^ 'Method object coerced to string'/, '... and warns naming Method';
