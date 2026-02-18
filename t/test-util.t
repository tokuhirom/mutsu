use Test;
use Test::Util;

plan 8;

# make-temp-file basics
{
    my $f = make-temp-file;
    ok $f.defined, 'make-temp-file returns a defined value';
    ok $f.Str.chars > 0, 'make-temp-file path is non-empty';
}

# make-temp-file with content
{
    my $f = make-temp-file(:content<hello>);
    is $f.slurp, 'hello', 'make-temp-file :content writes content';
}

# make-temp-path is alias for make-temp-file
{
    my $f = make-temp-path;
    ok $f.defined, 'make-temp-path returns a defined value';
}

# make-temp-dir
{
    my $d = make-temp-dir;
    ok $d.defined, 'make-temp-dir returns a defined value';
    ok $d.Str.chars > 0, 'make-temp-dir path is non-empty';
}

# is-eqv
{
    is-eqv 42, 42, 'is-eqv with equal integers';
    is-eqv (1, 2, 3), (1, 2, 3), 'is-eqv with equal lists';
}
