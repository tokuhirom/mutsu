use Test;

plan 2;

# A grammar may declare more than one proto regex.  The `regex` slang belongs
# to the proto declarator, so each declaration must register its own name
# rather than being parsed as a proto sub named `regex`.
grammar MultipleProtoRegex {
    regex TOP { <name> '=' <number> }
    proto regex name {*}
    regex name:sym<word> { <[a..z]>+ }
    proto regex number {*}
    regex number:sym<digits> { <[0..9]>+ }
}

my $match = MultipleProtoRegex.parse('abc=123');
ok $match, 'multiple proto regex declarations parse';
is $match<name>.Str ~ ':' ~ $match<number>.Str, 'abc:123',
    'each proto regex dispatches its own candidates';
