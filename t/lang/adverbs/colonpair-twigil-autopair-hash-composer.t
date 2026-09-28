use Test;

# A block body that starts with a variable autopair composes a Hash whatever
# the variable's twigil: `{ :%*DIRECTIVES, :@*ROWS }` is a Hash in rakudo.
# mutsu's brace classifier only knew the `!` twigil, so a dynamic-variable
# autopair turned the braces into a Block. Log::Reader's grammar actions
# (`make {:%*DIRECTIVES, :@*ROWS}`) returned that Block as the parse result.

plan 8;

my %*D = a => 1;
my @*R = 1, 2;

is {:%*D, :@*R}.^name, 'Hash', '{:%*D, :@*R} is a Hash';
is {:%*D}.^name,       'Hash', '{:%*D} is a Hash';
is {:@*R}.^name,       'Hash', '{:@*R} is a Hash';
is-deeply {:%*D, :@*R}, {D => {a => 1}, R => [1, 2]}, 'dynamic autopairs keep their values';

my $*S = 'x';
is {:$*S}.^name, 'Hash', '{:$*S} is a Hash';

class C {
    has $.pub = 1;
    has $!priv = 2;
    method pub-hash  { {:$.pub} }
    method priv-hash { {:$!priv} }
}
is C.new.pub-hash.^name,  'Hash', '{:$.attr} is a Hash';
is C.new.priv-hash.^name, 'Hash', '{:$!attr} is a Hash';

# A block that merely reads such a variable is still a block.
is { %*D<a> }.^name, 'Block', '{ %*D<a> } stays a Block';
