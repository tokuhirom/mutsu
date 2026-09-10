use Test;

plan 5;

our $output = '';
sub capture($code) {
    temp our $output = '';
    my $*OUT = class { method print(*@args) { $output ~= @args.join } }
    $code();
    $output
}

role TaggedStr {
    method Str() { "tagged:{self.Numeric}" }
}

my $mixed = 5 but TaggedStr;
is $mixed.Str, 'tagged:5', 'a role-mixed native value dispatches Str to the role';
is capture({ say $mixed }), "tagged:5\n", 'say dispatches Str through inherited gist';
is capture({ say [$mixed] }), "[tagged:5]\n", 'collection output dispatches a mixin element';

my $custom-gist = 7 but role :: { method gist() { "GISTED" } };
is capture({ say [$custom-gist] }), "[GISTED]\n", 'collection output honors a mixin gist';

my %seen of Int is default(0 but role :: { method Str() { "NULL" } });
is capture({ say %seen<not-there> }), "NULL\n", 'a mixed Hash default renders through its role Str';
