use Test;

# A comment inside braces is not code: a `.method` written in it does not
# make a hash composer a block.

plan 3;

sub s1(--> Hash) {
    { host => 1,
      # read it with .token。
      token => 2 }
}
is s1()<token>, 2, 'a .method in a comment keeps the hash';

my %h = { a => 1, # see .foo
          b => 2 };
is %h<b>, 2, 'hash assigned from a braced composer with a comment';

my $b = { .say # comment
};
isa-ok $b, Block, 'a real topic call still makes a block';
