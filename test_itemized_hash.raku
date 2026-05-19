my %x = (contents => ['initial']);
my @f = [$%x];

# Modify the hash through the array element
@f[0]<contents>.push("hello");

# In Raku, this should print: [initial hello]
# Because $%x should be a Scalar container holding a reference to %x
say %x<contents>;
