use Test;

plan 4;

# URI::FetchFile's parameterized Class role keeps a mutable `my $type` static.
# A caller using the same lexical name must not be rebound when the role method
# updates its own static.
role LazyClass[Str $name] {
    my $type;

    method class-name { $name }
    method type { $type = Int; $type }
    method available { not $type === Any }
}

class Provider does LazyClass['HTTP::UserAgent'] { }

my $type = Provider;
is $type.class-name, 'HTTP::UserAgent', 'the parameterized role exposes its argument';
is $type.type, Int, 'the role method updates its own static';
is $type.^name, 'Provider', 'updating a role static does not rebind the caller lexical';
ok $type.available, 'the role static remains visible on the next method call';
