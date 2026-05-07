# Hash operations - insert, lookup, delete
my $checksum = 0;

# Insert
my %h;
for 1..10000 -> $i {
    %h{"key-$i"} = $i;
}
$checksum += %h.elems;

# Lookup
for 1..10000 -> $i {
    $checksum += %h{"key-$i"};
}

# Keys and values
$checksum += %h.keys.elems;
$checksum += %h.values.elems;

# Delete
for 1..5000 -> $i {
    %h{"key-$i"}:delete;
}
$checksum += %h.elems;

say "checksum = $checksum";
