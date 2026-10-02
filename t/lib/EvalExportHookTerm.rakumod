# The term names this module exports are the `use` arguments themselves, so
# only running `sub EXPORT` can tell an importer what they are (#11062).
sub EXPORT(*@names --> Map()) {
    class Box { has $.k; method new($k) { self.bless(k => $k) } }
    my %h;
    for @names.kv -> $i, $name {
        %h{$name} = Box.new($i + 1);
    }
    %h
}
