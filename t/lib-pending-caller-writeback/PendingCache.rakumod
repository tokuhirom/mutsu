# A module-scope cache written from a `Lock.protect` block, the shape of
# Monad's type cache (`Monad::Maybe`'s `publish-type`).
unit module PendingCache;

my $lock  = Lock.new;
my $cache = Map.new;

sub absent(Mu \candidate --> Bool) {
    candidate =:= Nil || candidate =:= Any;
}

sub publish(Str \key, &build --> Mu) {
    $lock.protect({
        my \raced = $cache{key};
        if absent(raced) {
            my \built = build();
            my %next  = $cache;
            %next{key} = built;
            $cache = %next.Map;
            built;
        }
        else {
            raced;
        }
    });
}

sub cached(Mu \T) is export {
    my \key = T.WHICH ~ '|cached';
    my \hit = $cache{key};
    absent(hit) ?? publish(key, { T.^name }) !! hit;
}
