module SeenUtil {
    my Lock $lock .= new;
    my %seen{Any};

    proto sub ast-of(|) is export {*};
    multi sub ast-of(Str:D $s) { $s }
    multi sub ast-of(Hash:D $h) {
        my $init;
        my $dict = $lock.protect: {
            %seen{$h} //= do { $init = True; %() }
        }
        if $init {
            LEAVE $lock.protect: { %seen{$h}:delete }
            $dict{.key} = ast-of(.value) for $h.pairs;
        }
        $dict;
    }
    multi sub ast-of(List:D $l) {
        my $init;
        my $arr = $lock.protect: {
            %seen{$l} //= do { $init = True; [] }
        }
        if $init {
            LEAVE $lock.protect: { %seen{$l}:delete }
            $arr.push(ast-of($_)) for $l.list;
        }
        $arr;
    }
}
