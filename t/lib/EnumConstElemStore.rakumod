# Support module for t/collections/enum-constant-not-aliased-by-element-store.t
unit module EnumConstElemStore;

enum Edge is export <E_BOTH E_LEFT E_RIGHT E_NONE>;

class Cell is export {
    has Edge @.edges;

    # The store happens in a closure, where the imported enum members are read
    # through the closure's capture.
    method made(Int $kind) {
        my @r = (0, 1).map(-> $i {
            my $obj = Cell.new;
            if $kind == 0 { $obj.edges[0] = E_RIGHT } else { $obj.edges[0] = E_LEFT }
            $obj
        });
        @r[1]
    }

    # A later store into the slot must replace the slot's value only.
    method flip {
        @!edges[0] = do given @!edges[0] {
            when $_ eq E_RIGHT { E_LEFT }
            when $_ eq E_LEFT { E_RIGHT }
            default { $_ }
        }
    }
}

sub members is export { (E_BOTH, E_LEFT, E_RIGHT, E_NONE).map(*.key).join(',') }
