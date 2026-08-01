module RouteBlockDsl {
    # The Cro::HTTP::Router shape: module-body `our` initialized by a sub that
    # news a class declared later, read back from a nested class's method.
    our $plugin = reg('link');

    class Key {
        has Str $.id is required;
    }

    class Runner {
        method !inner() {
            describe($plugin, 'cfg')
        }
        method go() {
            self!inner
        }
    }

    sub reg(Str $id) {
        RouteBlockDsl::Key.new(:$id)
    }

    sub describe(RouteBlockDsl::Key $key, $config) {
        "got:" ~ $key.id ~ ":" ~ $config
    }

    # A bare `multi` (no `sub` keyword) DSL entry point, as Cro::HTTP::Router's
    # `multi route(&route-definition, Str :$name) is export`.
    multi dsl-run(&definition, Str :$name) is export {
        definition();
        "dsl:" ~ ($name // 'anon') ~ ":" ~ RouteBlockDsl::Runner.new.go
    }
}
