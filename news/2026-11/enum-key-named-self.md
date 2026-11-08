# An imported enum key named `self` no longer replaces a method's `self`

`enum Sector is export <self child parent>` (Qwiratry's `WalkCursor`) made the
importing unit declare a term `self`, which a method of that unit read instead of
its invocant: `self.new` died with "Enum 'Sector' is insufficiently type-like to be
instantiated". The parse-time scan of a module's exports no longer lists a key
spelled `self` as a term; `Sector::self` stays reachable. (#17197)
