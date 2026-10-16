# Date.today, Date.new-from-daycount and DateTime.now are method-table rows

The owner entry of the method table now admits a type object only to a row flagged for one, as the guard step in front
of every row already did, so an instance method's handler never sees a type object (ADR-11276 §9.123). With that in
place `Date.today`, `Date.new-from-daycount` and `DateTime.now` became rows of `Date` and `DateTime`, found for the
type object of the class or of a user subclass, in place of name-matching arms in the interpreter's slow path.
