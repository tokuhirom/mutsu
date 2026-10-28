# `self.Range::new(...)` from a `class ... is Range`

`Range`'s constructor is not a native `new` row, so the qualified constructor call a Range subclass makes
(`self.Range::new($min, $max, :$excludes-min, :$excludes-max)`) died "No such method 'new' for invocant
of type 'Range'". It now builds the range the way the subclass's own `.new` does, and the result is an
instance of the subclass. Found loading Math::Sequences; the rest of its suite needs #15288 and #15289.
