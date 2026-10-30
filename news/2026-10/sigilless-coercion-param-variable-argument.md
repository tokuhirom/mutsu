# A sigilless coercion parameter coerces a variable argument

`sub f(Num() \y)` called as `f($k)` with `my $k = 3` bound the caller's variable as a raw alias
and dropped the coerced value, so `y` still held the Int (and `has num` attributes then refused it:
Pop's `Pop::Rect.new: 3` died in `!SET-SELF(Num() \x, Num() \y)`). A coercion-typed sigilless
parameter now binds the coerced value, and an array-element argument (`f(@a[0])`) no longer fails
with "Impossible coercion" because the coercion reads the container's content.
