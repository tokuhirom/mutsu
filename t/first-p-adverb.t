use Test;

plan 7;

my @items = 1, 2, 3, 4;

is @items.first(* %% 2, :p), 1 => 2, ".first(:p) returns index/value pair";
is first(* %% 2, @items, :p), 1 => 2, "first(:p) sub form returns index/value pair";
is @items.first(* %% 2, :end, :p), 3 => 4, ".first(:end, :p) keeps original index";
is @items.first(:p), 0 => 1, ".first(:p) without matcher returns first pair";
is @items.first(:end, :p), 3 => 4, ".first(:end, :p) without matcher returns last pair";
is @items.first(* %% 2, :!p), 2, ".first(:!p) returns value";
is first(* %% 2, @items, :!p), 2, "first(:!p) sub form returns value";
