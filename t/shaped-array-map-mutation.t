use Test;

# `.map(* OP= ...)` / `.map({ $_ OP= ... })` rw-binds `$_` to each element, so
# element mutations must persist to the source array (Raku semantics). For a
# shaped array the shape/structure is preserved — only leaf values change.
# Two bugs fixed:
#  1. The shaped-array map writeback was skipped entirely.
#  2. Fused compound assigns (`+=`/`~=`/etc., via AtomicCompoundVar) did not
#     record the `$_` mutation for the rw-map writeback, so only `*=`/`/=`
#     (whose LHS desugars through a `defined`-ternary, skipping fusion) worked.

plan 12;

# native int shaped, compound *= (ternary-LHS form)
my int @i[4] = 10, 15, 12, 16;
@i.map(* *= 2);
is @i.join(","), "20,30,24,32", "int shaped: map(* *= 2) persists";

# native int shaped, += (fused form — the previously-broken path)
my int @j[3] = 1, 2, 3;
@j.map({ $_ += 10 });
is @j.join(","), "11,12,13", "int shaped: map(\$_ += 10) persists";

# native int shaped, plain assign to $_
my int @k[3] = 1, 2, 3;
@k.map({ $_ = $_ * 100 });
is @k.join(","), "100,200,300", "int shaped: map(\$_ = ...) persists";

# native str shaped, ~= (fused concat-assign)
my str @s[3] = "a", "b", "c";
@s.map({ $_ ~= "x" });
is @s.join(","), "ax,bx,cx", "str shaped: map(\$_ ~= ...) persists";

# shape and element type are preserved
is @i.shape, (4,), "int shaped: shape preserved after map";
ok @i ~~ Array, "int shaped: still an array after map";

# a non-mutating map must NOT change the source
my int @n[3] = 1, 2, 3;
my @r = @n.map({ $_ + 100 });
is @n.join(","), "1,2,3", "non-mutating map leaves shaped source unchanged";
is @r.join(","), "101,102,103", "non-mutating map returns mapped values";

# multi-dim shaped: map over leaves, structure preserved
my @m[2;2] = (1, 2), (3, 4);
@m.map({ $_ += 1 });
is @m[0;0], 2, "multidim shaped: leaf (0,0) mutated";
is @m[1;1], 5, "multidim shaped: leaf (1,1) mutated";
is @m.shape, (2, 2), "multidim shaped: shape preserved";

# ordinary (non-shaped) array still works
my @o = 1, 2, 3;
@o.map({ $_ += 1 });
is @o.join(","), "2,3,4", "non-shaped array: compound-assign map persists";
