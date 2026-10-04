use Test;

plan 8;

# A parametric role whose parameters all have defaults puns to its default
# curry, which is named after the role and is a different type from an
# explicit `E[Any]` (#11535).
role E[::R = Any] { method r { R.^name } }

is E.new.^roles.map(*.^name).join(","), "E", "default pun composes the bare role";
nok E.new ~~ E[Any], "default pun does not match explicit E[Any]";
ok E.new ~~ E, "default pun matches the role itself";
is E.new.r, "Any", "default parameter still bound";
is E[Any].new.^roles.map(*.^name).join(","), "E[Any]", "explicit pun keeps E[Any]";
ok E[Any].new ~~ E[Any], "explicit E[Any] pun matches E[Any]";
ok E[Int].new ~~ E[Int], "explicit E[Int] pun matches E[Int]";
is E.new.^name, "E", "default pun is named after the role";
