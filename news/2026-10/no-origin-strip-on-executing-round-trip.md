# The executing round trip no longer walks the unit to strip term origins

`expr_origins::strip` took the origin wrappers off a unit before its conversion, with two full walks of the tree. An executing parse never records term origins (`term_origins::Wanted` is entered only by `Str.AST`), so the walks found nothing. The executing round trip skips them; `Str.AST` still strips.

Frontend Ir against the frozen baseline: zef-cli 104.8%, s32-exceptions-misc 102.7%, s12-class-attributes 103.3%, s15-nfg-emoji-test 100.8% (was 102.1%), grammar-cursor-attributes 109.0% (was 109.3%).

Measured and not worth a change: carrying an unresolved bareword through unchanged (no measurable difference, the cost is the classification) and folding `qualify_type_names` into the carried walk (the ordinary parse paid for it too, so it is not part of the excess over the baseline).
