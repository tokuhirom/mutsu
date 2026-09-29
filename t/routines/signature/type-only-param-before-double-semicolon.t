use Test;

plan 4;

enum D <LOG WARN>;

multi f(LOG;; $x) { "ok" }
is f(LOG, "x"), "ok", 'enum-value literal followed by ;;';

multi g(Int ;; $x) { "int-ok" }
is g(1, "x"), "int-ok", 'type-only param followed by ;; with a space';

multi h(Int; $x) { "semi-ok" }
is h(1, "x"), "semi-ok", 'type-only param followed by ;';

sub k(Str, Int;; $x) { "many" }
is k("a", 1, "x"), "many", 'several type-only params before ;;';
