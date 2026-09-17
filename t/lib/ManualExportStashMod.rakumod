unit module ManualExportStashMod;

# The "manual EXPORT stash" idiom: declaring routines (including a custom
# operator) directly inside a module's own `EXPORT::<tag>` package makes them
# part of that tag's export list, whether or not they also carry an explicit
# `is export` trait. `Net::IP::Parse` (fez) uses exactly this shape for its
# `infix:<< ip== >>` operator (see #7988).
my package EXPORT::DEFAULT {
    our sub greeting($name) {
        return "hi $name";
    }

    our sub infix:<< stash-eq >> ($lhs, $rhs --> Bool:D) {
        return $lhs == $rhs;
    }
}
