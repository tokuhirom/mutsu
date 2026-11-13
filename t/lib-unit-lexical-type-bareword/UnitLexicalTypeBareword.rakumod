use nqp;

# A module-level lexical class read as a bareword from the module's own subs
# (the shape of Array::Sorted::Util's `NotFound`).
my class NotFound is Int { }

my sub found-or-not($x) is export { nqp::istype($x, NotFound) ?? 'not-found' !! 'found' }

my sub make-not-found(Int $i) is export { nqp::box_i($i, NotFound) }

my sub name-of-not-found() is export { NotFound.^name }

my sub inner-shadow() is export {
    my class NotFound { method tag { 'inner' } }
    NotFound.tag
}
