use Test;

plan 4;

# Resolving the declaration first fills the base-name index before export
# aliases are installed. Later qualified calls must see those new keys.
module ExportAliasCacheSingle {
    sub answer is export { 42 }
}
is ExportAliasCacheSingle::EXPORT::DEFAULT::answer(), 42,
    'DEFAULT alias is visible after the source sub was registered';
is ExportAliasCacheSingle::EXPORT::ALL::answer(), 42,
    'ALL alias is visible after the source sub was registered';

module ExportAliasCacheMulti {
    proto sub add-one(|) is export {*}
    multi sub add-one(Int $value) { $value + 1 }
}
is ExportAliasCacheMulti::EXPORT::DEFAULT::add-one(4), 5,
    'DEFAULT alias sees a multi candidate registered after its proto';
is ExportAliasCacheMulti::EXPORT::ALL::add-one(4), 5,
    'ALL alias sees a multi candidate registered after its proto';
