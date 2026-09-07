use NeedOwnImports::Provider;

unit class NeedOwnImports::UsedConsumer;

method go() { provided(1) }

sub used-go() is export { provided(2) }
