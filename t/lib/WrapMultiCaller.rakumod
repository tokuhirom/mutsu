unit module WrapMultiCaller;
use WrapMultiFamily;
sub named-call($prompt = "") is export { chat(:$prompt) }
sub positional-call($prompt = "") is export { chat($prompt) }
