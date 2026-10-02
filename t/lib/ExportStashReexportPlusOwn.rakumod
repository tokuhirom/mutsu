# From Test::Coverage: re-export a dependency's whole EXPORT::DEFAULT stash
# while also declaring its own `is export` subs.
use Test;

my sub own-plain() is export { 'plain' }
my sub own-asserting() is export is test-assertion { 'asserting' }

BEGIN EXPORT::DEFAULT::{.key} := .value for Test::EXPORT::DEFAULT::;
