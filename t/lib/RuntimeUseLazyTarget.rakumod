use RuntimeUseLazyStubRole;
unit class RuntimeUseLazyTarget does RuntimeUseLazyStubRole;
use RuntimeUseLazyTrait;

has $.x is runtime-use-lazy;

method build-x { 42 }
