# ADR-9761: User subclasses of RakuAST nodes retain the native parent node

- **Status**: Proposed (2026-10-22).
- **Context**: [#9761](https://github.com/tokuhirom/mutsu/issues/9761),
  [ADR-0011](0011-rakuast-model-layer-and-phasing.md), and
  [ADR-10723](10723-rakuast-is-the-frontend-ir.md).

## Problem

A user class can inherit a `RakuAST::*` class and receive the right MRO, but its
instance is an ordinary `Value::Instance`. The native parent constructor builds
a `Value::RakuAst`, which the instance previously discarded. The subclass then
loses the parent's fields, visitor, builders, rendering, and `EVAL` behavior.

## Decision

Keep the user class as an `Instance`, including its own attributes and methods.
When its constructor reaches a native RakuAST ancestor, construct that node and
attach it to the instance's shared side storage. The side storage is traced by
the existing instance GC path and shared by aliases of the instance. A clone of
the instance clones its node value, matching the existing clone behavior of
native RakuAST values.

The native node remains the single implementation of its model fields and
mutators. Accessors, visitor rows, builders, rendering, and `EVAL` consult the
attached node after the subclass's own methods have had precedence. Lowering
still feeds the existing compiler and VM; it does not execute a second AST
interpreter.

This representation follows ADR-0011's dedicated node variant without
registering a separate `Value` variant for each user subclass. It is also
compatible with ADR-10723's proposed frontend direction: a subclass is a user
object that owns a native node, regardless of which frontend built the node.

## Consequences

The parent node must be visible to the instance's GC traversal, and every
native RakuAST method family must resolve through the subclass MRO while
preserving user overrides. Constructor and mutator tests must check both the
user object's identity and the inherited node's behavior.
