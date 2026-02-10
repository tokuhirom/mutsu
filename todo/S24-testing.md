# S24 - Testing

Reference: `old-design-docs/S24-testing.pod`

Covers the standard test module and TAP output.

---

## Test Plans

- [x] `plan($count)` - declare number of tests
- [x] `done-testing` - end without predetermined count

---

## Basic Assertions

- [x] `ok($condition, $description)` - boolean true
- [x] `nok($condition, $description)` - boolean false
- [x] `pass($description)` - unconditional pass
- [x] `flunk($description)` - unconditional fail

---

## Comparison Tests

- [x] `is($got, $expected, $description)` - equality (using `eq`)
- [x] `isnt($got, $expected, $description)` - inequality
- [x] `cmp-ok($got, $op, $expected, $description)` - operator comparison
- [x] `is-deeply($got, $expected, $description)` - structural equality (eqv)
- [x] `is-approx($got, $expected, $description)` - numeric tolerance (1e-5)

---

## Type Tests

- [x] `isa-ok($obj, $type, $description)` - type membership
- [x] `does-ok($obj, $role, $description)` - role check
- [x] `can-ok($obj, $method, $description)` - method existence

---

## Pattern Tests

- [x] `like($got, $pattern, $description)` - regex match
- [x] `unlike($got, $pattern, $description)` - regex non-match

---

## Exception Tests

- [x] `dies-ok(&code, $description)` - code throws
- [x] `lives-ok(&code, $description)` - code doesn't throw
- [x] `eval-lives-ok($string, $description)` - eval doesn't throw
- [x] `throws-like(&code, $type, $description)` - throws specific type

---

## Grouping

- [x] `subtest($description, &code)` - grouped tests

---

## Test Control

- [x] `skip($reason, $count)` - skip tests
- [x] `skip-rest($reason)` - skip remaining
- [x] `todo($reason, $count)` - mark as TODO
- [x] `bail-out($reason)` - abort testing

---

## Diagnostics

- [x] `diag($message)` - print diagnostic to stderr

---

## Missing / Not in Spec but Useful

- [ ] `use Test` proper module loading (currently built-in)
- [ ] TAP version 13 support
- [ ] Nested TAP for subtests
