#!/bin/bash
# Validate the layout of the t/ TAP suite against docs/t-directory-layout.md.
#
# Run by `make check-t-layout`, by `make test` and by CI. Enforces the parts a
# machine can check:
#
#   - every directory directly under t/ is either a known category or a support
#     directory (lib/, fixtures/, packages/, the grandfathered lib-* / *-lib)
#   - a test file sits at most two levels below t/ (t/<cat>/<sub>/<name>.t)
#   - a subdirectory of a category is a declared subcategory
#   - no .t file lives under a support directory
#   - test basenames are globally unique across the tree
#   - (once MIGRATED=1) no .t file sits at t/ top level
#
# The last rule is off until the migration PR moves the 3938 flat files, so
# that this script lands and starts guarding the other five first. Flip
# MIGRATED to 1 in that PR.

set -uo pipefail
export LC_ALL=C

MIGRATED=0

# The closed category set from docs/t-directory-layout.md §2. Adding one is a
# change to that document, not a passing edit here.
CATEGORIES="
collections
concurrency
control
exceptions
grammar
io
lang
modules
nativecall
oo
rakuast
regex
routines
tooling
types
vm
"

# Declared subcategories, as '<category>/<subcategory>'. A category may nest one
# level once it passes ~200 files (§2); until then this list is empty.
SUBCATEGORIES="
"

# Non-category directories under t/. lib/, fixtures/ and packages/ are the
# supported homes for fixtures; the lib-* / *-lib forms are grandfathered and
# must not grow (§6).
is_support_dir() {
  case "$1" in
    lib|fixtures|packages) return 0 ;;
    lib-*|*-lib) return 0 ;;
    *) return 1 ;;
  esac
}

is_category() {
  local d="$1" c
  for c in $CATEGORIES; do [ "$d" = "$c" ] && return 0; done
  return 1
}

is_subcategory() {
  local p="$1" s
  for s in $SUBCATEGORIES; do [ "$p" = "$s" ] && return 0; done
  return 1
}

cd "$(dirname "$0")/.." || exit 1

status=0

# --- directories directly under t/ -----------------------------------------
for dir in t/*/; do
  [ -d "$dir" ] || continue
  name="${dir#t/}"; name="${name%/}"
  if is_support_dir "$name" || is_category "$name"; then continue; fi
  echo "t/$name/ is neither a known category nor a support directory." >&2
  echo "    Categories are listed in docs/t-directory-layout.md §2; fixtures go in t/lib or t/fixtures." >&2
  status=1
done

# --- test files -------------------------------------------------------------
top_level=0
while IFS= read -r f; do
  rel="${f#t/}"
  case "$rel" in
    */*/*/*)
      echo "$f is more than two levels below t/; the cap is t/<category>/<subcategory>/<name>.t" >&2
      status=1
      continue
      ;;
  esac

  first="${rel%%/*}"

  if [ "$first" = "$rel" ]; then
    top_level=$((top_level + 1))
    if [ "$MIGRATED" = "1" ]; then
      echo "$f sits at t/ top level; every test belongs in a category directory" >&2
      status=1
    fi
    continue
  fi

  if is_support_dir "$first"; then
    echo "$f lives under the support directory t/$first/, which holds no tests" >&2
    status=1
    continue
  fi

  if ! is_category "$first"; then
    echo "$f is in the unknown category t/$first/ (see docs/t-directory-layout.md §2)" >&2
    status=1
    continue
  fi

  rest="${rel#*/}"
  if [ "$rest" != "${rest%%/*}" ]; then
    sub="${rest%%/*}"
    if ! is_subcategory "$first/$sub"; then
      echo "$f is in the undeclared subcategory t/$first/$sub/; add it to SUBCATEGORIES here" >&2
      status=1
    fi
  fi
done < <(find t -name '*.t' -type f | sort)

# --- basename uniqueness ----------------------------------------------------
# Several tools index tests by basename rather than path (scripts/test-module-
# sweep.sh's flat work directory above all), and every historical news/ and
# docs/ reference names them that way. See docs/t-directory-layout.md §5.
# `sed`, not `find -printf`: -printf is a GNU extension and this script also
# runs on a developer's machine.
dupes=$(find t -name '*.t' -type f | sed 's|.*/||' | sort | uniq -d)
if [ -n "$dupes" ]; then
  echo "duplicate test basenames (they must be globally unique, see docs/t-directory-layout.md §5):" >&2
  while IFS= read -r d; do
    [ -n "$d" ] || continue
    find t -name "$d" -type f | sed 's|^|    |' >&2
  done <<< "$dupes"
  status=1
fi

if [ $status -eq 0 ]; then
  count=$(find t -name '*.t' -type f | wc -l)
  if [ "$top_level" -gt 0 ]; then
    echo "t/ layout OK ($count tests; $top_level still at top level, awaiting the migration PR)"
  else
    echo "t/ layout OK ($count tests, all in category directories)"
  fi
fi
exit $status
