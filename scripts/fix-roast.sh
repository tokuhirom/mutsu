#!/bin/bash
set -euo pipefail

while true; do
  echo "=== $(date): Starting new cycle ==="

  git checkout main
  git pull origin main

  # Do NOT run roast-history.sh here — it modifies HISTORY.tsv/SVG
  # which causes merge conflicts on feature branches.
  # Run it manually with --commit when needed.

  # Pick the next failing roast test
  TEST_FILE=$(./scripts/pick-next-roast.sh 2>/dev/null | grep -v '^===' | head -1 | awk '{print $2}') || true

  if [ -z "$TEST_FILE" ]; then
    echo "All tests passing! Nothing to fix."
    break
  fi

  echo "Target: $TEST_FILE"

  BRANCH="fix/$(basename "$TEST_FILE" | sed 's/[^a-zA-Z0-9_-]/-/g')-$(date +%s)"
  git checkout -b "$BRANCH"

  PROMPT="Fix the failing roast test: $TEST_FILE

Steps:
1. Run the test with a timeout and check what is failing.
2. Use raku -e to verify the expected behavior.
3. Use mutsu's --dump-ast and MUTSU_TRACE=1 to investigate the cause.
4. Fix mutsu so the test passes.
5. Run make test and make roast. If there are regressions, fix them.
6. Once fixed, git add && git commit, then create a PR with auto-merge. Check CI status every minute. If CI fails, fix and push again. If CI passes, auto-merge completes and you are done."

  echo start claude
  claude -p --output-format stream-json --verbose \
    --allowedTools \
      "Bash(cargo *)" "Bash(make *)" "Bash(prove *)" \
      "Bash(git *)" "Bash(gh *)" \
      "Bash(raku *)" "Bash(./target/debug/mutsu *)" "Bash(timeout *)" \
      "Read" "Edit" "Write" "Glob" "Grep" "Task" \
    -- "$PROMPT"

  echo stash
  git stash --include-untracked
  git checkout main
  git stash pop || true
  echo "=== Cycle complete ==="
  sleep 5
done
