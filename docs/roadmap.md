# Roadmap to an MVP

## The MVP in one sentence

On any pull request of a Clojure repository, a reviewer gets the structural report
without cloning anything, writes notes against form paths (alone or with an
LLM), and posts them as a real GitHub review, with no permissions beyond the
ones GitHub already gave them.

## Done when

- The report is available on demand for any PR the reviewer can read.
- Five consecutive PRs of a production application were reviewed from the report, and at least one
  review was posted through `sdiff review`.
- No PR in that run produced a pairing the reviewer had to correct by reading
  the raw diff.

## Where we are

Done: form-level diff, file verdicts, moves, extraction with drift and renamed
locals, rename roll-up across files, text / edn / html output, merge-base
ranges. M2 and M3 below. A local MCP server (stdio, plumcp 0.3.0) with
`structural-diff`, `review-draft` and `post-review`. Tests over atlas PRs
#7, #4 and #2. Posting works: the first real review was posted through the MCP
tool on 2026-10-05.

CI on every PR is dropped from the MVP: reviews are on demand, through the MCP
server. The remote server is the next milestone, replacing M4.

## Milestones

### M1. Pairing gaps that real PRs hit (done)

Forms that lost their identity pair by name, then by similarity (shared
subtrees and token overlap, at least one half): visibility changes, a form
wrapped by a new macro, a definition renamed and rewritten. A single-arity
function that gains arities is diffed against its closest arity. Checked on 25
merges of a production application: every pairing was a real rename or
rewrite.


- Pair top-level forms whose identity changed but whose body survived, by
  shared-subtree similarity. Seen when every call to one registration macro is
  replaced by another with a new name: each shows as a remove plus an add.
- Every change carries its head-side line range in the EDN, including deleted
  forms, anchored to the enclosing form.
- Fixture: an atlas PR or an inline case with a renamed top-level form.

### M2. `review`: notes to a GitHub review

- Input: the EDN report plus a notes file:
  `{:verdict :approve|:request-changes|:comment :summary "…" :notes [{:file … :path [...] :body …}]}`.
- Resolve each note's form path to a head line. A note on a line outside the
  PR's diff hunks goes into the summary body, since GitHub rejects it inline.
- `sdiff review … --dry-run` prints the payload; without it the command shows
  the payload and asks before posting through `gh api …/pulls/N/reviews`.
- Tests: path-to-line resolution and in-hunk / out-of-hunk routing on fixtures.

### M3. GitHub as a source

- `sdiff pr <owner/repo> <num> text|edn|html` reads base and head blobs and the
  merge base through the GitHub API with the caller's `gh` token. No clone.
- `sdiff.git/report` and the new provider share one shape, so every renderer and
  `review` work on both.

### M4 (replaced). Remote MCP server

- The same tools over streamable HTTP, so claude.ai can use them.
- Login through the plumcp fork's auth-server module with a GitHub identity
  provider that keeps each user's GitHub token for the session; tools call the
  API with that token instead of `gh`.
- Client ID Metadata Documents alongside dynamic client registration.

### M4 (original, dropped from the MVP). Report on every PR

- A GitHub Action, first in atlas (public, no secrets), then in private repos.
- Job summary gets the text report, the Checks API gets one annotation per
  changed form on its head line (batched at 50), the HTML page is a run artifact.
- A private consumer repo needs its own workflow file and token, set up by
  its owners.

### M5. Wire the consumers

- atlas `atlas-review-branch`: `semantic.clj` maps hunks to entities through
  form paths from the EDN report instead of line-range overlap.
- Project review skills downstream: the structural report sits beside the
  registry diff.

## After the MVP

- Remote MCP server with GitHub OAuth: `structural-diff(pr-url)` and
  `post-review(pr-url, notes, verdict)`, acting with the user's own grants.
- Cost-based alignment in the style of autochrome, if pairing errors persist.
- Registry contract diff in the semantic layer with editscript (data) and
  deep-diff2 (printing).
- Moves across files, and a public release.

## Open decisions

- Notes format: EDN as above, or also accept a GitHub-style markdown list.
- Whether the MCP server moves into the MVP instead of M4's Action.
