# diff

Structural review of Clojure changes. Instead of lines, a change is named by the
form it lives in:

```
■ core/src/atlas/registry.cljc   [semantic]
  defn compile!
    ~ arity 1 › let 2 › binding dev-id-conflicts  [extracted → dev-id-conflicts]
  defn dev-id-conflicts   ⇠ extracted from defn compile! › arity 1 › let 2 › binding dev-id-conflicts
    + new form
      drift: replaced keep 3 › fn › body › body › test  - (next cids)  + (and dev-id (some …
      drift: replaced keep 3 › fn › body › body › body 1 › key :winner  - (last cids)  + (conj …
      drift: added sort-by 4  + (sort-by str)
```

Each file gets one verdict: `semantic`, `rename-only`, `comments-only` or
`whitespace-only`. Inside a semantic file, every change is a path through the
code (binding, clause, argument, body step) with the old and new expression,
the sub-expressions that survived, moves between slots, and extractions of an
existing expression into a new function, including what drifted on the way and
which locals were renamed.

## Use

Babashka, no install beyond the repo:

```
bb sdiff text <repo> <base> <head>
bb sdiff edn  <repo> <base> <head>
bb sdiff html <repo> <out.html> <github-url|-> (<base> <head> <num> <title>)+
```

Ranges are diffed from the merge base of `base` and `head`, which is what a
pull request shows. For a squash-merged commit `C`, pass `C^ C`.

`edn` is the contract for other tools: the report map with every node as
`{:src :row :col :end-row :end-col}`, so a consumer can print a form, anchor a
review comment to a head-side line, or re-read the source. `html` is a
self-contained page with the whole form shown and the changes marked.

As a library, `sdiff.core/file-report` takes a path and two source strings,
`sdiff.git/report` takes a repo and two refs, `sdiff.edn/report->edn` makes the
result plain data.

## Tests

```
bb test
```

The fixtures are the changed Clojure files of three merged
[semantic-namespace/atlas](https://github.com/semantic-namespace/atlas) pull
requests at merge base and head (`test/fixtures/atlas.refs`): #7 for an
extraction with drift, #4 for a move into a binding and a dropped reader
conditional, #2 for renames rolled up across files. Behaviours no atlas PR
exercises yet are covered by small inline sources in the test.

## Scope

This layer is syntactic. It knows nothing about what a form means in a given
codebase; a registry or ontology layer can take the EDN report and attach that.
