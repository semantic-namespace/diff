# diff

Structural review of Clojure changes. Instead of lines, a change is named by the
form it lives in:

```
■ src/malli/error.cljc   [semantic]
  defn ^:no-doc -resolve-root-error
    ~ body › body › body › binding [path' m' p'] › let 1 restructured let → when-let;
      kept: (when-let [m' (error-message {:schema schema} options)] …, [schema (mu/get-in schema path)]
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

The fixtures are the changed files of three merged
[metosin/malli](https://github.com/metosin/malli) pull requests at merge base
and head (`test/fixtures/malli.refs`).

## Scope

This layer is syntactic. It knows nothing about what a form means in a given
codebase; a registry or ontology layer can take the EDN report and attach that.
