[private]
@default: help

@help:
  echo "Usage: just <recipe>"
  echo ""
  just --list

build:
    emacs -Q --batch -L . -f batch-byte-compile key-layout-mapper.el
    emacs -Q --batch -L . -f batch-byte-compile key-layout-mapper-test.el

test: build
    emacs -Q --batch -L . -l motion-tests.el -l ert --eval "(ert-run-tests-batch-and-exit)"
    emacs -Q --batch -L . -l motion-tests.elc -l ert --eval "(ert-run-tests-batch-and-exit)"
