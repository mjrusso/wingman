EMACS ?= emacs

SOURCE = wingman.el
TESTS = wingman-tests.el

DEPS = compat dash request transient

INIT_PACKAGES := "(progn \
  (require 'package) \
  (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t) \
  (package-initialize) \
  (let ((missing-packages (cl-remove-if 'package-installed-p '(PACKAGES)))) \
    (when missing-packages \
      (package-refresh-contents) \
      (dolist (pkg missing-packages) \
        (condition-case err \
          (package-install pkg) \
          (error (message \"Failed to install %s: %s\" pkg err)))))) \
  )"

.PHONY: test
test:
	$(EMACS) -Q --eval $(subst PACKAGES,$(DEPS),$(INIT_PACKAGES)) -batch -l $(SOURCE) -l $(TESTS) -f ert-run-tests-batch-and-exit


.DEFAULT_GOAL := test
