PACKAGE_BASENAME = beginend

export EMACS_VERSION?=26.1

PACKAGE_TEST_ARCHIVES=gnu melpa
PACKAGE_TEST_DEPS=assess buttercup

CURL = curl --fail --silent --show-error --insecure --location --retry 9 --retry-delay 9
GITHUB = https://raw.githubusercontent.com

export CI=false

EMAKE_SHA1=3caabb0b5b2b0f42d242a18642d3d5d8b2320012

emake.mk:
	$(CURL) -O ${GITHUB}/vermiculus/emake.el/${EMAKE_SHA1}/emake.mk

# Include emake.mk if present
-include emake.mk

.PHONY: check lint test

check: lint test

# Run checkdoc and package-lint on test files too. I can't run compile
# on test files because of
# https://github.com/vermiculus/emake.el/issues/23
lint-checkdoc: PACKAGE_LISP += ${PACKAGE_TESTS}
lint-package-lint: PACKAGE_LISP += $(filter-out test/test-helper.el, ${PACKAGE_TESTS})

lint: lint-checkdoc lint-package-lint compile

test: test-buttercup
