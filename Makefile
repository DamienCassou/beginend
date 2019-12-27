ELPA_DEPENDENCIES=package-lint assess buttercup

ELPA_ARCHIVES=melpa-stable gnu

TEST_BUTTERCUP_OPTIONS=test
LINT_CHECKDOC_FILES=$(wildcard *.el) ${TEST_ERT_FILES}
LINT_PACKAGE_LINT_FILES=$(wildcard *.el) ${TEST_ERT_FILES}
LINT_COMPILE_FILES=$(wildcard *.el) ${TEST_ERT_FILES}

makel.mk:
	# Download makel
	@if [ -f ../makel/makel.mk ]; then \
		ln -s ../makel/makel.mk .; \
	else \
		curl \
		--fail --silent --show-error --insecure --location \
		--retry 9 --retry-delay 9 \
		-O https://gitlab.petton.fr/DamienCassou/makel/raw/v0.6.0/makel.mk; \
	fi

# Include makel.mk if present
-include makel.mk
