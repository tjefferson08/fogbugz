.PHONY: unit feature test

unit:
	cask exec ert-runner

feature:
	cask exec ecukes

test: unit feature

# Local Variables:
# indent-tabs-mode: t
