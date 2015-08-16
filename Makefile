.PHONY: unit feature test

unit:
	cask exec ert-runner -l fogbugz.el

feature:
	cask exec ecukes

test: unit feature

# Local Variables:
# indent-tabs-mode: t
