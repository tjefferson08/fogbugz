.PHONY: unit feature test

unit:
	cask exec ert-runner

feature:
	cask exec ecukes --reporter magnars

test: unit feature

# Local Variables:
# indent-tabs-mode: t
