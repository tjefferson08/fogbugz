# fogbugz
Some useful fogbugz features via the Fog Creek's [web API](http://help.fogcreek.com/8202/xml-api)

# Quick start
You'll need to set the following variables: `fogbugz-domain`, `fogbugz-email`, and `fogbugz-password`. One (easy) way is to call `M-x fogbugz-prompt-for-credentials`.
Once those variables are set, `M-x fogbugz-logon` should successfully set the all-important variable: `fogbugz-token`.

From there, `M-x fogbugz-list-filters` will drop you into a list of your saved/shared fogbugz filters.

# Contributing
Use [cask](http://cask.readthedocs.org/en/latest/). Contributions are welcome. Unit and feature tests exist in some form (always improving). Run `make test` on your local

