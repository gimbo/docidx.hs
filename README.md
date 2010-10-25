# docidx.hs

docidx.hs - create an HTML index of your installed Haskell packages

## Overview

`docidx` is a simple tool which creates an HTML index of your
installed Haskell packages, with links to each package's Haddock docs
locally and on Hackage.  While `cabal install` now creates and
maintains an index "by module", sometimes it's nice to have this other
view (and the quick links to Hackage).

`docidx` works by querying your global and user package databases (via
`ghc-pkg`, via the `Cabal` package) for information on all installed
packages.  When `ghc-pkg` expects Haddock docs to be present, their
local paths are included in this information.  Thus `docidx` builds an
index of all installed packages, including version numbers where more
than one version is present, package synopses, links to the local
Haddock docs, and a link to the package on Hackage.

Packages are grouped by first letter, and there's an A-Z index at the
top of the page, which can be easily extended via a config file to
include extra links you think are handy (e.g. to the per-module index,
to local `ghc` documentation, to the Haskell report, etc. - whatever
you want).

## Installation

The tool hasn't been released to hackage yet, so just download the
source from the
[project home page](http://github.com/gimbo/docidx.hs) and
build/install it using the usual `cabal` mechanism.

## Usage

...is very simple:
  
    docidx
    
on its own will write its output (a single HTML page) to `stdout`,
whereas:

    docidx filename
    
will attempt to write its output to the specified file.

Warnings and errors (e.g. if `Cabal` reports that a package has
Haddock docs, but they can't be found/read) go to `stderr`.

I run it every hour from `cron`, personally.

## Configuration

`docidx` will look for the file `config` in the application's as
reported by `System.Directory.getAppUserDataDirectory` (so in Unixy
systems that's `~/.docidx/config`, and in Windows it's probably
`C:/Documents And Settings/user/Application Data/docidx/config`).

An
[example config file](http://github.com/gimbo/docidx.hs/blob/master/examples/config)
is included in the distribution.

At present the only thing which can be configured this way is the list
of extra links in the table of contents.  The following could be
exposed via the config file, but are also quite easy to change within
the program's code:

  * Which package databases to query.  (Maybe a command-line option?)

  * Page title.

  * CSS (currently just references hackage's).

  * Favicon (currently uses the hackage package list bullet point).

  * Other symbols, particularly the "hackage link" arrow (currently
    some Unicode arrow).

## History

Andy Gimblett started the ball rolling with
[a python program](http://gimbo.org.uk/blog/2009/09/23/) to index his
global Haddock directory just by crawling the HTML.  Andy Price then
[ported that to Haskell](http://github.com/andyprice/docidx.hs), and
about a year later Andy Gimblett forked that and pretty much rewrote
it (but keeping some of the HTML processing parts) to query `Cabal`
for the package information.
