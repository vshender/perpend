The fixture is a repository with a manifest and a few files.  The files must
be in the git index, as they would be in a real repository.  The manifest has
an external module too, which claims no file.  Git must not read the
configuration of the machine: a global exclude file could hide fixture files.

  $ export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_SYSTEM=/dev/null
  $ git init -q -b main
  $ git add -A

A section per module in manifest order with its files in git order, then the
unattributed files, then the ambiguous ones with their candidates, then the
summary.

  $ perpend files
  core (1 file)
    src/core/a.py
  
  ui (0 files)
  
  styles (1 file)
    src/ui/a.css
  
  tests (0 files)
  
  unattributed (2 files)
    README.md
    perpend.yaml
  
  ambiguous (1 file)
    src/ui/tests/a.css: **/*.css (styles), **/tests/** (tests)
  
  5 files, 2 unattributed, 1 ambiguous

One file at a time, with the default manifest or another one.

  $ perpend which src/core/a.py
  core
  $ perpend which ./src/core/a.py
  core
  $ perpend which ././src/core/a.py
  core
  $ printf "version: 1\nmodules: {src: {paths: ['src/**']}}\n" > alt.yaml
  $ perpend which --manifest alt.yaml src/core/a.py
  src
  $ perpend which docs/a.md
  unattributed
  $ perpend which src/ui/tests/a.css
  ambiguous: **/*.css (styles), **/tests/** (tests)

Errors go to stderr with exit code 1, and nothing goes to stdout; a command
that succeeds writes nothing to stderr.

  $ perpend which src/core/a.py 2>&1 >/dev/null
  $ perpend which /src/core/a.py 2>/dev/null
  [1]

  $ perpend which --manifest missing.yaml src/core/a.py
  perpend: missing.yaml: No such file or directory
  [1]
  $ perpend which /src/core/a.py
  perpend: invalid path '/src/core/a.py': must not start with '/'
  [1]
  $ perpend which .
  perpend: invalid path '.': '.' is not allowed
  [1]
  $ perpend which ./
  perpend: invalid path './': empty
  [1]
  $ printf 'version: 1\nmodules: {}\n' > broken.yaml
  $ perpend files --manifest broken.yaml
  perpend: broken.yaml: line 2: modules: at least one module expected
  [1]
  $ perpend files --manifest src
  perpend: src: is a directory
  [1]

When git fails, its own message passes through before ours.

  $ GIT_DIR=none perpend files
  fatal: not a git repository: 'none'
  perpend: git ls-files exited with code 128
  [1]

An error in the command line itself is cmdliner's, with its exit code 124.

  $ perpend which 2>/dev/null
  [124]
