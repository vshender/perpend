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

The graph of the facts that a provider printed: a line per module in manifest
order with the modules that it depends on, then the files without a single
module, then the numbers of unlisted packages and unresolved imports, then the
summary.

  $ cat > facts.jsonl <<EOF
  > {"type":"provider","protocol":"perpend.facts/1","name":"py-grimp","version":"0.1","languages":["python"],"capabilities":["file_imports","package_imports","unresolved"]}
  > {"type":"fact","subject":"src/ui/b.py","object":{"kind":"file","id":"src/core/a.py"},"precision":"syntactic","names":[],"flags":[],"evidence":{"line":1}}
  > {"type":"fact","subject":"src/ui/b.py","object":{"kind":"file","id":"src/core/a.py"},"precision":"syntactic","names":[],"flags":[],"evidence":{"line":2}}
  > {"type":"fact","subject":"src/core/a.py","object":{"kind":"package","id":"sqlalchemy"},"precision":"syntactic","names":[],"flags":[],"evidence":{"line":3}}
  > {"type":"fact","subject":"src/core/a.py","object":{"kind":"package","id":"requests"},"precision":"syntactic","names":[],"flags":[],"evidence":{"line":4}}
  > {"type":"fact","subject":"src/ui/b.py","object":{"kind":"file","id":"src/ui/tests/a.css"},"precision":"syntactic","names":[],"flags":[],"evidence":{"line":5}}
  > {"type":"fact","subject":"setup.py","object":{"kind":"file","id":"src/core/a.py"},"precision":"syntactic","names":[],"flags":[],"evidence":{"line":6}}
  > {"type":"unresolved","subject":"src/ui/b.py","target":"importlib.import_module(...)","evidence":{"line":7}}
  > EOF
  $ perpend graph --facts facts.jsonl
  core
    -> orm (1 fact)
  ui
    -> core (2 facts)
  styles
  tests
  orm
  
  unattributed (1 file)
    setup.py
  
  ambiguous (1 file)
    src/ui/tests/a.css: **/*.css (styles), **/tests/** (tests)
  
  unlisted packages: 1
  unresolved imports: 1
  
  5 modules, 2 edges, 3 facts

A hidden module is not in the report, and neither are the edges from it and
to it.

  $ perpend graph --facts facts.jsonl --hide orm --hide ui
  core
  styles
  tests
  
  unattributed (1 file)
    setup.py
  
  ambiguous (1 file)
    src/ui/tests/a.css: **/*.css (styles), **/tests/** (tests)
  
  unlisted packages: 1
  unresolved imports: 1
  
  3 modules (2 hidden), 0 edges, 0 facts

Another manifest gives another graph of the same facts.

  $ perpend graph --manifest alt.yaml --facts facts.jsonl
  src
  
  unattributed (1 file)
    setup.py
  
  ambiguous (0 files)
  
  unlisted packages: 2
  unresolved imports: 1
  
  1 module, 0 edges, 0 facts

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

  $ perpend graph --facts missing.jsonl
  perpend: missing.jsonl: No such file or directory
  [1]
  $ printf '{"type":"file","path":"a.py"}\n' > broken.jsonl
  $ perpend graph --facts broken.jsonl
  perpend: broken.jsonl: line 1: provider header expected
  [1]
  $ perpend graph --facts facts.jsonl --hide docs
  perpend: no module is 'docs' or under it
  [1]
  $ perpend graph --facts facts.jsonl --hide orm,tests
  perpend: no module is 'orm,tests' or under it
  [1]
  $ perpend graph --facts facts.jsonl --hide a//b
  perpend: invalid module id 'a//b': empty segment
  [1]

When git fails, its own message passes through before ours.

  $ GIT_DIR=none perpend files
  fatal: not a git repository: 'none'
  perpend: git ls-files exited with code 128
  [1]

An error in the command line itself is cmdliner's, with its exit code 124.

  $ perpend which 2>/dev/null
  [124]
