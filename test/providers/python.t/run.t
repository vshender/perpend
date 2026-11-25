The fixture project of the Python provider holds the facts that the provider
must print for it, `facts.jsonl`, and a manifest, `perpend.yaml`, which groups
its files into modules.  The tests of the provider check that its output is
equal to that file.  This test checks the file itself: `perpend graph` reads
facts with the reader of `lib/facts`, which accepts only what the protocol
defines, so a record that breaks the protocol is an error here.

  $ cd ../../../providers/python/tests/fixture
  $ perpend graph --facts facts.jsonl
  app
  core
  web
    -> core (4 facts)
    -> plugins (2 facts)
    -> helpers (2 facts)
  plugins
    -> core (1 fact)
  helpers
  broken
  
  unattributed (0 files)
  
  ambiguous (0 files)
  
  unlisted packages: 6
  unresolved imports: 3
  
  6 modules, 4 edges, 9 facts
