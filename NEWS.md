Version 0.1.2
=============

* Number of threads can now be controlled with `options("mlvocab.nthreads")` or environment variable `MLVOCAB_NTHREADS`
* New argument `keep_terms` to `prune_vocab`

Version 0.1
===========

* Automatically sort vocabs in decreasing term frequency order
* [#3](https://github.com/vspinu/mlvocab/issues/3) Add support for character vector corpus.
* Add support for `data.frame` corpus
* New index generator function `tix_df` to produce flat indices as `data.frames` with provision for multi-keys for documents.
* Rename `tixyz` function into `tix_xyz`.
* Rename `vocab_embed` -> `prune_embeddings`, `vocab_prune` -> `prune_vocab`, `vocab_update` -> `update_vocab`.


Version 0.0.1
=============

Initial Release
