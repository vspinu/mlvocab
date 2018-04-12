[![Build Status](https://travis-ci.org/vspinu/mlvocab.svg?branch=master)](https://travis-ci.org/vspinu/mlvocab) [![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/mlvocab)](https://cran.r-project.org/package=mlvocab) [![CRAN version](http://www.r-pkg.org/badges/version/mlvocab)](https://cran.r-project.org/package=mlvocab)

## Corpus and Vocabulary Preprocessing Utilities for Natural Language Pipelines (an R package)

The following two-step abstraction is provided by the package:

  1. The vocabulary object is first built from the entire corpus with the help of `vocab()`, `vocab_update()` and `vocab_prune()` functions. 
  2. Then, the vocabulary is passed alongside the corpus to a variety of corpus pre-processing functions. Most of the `mlvocab` functions accept `nbuckets` argument for partial or full hashing of the corpus.

Current functionality includes:

 - __term index sequences__: `tiseq()` and `timat()` produce integer sequences suitable for direct consumption by various sequence models.
 - __term matrices__: `dtm()`, `tdm()` and `tcm()` create document-term term-document and term-co-occurrence matrices respectively.
 - __vocabulary embedding__: given pre-trained word-vectors `vocab_embed()` creates smaller embedding matrices treating missing and unknown vocabulary terms with grace.
 - __tfidf weighting__: `tfidf()` computes various versions of term frequency, inverse document frequency weighting of `dtm` and `tdm` matrices.
 
 
## Stability

Package is in alpha state. API changes are likely.
