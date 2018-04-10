[![Build Status](https://travis-ci.org/vspinu/mlvocab.svg?branch=master)](https://travis-ci.org/vspinu/mlvocab) [![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/mlvocab)](https://cran.r-project.org/package=mlvocab) [![Development version](https://img.shields.io/badge/devel-0.0.1.9000-orange.svg)](https://github.com/vspinu/mlvocab) [![CRAN version](http://www.r-pkg.org/badges/version/mlvocab)](https://cran.r-project.org/package=mlvocab)

## Corpus and Vocabulary Preprocessing Utilities for Natural Language Pipelines (an R package)

The following two-step abstraction is provided by the `mlvocab` package. 

  - First, the vocabulary object is built from the entire corpus with the help of `vocab()``, `vocab_update()` and `vocab_prune()` functions. 
  - Second, the vocabulary is passed alongside the corpus to a variety of corpus pre-processing functions. Most of the `mlvocab` functions accept `unknown_buckets` argument for partial or full hashing of the corpus.

Current functionality includes:

 - _term index sequences_: `tiseq()` and `timat()` produce integer sequences suitable for direct consumption by various sequence models.
 - _term matrices_: `dtm()`, `tdm()` and `tcm()` create document-term term-document and term-co-occurrence matrices respectively.
 - _vocabulary embedding_: given pre-trained word-vectors `vocab_embed()` creates smaller embedding matrices treating missing and unknown vocabulary terms with grace.
 - _tfidf weighting_: `tfidf()` computes various versions of term frequency, inverse document frequency weighting of `dtm` and `tdm` matrices.
 
 
## Stability

Package is in alpha state. API changes are likely.
