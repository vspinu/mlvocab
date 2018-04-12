context("embeddings")

corpus <- list(a = c("The", "quick", "brown", "fox", "jumps", "over", "the",
                     "lazy", "dog"), 
               b = c("the", "quick", "brown", "fox", "jumps", "over", "the", "lazy",
                     "dog", "the", "quick", "brown", "fox", "jumps", "over", "the", "lazy",
                     "dog"))


test_that("average embedding works", {

    embs <- matrix(rep(seq_along(letters), each = 10), nrow = 10, dimnames = list(NULL, letters))
    N <- 8
    vocab <- vocab(letters[1:N])

    ## simple
    expect_equal(vocab_embed(vocab, embs), embs[, 1:N])

    ## with buckets
    ev <- vocab_embed(vocab, embs, nbuckets = 10, max_in_bucket = 0)
    expect_true(all(ev[, -c(1:N)] == 0))

    ev <- vocab_embed(vocab, embs, nbuckets = 10, max_in_bucket = 1)
    tev <- ev[, -c(1:N)]
    expect_true(all(tev - floor(tev) == 0))

    ev <- vocab_embed(vocab, embs, nbuckets = 10, max_in_bucket = 2)
    tev <- ev[, -c(1:N)]
    expect_equal(unique(c(tev - floor(tev))), c(0.0, 0.5))

    ev <- vocab_embed(vocab, embs, nbuckets = 10, max_in_bucket = 3)
    tev <- ev[, -c(1:N)]
    expect_equal(unique(c(tev - floor(tev))), c(0, 0.5, 1/3, 2/3))
    expect_equal(ev[, 1:N], embs[, 1:N])

    vocab2 <- vocab(tail(letters, N))
    expect_equal(vocab_embed(vocab2, embs),
                 embs[, (ncol(embs) - N + 1):ncol(embs)])

    ev <- vocab_embed(vocab2, embs, nbuckets = 10, max_in_bucket = 3)
    tev <- ev[, -c(1:N)]
    expect_equal(unique(c(tev - floor(tev))), c(0, 0.5, 1/3, 2/3))
    expect_equal(ev[, 1:N], embs[, (ncol(embs) - N + 1):ncol(embs)])

    expect_equal(dim(vocab_embed(vocab2, embs, nbuckets = 2, max_in_bucket = 100)),
                 c(10, N + 2))
})

test_that("average embedding works with missing values", {
    embs <- matrix(rep(seq_along(letters), each = 10), nrow = 10, dimnames = list(NULL, letters))
    N <- 8
    vocab <- vocab(c("a", "b", "dd", "ee", "h"))
    ## one bucket is used
    e <- vocab_embed(vocab, embs, max_in_bucket = 100)
    expect_equal(e[, "dd"], e[, "ee"])
    ## two buckets are used
    e <- vocab_embed(vocab, embs, max_in_bucket = 10)
    expect_true(!all(e[, "dd"] == e[, "ee"]))
})

