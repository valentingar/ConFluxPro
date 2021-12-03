test_that("chunk_lapply works as normal lapply", {

  df <- data.frame(a = c(1:10),
                   b = LETTERS[c(1:10)]
  )

  t_a <-
  chunk_lapply(c(1:100),
               function(a){a+1})
  t_b <-
  lapply(c(1:100),
         function(a){a+1})
  t_c <-
  chunk_lapply(c(1:3),
               function(a){df})
  t_d <-
  lapply(c(1:3),
         function(a){df})

  expect_equal(t_a,t_b)
  expect_equal(t_c,t_d)

})


test_that("chunk_lapply works with extra function",
          {

          t_a <- chunk_lapply(X = c(1:100),
                              FUN = function(a){a+1},
                              fun_process = function(a) mean(unlist(a)),
                              n_per_chunk = 10)

          expect_equal(t_a,
                       seq(6.5, 96.5, by = 10))

          })
