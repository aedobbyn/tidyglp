
library(Rglpk)
library(tidyverse)
library(assertthat)

# Example from package
obj <- c(2, 4, 3)
mat <- matrix(c(3, 2, 1, 4, 1, 3, 2, 2, 2), nrow = 3)
dir <- c("<=", "<=", "<=")
rhs <- c(60, 40, 80)
max <- TRUE

Rglpk_solve_LP(obj, mat, dir, rhs, max = max)

# In dataframe format
obj_df <- matrix(obj, nrow = 1) %>%
  as_tibble() %>%
  mutate(
    dir = NA,
    rhs = NA
  )

df <- as_tibble(mat) %>%
  mutate(dir = dir,
         rhs = rhs)

full_df <- obj_df %>%
  bind_rows(df)


tidy_glp <- function(tbl, obj_row, dir_col, rhs_col,
                     maximize = FALSE, types = NULL,
                     ...) {

  if (is.null(types)) {
    types <- "continuous"
  }

  assertthat::assert_that(types %in% c("continuous", "integer", "binary"),
                          msg = "types must be continuous, integer, or binary")

  if (types == "integer") {
    types <- rep("I", nrow(tbl))
  } else if (types == "binary") {
    types <- rep("B", nrow(tbl))
  } else if (types == "continuous") {
    types <- rep("C", nrow(tbl))
  }

  matrix_cols <- which(!names(tbl) %in% c(dir_col, rhs_col))

  obj_vec <- tbl[obj_row, matrix_cols]
  tbl <- tbl[-obj_row, ]

  out <- Rglpk_solve_LP(obj = obj_vec,
                        mat = as.matrix(tbl[, matrix_cols]),    # matrix(as_vector(tbl[, matrix_cols]), nrow = nrow(tbl))
                        dir = tbl[[dir_col]],
                        rhs = tbl[[rhs_col]],
                        max = maximize,
                        types = types
                        )

  return(out)
}

tidy_glp(full_df, obj_row = 1, dir_col = "dir", rhs_col = "rhs", maximize = TRUE)







# Using mtcars
df <- as_tibble(mtcars) %>%
  select(1:4) %>%
  slice(1:5) %>%
  mutate(
    dir_obs = c(">", ">", "<=", ">=", "<")
  ) %>%
  select(2:5, 1) %>%   # mpg is rhs
  mutate(
    mpg = mpg + 500,
    obj_obs = 300:304
  )

tidy_glp(df, obj_col = "obj_obs", dir_col = "dir_obs", rhs_col = "mpg")

