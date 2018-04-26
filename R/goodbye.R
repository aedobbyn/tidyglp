
library(Rglpk)
library(tidyverse)

# Example from package
obj <- c(2, 4, 3)
mat <- matrix(c(3, 2, 1, 4, 1, 3, 2, 2, 2), nrow = 3)
dir <- c("<=", "<=", "<=")
rhs <- c(60, 40, 80)
max <- TRUE

Rglpk_solve_LP(obj, mat, dir, rhs, max = max)

# In dataframe format
df <- mat %>%
  as_tibble() %>%
  mutate(dir = dir,
         rhs = rhs,
         obj = obj)


tidy_glp <- function(tbl, obj_col, dir_col, rhs_col, maximize = FALSE, ...) {

  matrix_cols <- which(!names(tbl) %in% c(obj_col, dir_col, rhs_col))

  out <- Rglpk_solve_LP(obj = tbl[[obj_col]],
                        mat = as.matrix(tbl[, matrix_cols]),    # matrix(as_vector(tbl[, matrix_cols]), nrow = nrow(tbl))
                        dir = tbl[[dir_col]],
                        rhs = tbl[[rhs_col]],
                        max = maximize
                        )

  return(out)
}

tidy_glp(df, obj_col = "obj", dir_col = "dir", rhs_col = "rhs", maximize = TRUE)







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

