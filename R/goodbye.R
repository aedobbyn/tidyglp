
# https://github.com/cran/Rglpk/blob/master/R/Rglpk_solve.R
library(assertthat)


library(Rglpk)
library(tidyverse)

### Example from package using vectors ###
obj <- c(2, 4, 3)
mat <- matrix(c(3, 2, 1, 4, 1, 3, 2, 2, 2), nrow = 3)
dir <- c("<=", "<=", "<=")
rhs <- c(60, 40, 80)
max <- TRUE

Rglpk_solve_LP(obj, mat, dir, rhs, max = max)


### Using a tibble and tidy_glp() instead ###

# Create objective row
obj_df <- matrix(obj, nrow = 1) %>%
  as_tibble() %>%
  mutate(
    dir = NA_character_,
    rhs = NA_integer_,
    role = "obj"
  )

# Create constraint
constraint_df <- as_tibble(mat) %>%
  mutate(dir = dir,
         rhs = rhs,
         role = "constraint")

# Add objective row as first row of full dataframe and constraints as the rest
full_df <- obj_df %>%
  bind_rows(constraint_df)

full_df


tidy_glp <- function(tbl, dir_col = "dir", rhs_col = "rhs",
                     maximize = FALSE, types = NULL,
                     ...) {

  obj_row <- which(tbl$role == "obj")

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

  matrix_cols <- which(!names(tbl) %in% c(dir_col, rhs_col, "role"))

  obj_vec <- tbl[obj_row, matrix_cols]
  tbl <- tbl[-obj_row, ]

  out <- Rglpk_solve_LP(obj = obj_vec,
                        mat = as.matrix(tbl[, matrix_cols]),
                        dir = tbl[[dir_col]],
                        rhs = tbl[[rhs_col]],
                        max = maximize,
                        types = types
                        )

  return(out)
}

# Run solver
tidy_glp(full_df, obj_row = 1, dir_col = "dir", rhs_col = "rhs", maximize = TRUE)

assertthat::are_equal(Rglpk_solve_LP(obj, mat, dir, rhs, max = max),
                      tidy_glp(full_df, obj_row = 1, maximize = TRUE))






full_df_w_bounds <- full_df %>%
  bind_rows(
    tibble(
      V1 = c(5, -1),
      V2 = c(4, 0),
      V3 = c(6, -2),
      dir = NA_integer_,
      rhs = NA_integer_,
      role = c("upper_bound", "lower_bound")
    )
  )

tidy_glp(full_df_w_bounds, obj_row = 1, dir_col = "dir", rhs_col = "rhs", maximize = TRUE)



tidy_glp(full_df, obj_row = 1, dir_col = "dir", rhs_col = "rhs", maximize = TRUE,
         bounds = list(lower = list(ind = 1:3,
                                    val = 5:7)))




rename(
  v_1 = V1,
  v_2 = V2,
  v_3 = V3
)


# matrix(as_vector(tbl[, matrix_cols]), nrow = nrow(tbl))


# Using mtcars
df <- as_tibble(mtcars) %>%
  select(1:4) %>%
  slice(1:5) %>%
  mutate(
    dir_obs = c(NA, ">", "<=", ">=", "<")
  ) %>%
  select(2:5, 1) %>%   # mpg is rhs
  mutate(
    mpg = c(NA, 201, 300, 100, 500)
  )

tidy_glp(df, obj_row = 1, dir_col = "dir_obs", rhs_col = "mpg")





