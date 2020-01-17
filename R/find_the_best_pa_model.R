#' Best subset regression (BSR) on presence-absence data
#'
#' @param x data frame that holds observation data and environmental metrics. observation data should be in a col called `obs`
#' @param variables_lst list of variables to include in BSR
#'
#' @return list of all models run
#' @export
#'
find_the_best_pa_model <- function(x, variables_lst = c("edge", "nbr", "understory", "kurtosis", "height90")) {

  # stops

test_train <-
  x %>%
  rsample::initial_split(prop = 0.8)

  # select variables

  y <-
    test_train %>%
    rsample::training() %>%
    # create a variable that is presence absence
    dplyr::mutate(
      pa = if_else(
        obs > 0,
        1,
        0
      )
    ) %>% # select the columns to model with
    dplyr::select(
      pa,
      variables_lst
    ) %>% # remove records that have NA values
    tidyr::drop_na()

  # check if there are to few observations for a good model
  low_obs <-
    if_else(
      length(y) <= 7,
      TRUE,
      FALSE
    )

  # create model formula

  m1 = glm(
    formula = pa ~ .,
    family = binomial,
    data = y,
    na.action = na.fail
    )

  # dredge
 d <- MuMIn::dredge(
    m1,
    m.lim = c(0, 3),
    evaluate =  TRUE,
    extra = list( # add extra summary stats
      "R^2",
      "adjR^2",
      aic = function(x) stats::AIC(x),
      F = function(x) summary(x)$fstatistic[[1]]
      )
    ) %>% # to return model objects rather than stats
    MuMIn::get.models(subset = TRUE)

 # predict
 f <- formula(d[[1]])
 t1 <- glm(f, family = binomial, data = y)
 y$prediction_pa_train <- exp(predict(t1, type = "response"))
 y$prediction_rounded_pa_train <- round(y$prediction_pa_train)

 # bring obs back in
 y <- left_join(x = y, y = x) %>%
   dplyr::select(
     pa, obs,
     edge, nbr, understory, kurtosis, height90,
     prediction_pa_train, prediction_rounded_pa_train
   ) %>%
   # tidyr::drop_na() %>%
   unique()

 # remove unsuitable plots
 data_suitable_plots <- subset(
   y,
   prediction_rounded_pa_train>=1
   )

# testing
 test_df <-
   test_train %>%
   rsample::testing() %>%
   # create a variable that is presence absence
   dplyr::mutate(
     pa = if_else(
       obs > 0,
       1,
       0
     )
   ) %>% # select the columns to model with
   dplyr::select(
     pa, obs,
     variables_lst
   ) %>% # remove records that have NA values
   tidyr::drop_na()

 test_df$prediction_pa_test <- exp(predict(
   t1,
   type = "response",
   newdata = test_df))
 test_df$prediction_rounded_pa_test <- round(test_df$prediction_pa_test)


 test_complete <- tibble(
   obs_pa = test_df$pa,
   prediction_pa = test_df$prediction_pa_test,
   prediction_rounded = test_df$prediction_rounded_pa_test
   ) %>%
   drop_na()
 n_r2 <- length(test_complete$obs_pa)


 n_obs <- sum(y$obs, na.rm = TRUE)

list(
  mods = d,
  data_suitable_plots = data_suitable_plots,
  test_plots = test_df,
  test_complete = test_complete,
  n_obs = n_obs,
  low_obs = low_obs
     )

}
