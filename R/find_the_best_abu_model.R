#' Best subset regression (BSR) on abundance data
#'
#' @param x data frame that holds observation data and environmental metrics. observation data should be in a col called `obs`
#' @param variables_lst list of variables to include in BSR
#'
#' @return list of all models run
#' @export
find_the_best_abu_model <- function(x, variables_lst = c("edge", "nbr", "understory", "kurtosis", "height90")) {

  # stops

  # select variables

  z <- x$data_suitable_plots

  y <-
    x$data_suitable_plots %>% # select the columns you want
    dplyr::select(
      obs,
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
    formula = obs ~ .,
    family = gaussian,
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
  t1 <- glm(f, family = gaussian, data = y)
  y$prediction_abu <- predict(t1, type = "response")
  y$prediction_rounded_abu <- round(y$prediction_abu)


  # testing
  test_df <-
    x$test_plots %>%
    tidyr::drop_na()

  test_df$prediction_abu_test <- predict(
    t1,
    type = "response",
    newdata = test_df)
  test_df$prediction_rounded_abu_test <- round(test_df$prediction_abu_test)


  test_complete <- tibble(obs_abu = test_df$obs, prediction_abu = test_df$prediction_abu_test) %>%
    drop_na()
  n_r2 <- length(test_complete$obs_abu)


  n_obs <- sum(y$obs, na.rm = TRUE)


  list(
    mods = d,
    data_abu = y,
    test_complete = test_complete,
    n_obs = n_obs,
    low_obs = low_obs
  )
}
