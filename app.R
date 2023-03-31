library(googleAnalyticsR)

ga_auth()

ac <- ga_account_list()

test <- google_analytics(97072265,
                         date_range = c('2022-01-01', '2022-12-31'),
                         metrics = c('sessions', 'users'),
                         dimensions = c('date', 'medium', 'latitude', 'longitude'),
                         max = -1)


devtools::load_all(".")

pivotR(test)
