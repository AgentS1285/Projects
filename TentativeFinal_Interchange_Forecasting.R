library(fpp3)

#Reading in our data to a Tsibble object and formatting our Date column
intake_data <-  read.csv('/Users/chrismccann/Downloads/Animal_Services_Intake_Data.csv') %>% 
  drop_na() %>% 
  filter(Intake.Condition == "ALIVE", Animal.Type %in% c("DOG", "CAT")) %>% 
  mutate(Intake.Date = as.Date(Intake.Date, format = "%m/%d/%Y")) %>% 
  group_by(Intake.Date) %>% 
  summarise(Total = n())

# create a tsibble
intake_ts <- intake_data %>% 
  as_tsibble(index = Intake.Date)

# training subset of tsibble
intake_train <- intake_ts %>% 
  filter(year(Intake.Date) < 2013)

# view the data
intake_ts %>% autoplot()

# view in gg_season to get some clues to seasonality present
intake_train %>%  gg_season(Total, period = "year") +
  theme(legend.position = "right") +
  labs(y="Total Rescues Per Day", x="Month", title="Seasonality Plot by Year")

# view STL decomposition of our training data
intake_train %>% 
  model(STL(Total)) %>% 
  components() %>% 
  autoplot() 

# It appears there is a slight downward trend in our data
# we also see that there is weekly and yearly seasonality...let's explore more
intake_train %>%  filter(year(Intake.Date) == 2012) %>% 
  gg_season(Total, period = "week") +
  theme(legend.position = "right") +
  labs(y="Total Rescues Per Day", x="Month", title="Seasonality Plot by Week") 

# there seems to be peaks on Tuesdays and Saturdays. However, the seasonality
# isn't as noticeable and consistent as the yearly seasonality. Therefore, we
# will move forward with yearly seasonality 

# average each month - we must do this with the models we are investigating, as they will choose the first order of
#seasonality, in this case yearly
intake_yearly <- xts::apply.monthly(intake_ts,mean) 

# dates are saved as rownames, make column
intake_yearly$Intake.Date <- row.names(intake_yearly)

# back to tsibble
intake_yearly <- intake_yearly %>% 
  mutate(
    Intake.Date = yearmonth(Intake.Date)
  ) %>% 
  as_tsibble(index = Intake.Date) %>% 
  fill_gaps() 

intake_yearly_train <- intake_yearly %>% 
  filter(year(Intake.Date) < 2013)

# compare multiple years vs seasonality
autoplot(intake_train)
autoplot(intake_yearly_train)

####### MODELS -> Seasonal Naive, Naive, Drift, Mean
intake_fit <- intake_yearly_train %>% 
  model(
    SeasNaive = SNAIVE(Total ~ lag(12)),
    Naive = NAIVE(Total),
    Drift = NAIVE(Total ~ drift()),
    Mean = MEAN(Total)
  )

# naive residuals - I am noticing patterns in the residuals suggesting much of the model was not successfully captured
intake_fit %>% 
  select(Naive) %>% 
  gg_tsresiduals() +
  ggtitle("Residuals for NAIVE")

# seasonal naive residuals - Residuals look good
intake_fit %>% 
  select(SeasNaive) %>% 
  gg_tsresiduals() +
  ggtitle("Residuals for SNAIVE")

#  drift residuals - Similarly to the naive model, there is definitely a pattern in the lag plot
intake_fit %>% 
  select(Drift) %>% 
  gg_tsresiduals() +
  ggtitle("Residuals for Drift")

#  mean residuals - once again, our residual plot is showing we've left seasonality on the table - this makes sense
#  and is expected of simple models which do not consider seasonality
intake_fit %>% 
  select(Mean) %>% 
  gg_tsresiduals() +
  ggtitle("Residuals for Mean")

# try forecasting
intake_fc <- intake_fit %>% 
  forecast(h="1 year")

# accuracy test - as expected, SeasNaive is far superior to the other plots
accuracy(intake_fc, intake_yearly)

# seasonal naive looks best, let's plot
intake_fc %>% 
  autoplot(intake_ts, level = NULL) +
  autolayer(augment(intake_fit)) +
  xlab("Year") +
  ylab("Total Count") +
  ggtitle("Total Rescues per Year") 

#ljung box text
augment(intake_fit) %>%
  features(.innov, ljung_box)
# The p-values for SNAIVE are less than 0.05, this is good because it suggests our residuals are not
# highly auto-correlated in our SeasNaive model. Ljung is not appropriate for NAIVE, Mean, or Drift

####### MODELS -> ETS, ARIMA

# check a bunch of ETS modesl
Intake_fit_HW <- intake_yearly_train %>%
  model(HWA = ETS(Total ~ error("A") + trend("N") + season("A")),
        HWM = ETS(Total ~ error("M") + trend("N") + season("M")),
        HWMA = ETS(Total ~ error("M") + trend("A") + season("M")),
        HWAA = ETS(Total ~ error("A") + trend("A") + season("A"))
  )

# Forecasting
Intake_fc_HW <- Intake_fit_HW %>%
  forecast(h = "1 year")

# check the accuracy
accuracy(Intake_fc_HW, intake_yearly)

# HWMA appears to be the lowest RMSE

#ljung box text
augment(Intake_fit_HW) %>%
  features(.innov, ljung_box)
# only models HWAA and HWMA have p-values lower than 0.05 - we will procede with them

Intake_fit_2 <- intake_yearly_train %>%
  model(
    HWMA = ETS(Total ~ error("M") + trend("A") + season("M")),
    SeasNaive = SNAIVE(Total),
    auto_ARIMA = ARIMA(Total),
    HWAA = ETS(Total ~ error("A") + trend("A") + season("A"))
  )

# forecast the models
Intake_fc_2 <- Intake_fit_2 %>% 
  forecast(h = "1 year")

# graph the forecast and the models
Intake_fc_2 %>% 
  autoplot(intake_yearly, level = NULL) +
  autolayer(augment(Intake_fit_2)) +
  xlab("Year") +
  ylab("Total Count") +
  ggtitle("Total Rescues per Year") 

# accuracy tests
accuracy(Intake_fc_2, intake_yearly)

#ljung box text
augment(Intake_fit_2) %>%
  features(.innov, ljung_box)

# ARIMA is the only model with a p-value higher than 0.05

# residuals for ARIMA - Arima left out quite a bit of seasonality
Intake_fit_2 %>% 
  select(.model = 'auto_ARIMA') %>% 
  gg_tsresiduals() +
  ggtitle("ARIMA Residuals")

# residuals for Seasonal Naive - residuals look good
Intake_fit_2 %>% 
  select(.model = 'SeasNaive') %>% 
  gg_tsresiduals() +
  ggtitle("Seasonal Naive Residuals")

# residuals for HWMA - these residuals also look good
Intake_fit_2 %>% 
  select(.model = 'HWMA') %>% 
  gg_tsresiduals() +
  ggtitle("Holts Winter Residuals")

# data is bimodal?

# residuals for HWMA
Intake_fit_2 %>% 
  select(.model = 'HWAA') %>% 
  gg_tsresiduals() +
  ggtitle("Holts Winter Residuals")

# The two Holts-Winter models appear to be the best. Let's test all with...
# Cross-Validation


intake_cv <- intake_yearly_train %>%
  select(Intake.Date, Total) %>% 
  stretch_tsibble(.init = 23) 

# create models
cv_fit <- intake_cv %>%
  model( 
    HWMA = ETS(Total ~ error("M") + trend("A") + season("M")),
    SeasNaive = SNAIVE(Total),
    auto_ARIMA = ARIMA(Total),
    HWAA = ETS(Total ~ error("A") + trend("A") + season("A"))
  ) 

# Forecast1 year ahead
cv_fc <- cv_fit %>%
  forecast(h = "1 year")

# evaluate accuracy
accuracy(cv_fc, intake_yearly)

# Holts-Winter w/ multiplicative error, trend, and season came out first


# VISUALS 
cv_fc %>% 
  autoplot(intake_cv, level = NULL) +
  autolayer(augment(cv_fit)) +
  xlab("Year") +
  ylab("Total Count") +
  ggtitle("Total Rescues per Year") +
  guides(colour = guide_legend(title = c("Model","Forecast"))) 

# FINAL MODEL - applying our chosen model to our converted monthly data set
final_fit <- intake_yearly %>%
  model( 
    ETS(Total ~ error("M") + trend("A") + season("M"))
  ) 

# forecast forward 1 year
final_fc <- final_fit %>%
  forecast(h = "1 year")

# VISUALS 
final_fc %>% 
  autoplot(intake_ts, level = NULL, size = 2, color = 'red') +
  autolayer(augment(final_fit), size = 2, color = 'blue') +
  xlab("Year") +
  ylab("Total Count") +
  ggtitle("Total Rescues per Year")

