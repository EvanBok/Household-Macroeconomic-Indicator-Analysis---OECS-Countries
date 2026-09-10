# ============================================================
Research Question 1: Did the 2008 financial crisis structurally change the relationship between household debt and savings behavior across OECD countries?
# ============================================================
  #DEBT: Australia and Canada's debt rose higher, USA's dropped significantly, and Japans dropped a little but stayed stable
  #SAVINGS: All stayed relatively stable
  
ggplot(data=hh_budget, aes(x=Year))+
  geom_line(aes(y=Debt, linetype="Debt"),color="blue")+geom_line(aes(y=Savings, linetype = "Savings"))+
  geom_vline(xintercept = 2008, color="red", linetype="dashed")+
  facet_wrap(~Country)+
  labs(title="2008 Financial Crisis Debt vs Savings Comparison", y=NULL)

# ============================================================
Research Question 2: Is there a lagged relationship between household debt levels and unemployment — does rising debt predict unemployment increases 1–2 years later, or is causality more likely reversed?
# ============================================================

#Create a df that shows a side-by-side comparison of current year debt compared to debt to the previous two years
  #This code shows a wide table showing debt from the previous two years (debt_lag1 and debt_lag2) - we use this to feed into our regression model because models want each variable as its own column
hh_lagged <- hh_budget |> 
  arrange(Country, Year) |> 
  group_by(Country) |> 
  mutate(debt_lag1 = lag(Debt, 1),
         debt_lag2 = lag(Debt, 2)) |> 
  ungroup()

#This code shows a long table showing debt from one year ago (debt_lag1) - we use the long format to be able to create a ggplot chart because color=variable and geom_line() want one row per line per point (easier to plot two lines with different colors like this)
hh_lagged_long <- hh_lagged |> 
  select(Country, Year, debt_lag1, Unemployment) |> 
  pivot_longer(cols = c(debt_lag1, Unemployment),
    names_to = "variable", values_to = "value")

#NOTE: wide for modeling, long for plotting
ggplot(hh_lagged_long, aes(x = Year, y = value, color = variable)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ Country, scales = "free_y") +
  labs(title = "Prior-Year Debt vs. Current Unemployment Over Time",
       subtitle = "debt_lag1 is already shifted forward one year to align with the unemployment it's predicting",
       x = NULL, y = NULL, color = NULL) +
  theme_minimal()

#Here is the model to test the relationship between household debt levels and unemplyoment
model_lag<-lm(Unemployment ~ debt_lag1+debt_lag2, data=hh_lagged)
summary(model_lag)

  #In our model, Unemployment is the dependent variable and debt_lag1+debt_lag2 are the combined independent variables
  # debt_lag1 and debt_lag2 individually both have p-values greater than 0.05, which tells us that neither have a statistically significant relationships with unemployment
  # An R-squared of 0.023 is very low and tells us that debt_lag1 and debt_lag2 combined have almost no explanatory power on the changes in unemployment rate
  # An adjusted R-squared of -0.0028 (-0.28%) means the model is performing worse than a model with no predictors at all
  # The overall combined P-value of 0.41 tells us that debt_lag1 and debt_lag2 taken together, doesn't improve predicting unemployment rate. Since p-value>0.05, we fail to reject the null hypothesis that these have no real effect
  
#Now that we know that debt_lag1+debt_lag2 are not great predictors of unemployment rate, we can test the opposite - 
  #Is unemployment rate a significant predictor of future debt?
#This code creates a df with unemployment lag
hh_lagged2 <- hh_budget |> 
  arrange(Country, Year) |> 
  group_by(Country) |> 
  mutate(unemp_lag1=lag(Unemployment,1)) |> 
  ungroup()

#Here is our model measuring the significance of impact that unemployment has on predicting future debt
model_reverse <- lm(Debt ~ unemp_lag1, data=hh_lagged2)
summary(model_reverse)

  #unemp_lag1 has a P-value of 0.623, meaning it is not a significant predictor of debt
  # R-squared is 0.00295(0.295%), is lower  than the original model but is still very low and tells us that unemployment rate has almost no explanatory power in determining debt
  # Adjusted R-squared of -0.00921(-0.921%) means the model performs slightly worse than with no predictors at all

#CONCLUSION: Neither direction shows a statistically significant relationship in this data
  #Debt doesn't significantly predict future employment, and unemployment doesn't significantly predict future debt
  #This suggests that in this data set and time frame debt and unemployment don't have a strong causal relationship in either direction, any relationship between the two may be driven by a third factor
  #The null result doesn't completely rule out a relationship between debt and unemployment - it's likely that their relationship is non-linear, driven by third factor (e.g macroeconomic shocks) or that the sample size of 4 countries lacks power to detect significant effects.

# ============================================================
Research Question 3: Does household wealth help explain why some countries' spending stays stable when income changes, while other countries' spending closely tracks income ups and downs?
# ============================================================

#need to make the df long to plot it
hh_long <- hh_budget |> 
  select(Year, Country, Expenditure, DI) |> 
  pivot_longer(cols=c(Expenditure, DI), names_to="variable", values_to("value"))

#graph
ggplot(hh_long, aes(x=Year, y=value, color=variable))+
  geom_line()+
  facet_wrap(~Country)+
  labs(title="Expenditure vs Disposable Income Change by Country", x=NULL, y=NULL)

#create a table to show average wealth, DI, and expenditure for each country. This allows us to compare each countries average wealth to their expenditure and disposable income to see if these change compared to their wealth.
hh_budget |> 
  group_by(Country, Year) |> 
  summarize(avg_wealth=mean(Wealth), avg_DI=mean(DI), avg_exp=mean(Expenditure)) |> 
  arrange(desc(Year)) |> 
  print(n=100)

#this code calculates a correlation coefficient for each country, telling us how closely Expenditure tracks DI over time. 
  #Correlation values near 1 mean spending closely tracks income changes, values near 0 means spending stays more stable despite income changes.
#It is also representative of the graph above
hh_budget |> 
  group_by(Country) |> 
  summarize(
    income_exp_correlation = cor(DI, Expenditure),
    avg_wealth = mean(Wealth)
  ) |> 
  arrange(desc(avg_wealth))

#Model
model_smoothing <- lm(Expenditure~DI*Wealth, data = hh_budget)
summary(model_smoothing)
  #This code tests whether DI and Wealth independently effect Expenditure and the research question - whether Wealth changes the strength of DI's effect on Expenditure
  #Expenditure is the dependent variable, DI and Wealth are both the independent variables
  #We use DI*Wealth so shows results for DI, Wealth, and DI:Wealth together
  #DI has a p-value of 0.09486 above the 0.05 threshold, so we fail to reject the null. Wealth has a p-value of 0.00156 making it significant. DI:Wealth is 0.00294 also making it statistically significant.
  #The model was a whole is a moderately strong predictor of Expenditure, with a R-squared of 0.4554 and p-value of 4.158e-11.
    #The significant interaction confirms that DI's effect on Expenditure depends on Wealth (strengthens as Wealth increases)

