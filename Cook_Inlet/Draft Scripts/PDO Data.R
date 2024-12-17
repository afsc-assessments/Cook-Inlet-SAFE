# Project Name: Upper Cook Inlet Bayesian PF and harvest specs
# Creator: Aaron Lambert - NOAA
# Date: 12-10-2024

# PDO data wrangling

# Read in PDO data. Saved from https://www.ncei.noaa.gov/access/monitoring/pdo/
PDO.df <- as.data.frame(read.table(file = paste0(getwd(),"/Data for SAFE report/PDO_NCEI_10Dec24.txt"), header = TRUE))

# Filter to less years and pivot longer
PDO.df2 <- PDO.df %>% 
  filter(Year >= 1990) %>% 
  pivot_longer(cols = -c(Year), names_to = "Month", values_to = "Index")

# Get the mean
PDO.mean <- PDO.df2 %>% 
  filter(Month%in%c("Mar","Apr","Jun")) %>% 
  group_by(Year) %>% 
  reframe("avg" = mean(Index))

# Add extra year to lag to
tt<- data.frame("Year"=2025,
                "avg" = NA)

# Bind
PDO.mean <- rbind(PDO.mean,tt)

# Lag the PDO 3 years
lag.PDO.mean <-PDO.mean %>% 
  mutate(lag.PDO = lag(avg, n = 3))

