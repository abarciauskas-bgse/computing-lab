players <- read.csv('lahman-csv_2015-01-24/Master.csv', stringsAsFactors = FALSE)
salaries <- read.csv('lahman-csv_2015-01-24/Salaries.csv', stringsAsFactors = FALSE)

playerSalaries <- merge(x = salaries, y = players, by = "playerID", all.y = TRUE)
playersSalaries$yearID <- as.factor(playersSalaries$yearID)

# Create a matrix of means of the average salary for all players in a given year:
years <- na.omit(unique(playersSalaries$yearID))
meanSalaries <- matrix(c(sort(years), rep(0, length(years))), nrow = length(years), ncol = 2)
colnames(meanSalaries) <- c("year", "avgSalary")

for (year in years) {
  playerSalariesForYear <- subset(playersSalaries, yearID == year)$salary
  meanSalaries[meanSalaries[,1] == year][2] = mean(playerSalariesForYear)
}

# Now we can normalize them!
#
playersSalaries$normalizedSalary <- mapply(
  function(salary, year) {
    yavg <- meanSalaries[meanSalaries[,1] == year][2]
    salary / yavg
  }, playersSalaries$salary, playersSalaries$yearID)
