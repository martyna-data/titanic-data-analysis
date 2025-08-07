# Load data
data <- read.csv("C:\\Users\\Dell\\Desktop\\Semestr 2\\R\\9_Preprocessing\\titanic_new.csv", stringsAsFactors = FALSE)
View(data)

# Check the structure of the data
str(data)

# Convert Age to numeric type
data$Age <- as.numeric(data$Age)

# Remove unnecessary columns
data <- data[ , !(names(data) %in% c("PassengerId", "Passenger.Id", "Name", "Ticket", "Cabin"))]

# Convert selected columns to factors
data$Survived <- as.factor(data$Survived)
data$Pclass   <- as.factor(data$Pclass)
data$Sex      <- as.factor(data$Sex)
data$Embarked <- as.factor(data$Embarked)

# Fill missing values in Age with the mean
data$Age[is.na(data$Age)] <- mean(data$Age, na.rm = TRUE)

# Fill missing values in Embarked with the most frequent category
most_common_port <- names(sort(table(data$Embarked), decreasing = TRUE))[1]
data$Embarked[is.na(data$Embarked) | data$Embarked == ""] <- most_common_port

# Create a new column: FamilySize (SibSp + Parch + 1)
data$FamilySize <- data$SibSp + data$Parch + 1

# Descriptive statistics for numerical variables
summary(data[, sapply(data, is.numeric)])

# Data cleaning - standardize incorrect values
data$Sex <- tolower(data$Sex)
data$Sex[data$Sex %in% c("female", "feemale", "f2emale")] <- "female"
data$Sex[data$Sex %in% c("male", "mal3e", "mal4e", "malle", "malwe")] <- "male"
data$Sex <- as.factor(data$Sex)

data$Embarked <- toupper(data$Embarked)
data$Embarked <- as.factor(data$Embarked)

# Check distribution of categorical variables
table(data$Sex)
table(data$Pclass)
table(data$Embarked)

# Fix values in Survived column (should be 0 or 1)
data$Survived <- as.numeric(as.character(data$Survived))
data$Survived[data$Survived > 1] <- 1
data$Survived[data$Survived < 0] <- 0
data$Survived <- as.factor(data$Survived)
table(data$Survived)

# Compute correlation matrix for numerical variables
correlation_matrix <- cor(data[c("Age", "Fare", "SibSp", "Parch", "FamilySize")], use = "complete.obs")

# Check data types before plotting
str(data)

# Remove non-numeric characters from Fare if any
data$Fare <- as.numeric(gsub("'", "", data$Fare))

# Plot the correlation matrix
library(corrplot)
corrplot(correlation_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)

# Chi-square test: survival vs. passenger class
chisq_test_class <- chisq.test(table(data$Survived, data$Pclass))
print(chisq_test_class)

# Chi-square test: survival vs. gender
chisq_test_sex <- chisq.test(table(data$Sex, data$Survived))
print(chisq_test_sex)

# Load e1071 package for skewness and kurtosis
library(e1071)

# Compute skewness and kurtosis for numerical variables
skewness_age <- skewness(data$Age, na.rm = TRUE)
kurtosis_age <- kurtosis(data$Age, na.rm = TRUE)

skewness_fare <- skewness(data$Fare, na.rm = TRUE)
kurtosis_fare <- kurtosis(data$Fare, na.rm = TRUE)

skewness_sibsp <- skewness(data$SibSp, na.rm = TRUE)
kurtosis_sibsp <- kurtosis(data$SibSp, na.rm = TRUE)

skewness_parch <- skewness(data$Parch, na.rm = TRUE)
kurtosis_parch <- kurtosis(data$Parch, na.rm = TRUE)

skewness_familysize <- skewness(data$FamilySize, na.rm = TRUE)
kurtosis_familysize <- kurtosis(data$FamilySize, na.rm = TRUE)

# Display the results
cat("Skewness (Age):", skewness_age, "Kurtosis (Age):", kurtosis_age, "\n")
cat("Skewness (Fare):", skewness_fare, "Kurtosis (Fare):", kurtosis_fare, "\n")
cat("Skewness (SibSp):", skewness_sibsp, "Kurtosis (SibSp):", kurtosis_sibsp, "\n")
cat("Skewness (Parch):", skewness_parch, "Kurtosis (Parch):", kurtosis_parch, "\n")
cat("Skewness (FamilySize):", skewness_familysize, "Kurtosis (FamilySize):", kurtosis_familysize, "\n")

# ggplot2 visualizations
library(ggplot2)

ggplot(data, aes(x = Sex, fill = Survived)) +
  geom_bar(position = "dodge") +
  labs(title = "Survival by Gender",
       x = "Gender",
       y = "Passenger Count",
       fill = "Survived") +
  scale_fill_manual(values = c("red", "green"),
                    labels = c("Did not survive", "Survived")) +
  theme_minimal()

ggplot(data, aes(x = Pclass, fill = Survived)) +
  geom_bar(position = "dodge") +
  labs(title = "Survival by Passenger Class",
       x = "Passenger Class",
       y = "Passenger Count",
       fill = "Survived") +
  scale_fill_manual(values = c("red", "green"),
                    labels = c("Did not survive", "Survived")) +
  theme_minimal()
