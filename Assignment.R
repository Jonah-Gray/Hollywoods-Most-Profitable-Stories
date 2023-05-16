#Loading and viewing the data
df <- read.csv("http://public.tableau.com/app/sample-data/HollywoodsMostProfitableStories.csv")

View(df)

install.packages("tidyverse")

str(df)

#Cleaning the Data
colSums(is.na(df))

df <- na.omit(df)

colSums(is.na(df))

dim(df[duplicated(df$film),])[1]

df$Profitability <- round(df$Profitability ,digit=2)

df$Worldwide.Gross <- round(df$Worldwide.Gross ,digit=2)

dim(df)

library(ggplot2)

#boxplot
ggplot(df, aes(x=Profitability,y=Worldwide.Gross)) + geom_boxplot(outlier.colour ="red", outlier.shape = 1)+ scale_x_continuous(labels =scales::comma)+coord_cartesian(ylim = c(0, 1000))

#removing outliers
Q1 <- quantile(df$Profitability, .25)
Q3 <- quantile(df$Profitability, .75)
IQR <- IQR(df$Profitability)
no_outliers <- subset(df, df$Profitability> (Q1 - 1.5*IQR) & df$Profitability< (Q3 + 1.5*IQR))
dim(no_outliers)

Q1 <- quantile(no_outliers$Worldwide.Gross, .25)
Q3 <- quantile(no_outliers$Worldwide.Gross, .75)
IQR <- IQR(no_outliers$Worldwide.Gross)
df1 <- subset(no_outliers, no_outliers$Worldwide.Gross> (Q1 - 1.5*IQR) & no_outliers$Worldwide.Gross< (Q3 + 1.5*IQR))
dim(df1)

summary(df1)

#scatterplot
ggplot(df1, aes(x=Lead.Studio, y=Rotten.Tomatoes..)) + geom_point()+ scale_y_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 110))+theme(axis.text.x = element_text(angle = 90))

#BarChart
ggplot(df1, aes(x=Year)) + geom_bar()

#Exporting Data
write.csv(df1, "clean_df.csv")
