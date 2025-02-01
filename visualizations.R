library(dplyr)
library(ggcorrplot)
eeg_data <- read.csv("/Train_and_Validate_EEG.csv")

#helper function to find number of missing values
count_NAs <- function(a) {
  
  return(sum(is.na(a)))
}


barplot(head(sort(apply(eeg_data, 2, count_NAs),decreasing=TRUE)), col="light blue", 
        main="Number of Missing Values in Columns", xlab="Column Names")
#removing missing values
eeg_removed <- eeg_data %>% dplyr::select(-X) %>% dplyr::filter(!is.na(IQ) & !is.na(education))

#removing coherence variables to reduce dimensions
noCOH <- eeg_removed[!grepl("COH",colnames(eeg_removed))]

#Correlation Matrices between different Frequency Bands
ggcorrplot::ggcorrplot(cor(noCOH[grepl("theta",colnames(noCOH))]), colors=c("green","white","blue"), title="Correlation between AB Theta Electrodes")
ggcorrplot::ggcorrplot(cor(noCOH[grepl("alpha",colnames(noCOH))]), colors=c("green","white","blue"), title="Correlation between AB Alpha Electrodes")
ggcorrplot::ggcorrplot(cor(noCOH[grepl("beta",colnames(noCOH))]), colors=c("green","white","blue"), title="Correlation between AB Beta Electrodes")
ggcorrplot::ggcorrplot(cor(noCOH[grepl("highbeta",colnames(noCOH))]), colors=c("green","white","blue"), title="Correlation between AB High Beta Electrodes")
ggcorrplot::ggcorrplot(cor(noCOH[grepl("delta",colnames(noCOH))]), colors=c("green","white","blue"), title="Correlation between AB Delta Electrodes")
ggcorrplot::ggcorrplot(cor(noCOH[grepl("gamma",colnames(noCOH))]), colors=c("green","white","blue"), title="Correlation between AB Gamma Electrodes")

#Correlation Matrix for non-EEG data
noCOH$sex <- as.factor(noCOH$sex)
noCOH$sex <- ifelse(noCOH$sex=="M", 0,1)
ggcorrplot::ggcorrplot(cor(noCOH[c(2,3,5,6)]), colors=c("green","white","blue"))
