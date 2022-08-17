#' Frequency Table for Categorical variables
#'
#' Made for nominal and ordinal categorical variables, generates a frequency table which contains the following calculations: Frequency, Relative Frequency, Accumulated Frequency and Relative Accumulated Frequency.
#'
#' @param datasetColumn Designed for nominal and ordinal categorical variables from a cleansed dataset column, such as: cars$speed
#' @param numberCategories The number of categories that handles the variable + 1. E.g.: 4 Categories; Fully agree (...) Fully disagree; numberCategories = 5.
#' @param classColname Optional: Provide a string of text to rename (1, 1) indexes from the table generated. Default name: "class".
#' @param cutDig.lab Provides an integer to dig.lab parameter from cut() function in case classes are rational.
#'
#' @return A frequency table to interpret or to calculate measures of central tendency.
#' @export
#'
#' @examples diamonds <- ggplot2::diamonds
#' cut <- freqTab_categorical(diamonds$cut, 6, "cut")
#' cut
freqTab_categorical <-
  function(datasetColumn, numberCategories, classColname = "class", cutDig.lab = 0){
    column2Table <- na.omit(as.numeric(datasetColumn))
    n <- length(column2Table)
    c <- round(log2(n) + 1)
    amplitude <- round((range(column2Table)[2] - range(column2Table)[1]) / c, )
    seqGroupLimits <- 1:numberCategories
    groupLimits <- c(min(column2Table) * numberCategories)

    freqTab <- cut(column2Table, breaks = seqGroupLimits, right = F, dig.lab = cutDig.lab, include.lowest = T) # Realiza los intervalos respectivos a los datos
    freqTab <- data.frame(table(freqTab))

    freqTab$Rel.Freq <- freqTab$Freq / sum(freqTab$Freq) * 100
    freqTab$Acc.Freq <- cumsum(freqTab$Freq)
    freqTab$Rel.Acc.Freq <- cumsum(freqTab$Rel.Freq)

    colnames(freqTab)[1] <- classColname
    freqTab
  }
