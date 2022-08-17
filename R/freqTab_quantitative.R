#' Frequency Table for quantitative variables
#'
#' Made for quantitative variables, generates a frequency table which contains the following calculations: Frequency, Relative Frequency, Accumulated Frequency and Relative Accumulated Frequency.
#' WARNING: It is important to note, that data columns that has 0 as its min() will not function.
#'
#' @param datasetColumn From a cleansed dataset column, such as: diamonds$price
#' @param classColname Optional: Provide a string of text to rename (1, 1) indexes from the table generated. Default name: "class".
#' @param cutDig.lab Provides an integer to dig.lab parameter from cut() function in case classes are rational.
#'
#' @return A frequency table to interpret or to calculate measures of central tendency.
#' @export
#'
#' @examples diamonds <- ggplot2::diamonds
#' price <- freqTab_quantitative(diamonds$price, "price", 10)
#' price
freqTab_quantitative <-
  function(datasetColumn, classColname = "class", cutDig.lab = 0){
    column2Table <- na.omit(as.numeric(datasetColumn))
    n <- length(column2Table)
    c <- round(log2(n) + 1)
    amplitude <- round((range(column2Table)[2] - range(column2Table)[1]) / c, )
    groupLimits <- c(seq(from = min(column2Table), to = max(column2Table) + amplitude, by = amplitude))

    freqTab <- cut(column2Table, breaks = groupLimits, right = F, dig.lab = cutDig.lab, include.lowest = T)
    freqTab <- data.frame(table(freqTab))

    freqTab$Rel.Freq <- freqTab$Freq / sum(freqTab$Freq) * 100
    freqTab$Acc.Freq <- cumsum(freqTab$Freq)
    freqTab$Rel.Acc.Freq <- cumsum(freqTab$Rel.Freq)

    colnames(freqTab)[1] <- classColname
    return(freqTab)
  }
