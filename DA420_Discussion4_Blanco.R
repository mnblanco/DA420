library(quantmod)
library(ggplot2)
library(lubridate)
getSymbols('ANTM',verbose=TRUE,env=globalenv())


data <- new.env()
# load historical data, getSymbols from quantmod
getSymbols('ANTM', src = 'yahoo', from = '1950-01-01', env = data, auto.assign = T, periodicity = "monthly")    


recessions.df = read.table(textConnection(
  "Peak, Trough
1857-06-01, 1858-12-01
1860-10-01, 1861-06-01
1865-04-01, 1867-12-01
1869-06-01, 1870-12-01
1873-10-01, 1879-03-01
1882-03-01, 1885-05-01
1887-03-01, 1888-04-01
1890-07-01, 1891-05-01
1893-01-01, 1894-06-01
1895-12-01, 1897-06-01
1899-06-01, 1900-12-01
1902-09-01, 1904-08-01
1907-05-01, 1908-06-01
1910-01-01, 1912-01-01
1913-01-01, 1914-12-01
1918-08-01, 1919-03-01
1920-01-01, 1921-07-01
1923-05-01, 1924-07-01
1926-10-01, 1927-11-01
1929-08-01, 1933-03-01
1937-05-01, 1938-06-01
1945-02-01, 1945-10-01
1948-11-01, 1949-10-01
1953-07-01, 1954-05-01
1957-08-01, 1958-04-01
1960-04-01, 1961-02-01
1969-12-01, 1970-11-01
1973-11-01, 1975-03-01
1980-01-01, 1980-07-01
1981-07-01, 1982-11-01
1990-07-01, 1991-03-01
2001-03-01, 2001-11-01
2007-12-01, 2009-06-01"), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE)



ANTM <- as.data.frame(ANTM)
ANTM$date <- ymd(rownames(ANTM))

getSymbols("MPCT04XXS", src="FRED", from = min(ANTM$date), return.class = "xts")
MPCT04XXS <- as.data.frame(MPCT04XXS)
MPCT04XXS$date <- ymd(rownames(MPCT04XXS))


recessions.trim <- subset(recessions.df, Peak >= min(ANTM$date) )
MPCT04XXS <- subset(MPCT04XXS, date >= min(ANTM$date) )


ggplot() + geom_line(aes(x=date, y=MPCT04XXS, colour = "ANTM"), MPCT04XXS)

ggplot() + geom_line(aes(x=date, y=ANTM$ANTM.Close, colour = "ANTM"), ANTM)


pdf(file="time_series.pdf") 
    
p <- ggplot(ANTM, aes(x = ANTM$date))
p <- p +  geom_line(aes(x=date, y=ANTM$ANTM.Close, colour = "ANTM"), ANTM)
p <- p + geom_line(aes(x=date, y=MPCT04XXS*53, colour = "MPCT04XXS"), MPCT04XXS)
p <- p + scale_y_continuous(sec.axis = sec_axis(~./53, name = "Percent Change from Preceding Period [%]"))
p <- p + scale_colour_manual(values = c("blue", "red"))
p <- p + labs(y = "Anthem, Inc Closing Price",
              x = "Date",
              colour = "Parameter")
p <- p + theme(legend.position = c(0.1, 0.9))
p <- p + labs(title = "Anthem, Inc and Total Construction Spending: Health Care")
p
dev.off()


getSymbols("PCUASHCASHC", src="FRED", from = min(ANTM$date), return.class = "xts")
PCUASHCASHC <- as.data.frame(PCUASHCASHC)
PCUASHCASHC$date <- ymd(rownames(PCUASHCASHC))


PCUASHCASHC <- subset(PCUASHCASHC, date >= min(ANTM$date) )

pdf(file="time_series2.pdf") 

p <- ggplot(ANTM, aes(x = ANTM$date))
p <- p +  geom_line(aes(x=date, y=ANTM$ANTM.Close, colour = "ANTM"), ANTM)
p <- p + geom_line(aes(x=date, y=PCUASHCASHC*2.1, colour = "PCUASHCASHC"), PCUASHCASHC)
p <- p + scale_y_continuous(sec.axis = sec_axis(~./2.1, name = "Producer Price Index"))
p <- p + scale_colour_manual(values = c("blue", "red"))
p <- p + labs(y = "Anthem, Inc Closing Price",
              x = "Date",
              colour = "Parameter")
p <- p + theme(legend.position = c(0.1, 0.9))
p <- p + labs(title = "Anthem, Inc and Producer Price Index: Health Care")
p
dev.off()


