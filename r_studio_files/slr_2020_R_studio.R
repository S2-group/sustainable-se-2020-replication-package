library(ggplot2)
library(dplyr)
library(ggpubr)

# plot for extended publication trends
xval <- dates$V2
color <- dates$V1
ggplot(dates, aes(x=xval, fill=color)) +
  geom_histogram( color="#e9ecef", alpha=1, position = 'stack', binwidth = 2) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="", x = "Year of Publication", y = "Frequency") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"))
# END OF PUBLICATION TRENDS

# Plot for experiment distribution
df <- experiments %>%
  group_by(experiments$V1) %>%
  summarise(counts = n())
df

data <- data.frame(
  type=c("None", "Combination", "Real-world", "Simulation"),
  freq=c(1,3,4,9)
)

data

ggplot(data, aes(x=reorder(type, freq), y=freq)) + 
  geom_bar(stat = "identity") +
  labs(fill="", x = "Type of Experiment", y = "Frequency") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
# End of plot for experiment distribution

# Plot for trade-off frequency
df <- trade.offs %>%
  group_by(trade.offs$V1) %>%
  summarise(counts = n())
df

data <- data.frame(
  type=c("Affordability", "Availability", "Mobility", "Simplicity", "Timeliness", "Understandability"),
  freq=c(1,1,1,3,8,2)
)

ggplot(data, aes(x=reorder(type, freq), y=freq)) + 
  geom_bar(stat = "identity") +
  labs(fill="", x = "QA Trading-off with Efficiency", y = "Frequency") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
