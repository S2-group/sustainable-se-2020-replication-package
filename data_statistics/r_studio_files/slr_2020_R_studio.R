library(ggplot2)
library(dplyr)
library(ggpubr)
library(waffle)
library(ggthemes)

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
  theme(axis.line = element_line(colour = "black"), text = element_text(size=25))
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

# WAFFLE EXPERIMENT DISTR
vals <- c(9, 4, 3, 1)
val_names <- sprintf("%s (%s)", c("Simulation", "Real-world", "Combination",  "None"), scales::percent(round(vals/sum(vals), 2)))
names(vals) <- val_names

waffle::waffle(vals, rows = 2) +
  theme(text = element_text(size=25))


ggplot(data, aes(x=reorder(type, freq), y=freq)) + 
  geom_bar(stat = "identity") +
  labs(fill="", x = "Type of Experiment", y = "Frequency") +
  theme_bw() +
  geom_text(aes(label=freq), vjust=1.6, color="white", size=10) +
  theme(axis.line = element_line(colour = "black"), text = element_text(size=25)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
# End of plot for experiment distribution

# Plot for trade-off frequency
df_tradeoff <- trade_offs %>%
  group_by(trade_offs$V1) %>%
  summarise(counts = n())
df_tradeoff

data_tradeoff <- data.frame(
  type=c("Reliability", "Maintainability", "Performance Efficiency"),
  freq=c(1,5,8)
)

data_tradeoff

ggplot(data_tradeoff, aes(x=reorder(type, freq), y=freq)) + 
  geom_bar(stat = "identity") +
  labs(fill="", x = "Type of Experiment", y = "Frequency") +
  theme_bw() +
  geom_text(aes(label=freq), vjust=1.6, color="white", size=10) +
  theme(axis.line = element_line(colour = "black"), text = element_text(size=25)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

ggplot(data, aes(x="", y=freq, fill=type)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

ggplot(data, aes(x=reorder(type, freq), y=freq)) + 
  geom_bar(stat = "identity") +
  labs(fill="", x = "QA Trading-off with Efficiency", y = "Frequency") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"), text = element_text(size=25)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

vals <- c(8, 4, 1, 1)
val_names <- sprintf("%s (%s)", c("Performance Efficiency", "Maintainability", "Mobility", "Reliability"), scales::percent(round(vals/sum(vals), 2)))
names(vals) <- val_names

waffle::waffle(vals, rows = 2) +
  theme(text = element_text(size=25))


# Plot for frequency of application domain
df_app_domains <- app_domains %>%
  group_by(app_domains$V1) %>%
  summarise(counts = n())
df_app_domains

data_app_domains <- data.frame(
  type=c("Firefighting Robot", "Energy Consumption Analysis", "Service Robot", "Wireless Robot Communication", "Industrial Robot", "Robot Exploration"),
  freq=c(1,2,2,2,3,7)
)
data_app_domains

ggplot(data_app_domains, aes(x=reorder(type, freq), y=freq)) + 
  geom_bar(stat = "identity") +
  labs(fill="", x = "Type of Experiment", y = "Frequency") +
  theme_bw() +
  geom_text(aes(label=freq), vjust=1.6, color="white", size=10) +
  theme(axis.line = element_line(colour = "black"), text = element_text(size=15)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

par(mfrow=c(2,1))
ggplot(data_app_domains, aes(x=reorder(type, freq), y=freq)) + 
  geom_bar(stat = "identity") +
  labs(fill="", x = "Application Domain", y = "Frequency") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"), text = element_text(size=25)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  coord_flip()

## WAFFLE PLOT FOR APP_DOMAINS
vals <- c(7,3,2,2,2,1)
val_names <- sprintf("%s (%s)", c("Robot Exploration", "Industrial Robot", "Energy Consumption Analysis", "Service Robot", "Wireless Robot Communication", "Firefighting Robot"), scales::percent(round(vals/sum(vals), 2)))
names(vals) <- val_names

waffle::waffle(vals, rows = 3) +
  theme(text = element_text(size=25))
# QAs




