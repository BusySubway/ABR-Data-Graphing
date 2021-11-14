abr <- read.csv("abr.csv")
abr$timepoint <- as.character(abr$timepoint)
abr$strain <- NA

#install.packages(c("dplyr", "tidyr", "ggplot2", "ggpubr", "Rmisc"))

#grouping data according to the id value
attach(abr)
abr$strain[id <= 491] <- "C57BL/6J"
abr$strain[id >= 492 & id < 502] <- "CBA/Ca"
abr$strain[id >= 502] <- "L236P-/-"
detach(abr)

#use function "summarySE" summarizing the data
library(Rmisc)
abr_summary <- summarySE(abr, measurevar = 'threshold', groupvars = c('strain','timepoint', 'ear', 'frequency'))
abr_summary

#modify the facet label
timepoint.labs <- c("D0", "D5")
names(timepoint.labs) <- c("0", "5")
ear.labs <- c("Left Ear", "Right Ear")
names(ear.labs) <- c("left", "right")
strain.labs <- c("C57BL/6J", "CBA/Ca", "L236P-/-")
names(strain.labs) <- c("C57BL/6J", "CBA/Ca", "L236P-/-")

#use ggplot2 make a graph to compare the strain difference on hearing
pdf("Strain.pdf")
library(ggplot2)
ggplot(abr_summary, aes(x = frequency/1000, y = threshold, colour = strain, group = strain)) +
  geom_errorbar(aes(ymin = threshold - se, ymax = threshold + se), colour = "black", width = .1) +
  geom_line() +
  geom_point(size = 3, shape = 21, fill = "white") +
  facet_grid(timepoint ~ ear, labeller = labeller(timepoint = timepoint.labs, ear = ear.labs)) +
  xlab("Frequency (kHz)") +
  ylab("Threshold (dB SPL)") +
  scale_colour_hue(name = "Strain", l = 40) +
  ggtitle("Strain Difference on Hearing") +
  expand_limits(y=15) +
  scale_y_continuous(breaks=0:90*10) +
  theme_bw() +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0))
dev.off()

#use ggplot2 make a graph to compare the timepoint difference of hearing
pdf("Timepoint.pdf")
ggplot(abr_summary, aes(x = frequency/1000, y = threshold, colour = timepoint, group = timepoint)) +
  geom_errorbar(aes(ymin = threshold - se, ymax = threshold + se), colour = "black", width = .1) +
  geom_line() +
  geom_point(size = 3, shape = 21, fill = "white") +
  facet_grid(strain ~ ear, labeller = labeller(strain = strain.labs, ear = ear.labs)) +
  xlab("Frequency (kHz)") +
  ylab("Threshold (dB SPL)") +
  scale_colour_hue(name = "Timepoint", l = 40, breaks = c("0", "5"), labels = c("D0", "D5")) +
  ggtitle("Timepoint Difference of Hearing") +
  expand_limits(y=15) +
  scale_y_continuous(breaks=0:90*10) +
  theme_bw() +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0))
dev.off()

#use ggplot2 make a graph to compare the ear difference of hearing
pdf("Ear.pdf")
ggplot(abr_summary, aes(x = frequency/1000, y = threshold, colour = ear, group = ear)) +
  geom_errorbar(aes(ymin = threshold - se, ymax = threshold + se), colour = "black", width = .1) +
  geom_line() +
  geom_point(size = 3, shape = 21, fill = "white") +
  facet_grid(strain ~ timepoint, labeller = labeller(strain = strain.labs, timepoint = timepoint.labs)) +
  xlab("Frequency (kHz)") +
  ylab("Threshold (dB SPL)") +
  scale_colour_hue(name = "Ear", l = 40, breaks = c("left", "right"), labels = c("Left", "Right")) +
  ggtitle("Ear Difference of Hearing") +
  expand_limits(y=15) +
  scale_y_continuous(breaks=0:90*10) +
  theme_bw() +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0))
dev.off()