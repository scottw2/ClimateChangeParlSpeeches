# climate change UK parl blog
library(tidyverse)
library(extrafont)
library(tidytext)
library(lubridate)
library(ggrepel)
library(patchwork)
library(scales)
library(ggridges)
library(ggalt) 
library(Rtsne)

# Topics
Topics <- read.csv("top_wordsCC1LDAmodelSP2hundred.csv", header = T, sep = ",")
Topics <- as_tibble(Topics)
head(Topics)
Topics <- Topics %>% 
  select(Topic,Word,P)
Topics$Topic <- as.factor(Topics$Topic)

# Top 10 and 15 words per topic
Topics_10 <- Topics %>% 
  group_by(Topic) %>% 
  slice(1:10)

Topics_15 <- Topics %>% 
  group_by(Topic) %>% 
  slice(1:15)

# Termite plot

Termite1 <- Topics_10 %>% 
  mutate(Topic = recode(Topic, "0" = "Wind", "1" = "Transport", "2" = "Conservation & Biodiversity","3" = "Unknown", "4"= "UK and UK Nations","5" = "International Security","6" = "Energy & Consumers" ,"7" = "International Aid", 
                        "8" = "Trade Standards", "9" = "Fossil Fuels & Nuclear", "10" = "Parliamentary Terms 1", "11" = "Economy", "12" = "Parliamentary Terms 2", "13" = "Parliamentary Terms 3",
                        "14" = "Treasury & Budget", "15" = "Local Issues", "16" = "China & HK", "17" = "Flooding & Water", "18" = "Carbon & Net-Zero", "19" = "Emissions Targets")) %>% 
  filter(!Topic %in% c("Parliamentary Terms 1","Parliamentary Terms 2", "Parliamentary Terms 3", "Unknown")) %>%
  mutate(WrappedTopic = str_wrap(Topic, width = 15)) %>% 
  ggplot() +
  geom_point(aes(x = WrappedTopic, y = reorder(Word, P), size = P), alpha = 0.6, colour = "black") +
  scale_size(range = c(1,10)) +
  theme_minimal() +
  theme(plot.title = element_text(family = "Open Sans", face = "bold",hjust = 0.5), 
        plot.subtitle = element_text(family = "Open Sans", hjust = 0.5, ), 
        text = element_text(family = "Open Sans"),
        axis.title = element_text(family = "Open Sans", face = "bold"), 
        plot.caption = element_text(family = "Open Sans", face = "italic"),
        plot.margin = unit( c(0.25,0.25,0.25,0.25), units = "in"),
        axis.text.x = element_text(angle = 45, hjust =1),
        legend.position = "bottom") +
  coord_cartesian(clip = "off") +
  labs(title = "Words In Each Contextual Topic",  subtitle = "Including all terms occuring in the top 10 terms of any topic, sized by probability", x = "", y = "", caption = "Own analysis, data source: https://doi.org/10.5281/zenodo.4066772", size = "Probability")

ggsave(Termite1, filename = "TermiteplotSP2xx.png", dpi = 400,
       width = 10, height = 15, units = "in", type = "cairo")

# Top topics col chart


Topiccol1 <- Topics_10 %>% 
  mutate(Topic = recode(Topic, "0" = "Wind", "1" = "Transport", "2" = "Conservation & Biodiversity","3" = "Unknown", "4"= "UK and UK Nations","5" = "International Security","6" = "Energy & Consumers" ,"7" = "International Aid", 
                        "8" = "Trade Standards", "9" = "Fossil Fuels & Nuclear", "10" = "Parliamentary Terms 1", "11" = "Economy", "12" = "Parliamentary Terms 2", "13" = "Parliamentary Terms 3",
                        "14" = "Treasury & Budget", "15" = "Local Issues", "16" = "China & HK", "17" = "Flooding & Water", "18" = "Carbon & Net-Zero", "19" = "Emissions Targets")) %>% 
  mutate(WrappedTopic = str_wrap(Topic, width = 15)) %>% 
  ggplot() +
  geom_col(aes(x = P, y = reorder_within(Word, P, Topic)), alpha = 1, fill = "black") +
  facet_wrap(~WrappedTopic, scales = "free", ncol = 5) +
  scale_y_reordered() +
  scale_x_continuous(breaks = pretty_breaks(n=3) )+
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(family = "Open Sans", face = "bold",hjust = 0.5), 
        plot.subtitle = element_text(family = "Open Sans", hjust = 0.5, ), 
        text = element_text(family = "Open Sans"),
        axis.title = element_text(family = "Open Sans", face = "bold"), 
        plot.caption = element_text(family = "Open Sans", face = "italic"),
        strip.text= element_text(family = "Open Sans", face = "bold",hjust = 0.5),
        plot.margin = unit( c(0.25,0.25,0.25,0.25), units = "in")) +
  labs(title = "Most Prominent Terms per Topic", subtitle = "Top 10 terms for each topic, ordered by probability of belonging to each topic", y = "Term", x = "Probability" ,caption = "Own analysis, data source: https://doi.org/10.5281/zenodo.4066772")

ggsave(Topiccol1, filename = "TopiccolSP1Bl2x.png", dpi = 400,
       width = 15, height = 10.5, units = "in", type = "cairo")


# SPEECHES

Speeches <- read.csv("CC1TopicsPTSP2x.csv")
Speeches <- as_tibble(Speeches)

Speechesc <- Speeches %>% 
  select(-X3, -X5, -X6)

Speeches$date <- as.Date(Speeches$date, format = c("%Y-%m-%d"))

Speeches <- Speeches %>% 
  mutate(Year = lubridate::year(date))

SpeechesLong <- Speeches %>% 
  pivot_longer(cols = "X0":"X19", names_to = "Topic",
               values_to = "Probability")

parties2keep <- c("Conservative", "Labour", "Labour (Co-op)", "Liberal Democrat", "Green Party", "Scottish National Party")

parties2keepsmall <- c("Conservative", "Labour", "Liberal Democrat", "Scottish National Party")

# colours
BGR7x <- c('#000000', '#555555', '#808080', '#c04040', '#ff0000', '#0000ff','#00cdf8')
BGRz8 <- c('#000000', '#5b5b5b','#00cdf8', '#ff0000', '#ff8d00','#b64949', '#2f510a', '#0000ff')
BGR5z = c('#000000', '#808080', '#ff0000','#0000ff','#00cdf8')

# n speeches over time by party

PartyTime <- Speeches %>% 
  group_by(Year, party) %>% 
  summarise(CountSpeeches = n()) %>% 
  mutate(partycolchart = ifelse(!party %in% parties2keep, "Other", party)) %>% 
  ggplot(aes(x = Year, y = CountSpeeches, fill = partycolchart)) +
  geom_col() +
  scale_fill_manual(values = BGR7x) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(family = "Open Sans", face = "bold",hjust = 0.5), 
        plot.subtitle = element_text(family = "Open Sans", hjust = 0.5, ), 
        text = element_text(family = "Open Sans"),
        axis.title = element_text(family = "Open Sans", face = "bold"), 
        plot.caption = element_text(family = "Open Sans", face = "italic"),
        plot.margin = unit( c(0.25,0.25,0.25,0.25), units = "in")) +
  labs(title = "Number of Speeches Over Time by Party", subtitle = expression(paste("Speeches in UK Parliament which include the term ", italic("climate change")," (1980 - 2020)")) , x = "", y = "Number of Speeches", fill = "Party")


ggsave(PartyTime, filename = "PartyTime6.png", dpi = 400,
       width = 10, height = 7, units = "in", type = "cairo") 

# topics over time

TopicTime <- SpeechesLong %>% 
  mutate(Topic = recode(Topic, "X0" = "Wind", "X1" = "Transport", "X2" = "Conservation & Biodiversity","X3" = "Unknown", "X4"= "UK and UK Nations","X5" = "International Security","X6" = "Energy & Consumers" ,"X7" = "International Aid", 
                        "X8" = "Trade Standards", "X9" = "Fossil Fuels & Nuclear", "X10" = "Parliamentary Terms 1", "X11" = "Economy", "X12" = "Parliamentary Terms 2", "X13" = "Parliamentary Terms 3",
                        "X14" = "Treasury & Budget", "X15" = "Local Issues", "X16" = "China & HK", "X17" = "Flooding & Water", "X18" = "Carbon & Net-Zero", "X19" = "Emissions Targets")) %>% 
  group_by(Year, Topic) %>% 
  summarise(AvProbability = mean(Probability), CountSpeeches = n()) %>% 
  filter(!Topic %in% c("Parliamentary Terms 1","Parliamentary Terms 2", "Parliamentary Terms 3", "Unknown")) %>%
  filter(Year >= 1989) %>% 
 ggplot(aes(x = Year, y = AvProbability)) +
  geom_col(fill = "black") +
  geom_smooth(colour = "red") +
  scale_y_continuous(limits = c(0, NA), oob = squish) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(family = "Open Sans", face = "bold",hjust = 0.5), 
        plot.subtitle = element_text(family = "Open Sans", hjust = 0.5, ), 
        text = element_text(family = "Open Sans"),
        axis.title = element_text(family = "Open Sans", face = "bold"), 
        plot.caption = element_text(family = "Open Sans", face = "italic"),
        plot.margin = unit( c(0.25,0.25,0.25,0.25), units = "in"),
        strip.text= element_text(family = "Open Sans", face = "bold",hjust = 0.5)) +
  labs(title = "How Each Topic's Prevalence Has Changed Over Time", subtitle = "Average prevalence of each topic, from 1989 onward",
       x = "", y = "Av. Prevalence", caption = "Own analysis, data source: https://doi.org/10.5281/zenodo.4066772") +
  facet_wrap(~Topic, scales = "free_y")

ggsave(TopicTime, filename = "TopicTimebl2Zz.png", dpi = 400,
       width = 15, height = 10.5, units = "in", type = "cairo")

# Topic Time Party

TopicTimeParty <- SpeechesLong %>% 
  mutate(Topic = recode(Topic, "X0" = "Wind", "X1" = "Transport", "X2" = "Conservation & Biodiversity","X3" = "Unknown", "X4"= "UK and UK Nations","X5" = "International Security","X6" = "Energy & Consumers" ,"X7" = "International Aid", 
                        "X8" = "Trade Standards", "X9" = "Fossil Fuels & Nuclear", "X10" = "Parliamentary Terms 1", "X11" = "Economy", "X12" = "Parliamentary Terms 2", "X13" = "Parliamentary Terms 3",
                        "X14" = "Treasury & Budget", "X15" = "Local Issues", "X16" = "China & HK", "X17" = "Flooding & Water", "X18" = "Carbon & Net-Zero", "X19" = "Emissions Targets")) %>% 
  mutate(partycolchart = ifelse(!party %in% parties2keepsmall, "Other", party)) %>% 
  group_by(Year, Topic, partycolchart) %>% 
  summarise(AvProbability = mean(Probability), CountSpeeches = n()) %>% 
  filter(!Topic %in% c("Parliamentary Terms 1","Parliamentary Terms 2", "Parliamentary Terms 3", "Unknown")) %>%
  filter(Year >= 1989) %>% 
  ggplot(aes(x = Year, y = AvProbability)) +
  geom_smooth(aes(colour = partycolchart), alpha = 0.15) +
  scale_y_continuous(limits = c(0, NA), oob = squish) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(family = "Open Sans", face = "bold",hjust = 0.5), 
        plot.subtitle = element_text(family = "Open Sans", hjust = 0.5, ), 
        text = element_text(family = "Open Sans"),
        axis.title = element_text(family = "Open Sans", face = "bold"), 
        plot.caption = element_text(family = "Open Sans", face = "italic"),
        plot.margin = unit( c(0.25,0.25,0.25,0.25), units = "in"),
        strip.text= element_text(family = "Open Sans", face = "bold",hjust = 0.5)) +
  labs(title = "How Each Topic's Prevalence Has Changed Over Time by Party", subtitle = "Average prevalence of each topic by party, from 1989 onward",
       x = "", y = "Av. Prevalence", caption = "Own analysis, data source: https://doi.org/10.5281/zenodo.4066772", colour = "Party") +
  facet_wrap(~Topic, scales = "free_y") +
  scale_colour_manual(values = BGR5z)

ggsave(TopicTimeParty, filename = "TopicTimeParty3z.png", dpi = 400,
       width = 15, height = 10.5, units = "in", type = "cairo")

# Top topics overall

TopTopic <- SpeechesLong %>% 
  mutate(Topic = recode(Topic, "X0" = "Wind", "X1" = "Transport", "X2" = "Conservation & Biodiversity","X3" = "Unknown", "X4"= "UK and UK Nations","X5" = "International Security","X6" = "Energy & Consumers" ,"X7" = "International Aid", 
                        "X8" = "Trade Standards", "X9" = "Fossil Fuels & Nuclear", "X10" = "Parliamentary Terms 1", "X11" = "Economy", "X12" = "Parliamentary Terms 2", "X13" = "Parliamentary Terms 3",
                        "X14" = "Treasury & Budget", "X15" = "Local Issues", "X16" = "China & HK", "X17" = "Flooding & Water", "X18" = "Carbon & Net-Zero", "X19" = "Emissions Targets")) %>% 
  group_by(Topic) %>% 
  summarise(AvProbability = mean(Probability)) %>%
  ggplot() +
  geom_lollipop(aes(y = AvProbability, x = reorder(Topic, AvProbability)), alpha = 1, fill = "black", size = 0.75) +
  theme_minimal() +
  theme(legend.position ="bottom",
        plot.title = element_text(family = "Open Sans", face = "bold",hjust = 0.5), 
        plot.subtitle = element_text(family = "Open Sans", hjust = 0.5, ), 
        text = element_text(family = "Open Sans"),
        axis.title = element_text(family = "Open Sans", face = "bold"), 
        plot.caption = element_text(family = "Open Sans", face = "italic"),
        plot.margin = unit( c(0.25,0.25,0.25,0.25), units = "in")) +
  scale_colour_manual(values = BGR5) +
  coord_flip() +
  labs(title = "Most Prevalent Topics", subtitle = "Average prevalence of each topic across all speeches", y = "Average Prevalence", x = "", caption = "Own analysis, data source: https://doi.org/10.5281/zenodo.4066772")

ggsave(TopTopic, filename = "TopTopics1Zx.png", dpi = 400,
       width = 10, height = 7, units = "in", type = "cairo")

# Tory Speeches vs Labour by Topic

AvProbToryLab <- SpeechesLong %>% 
  mutate(Topic = recode(Topic, "X0" = "Wind", "X1" = "Transport", "X2" = "Conservation & Biodiversity","X3" = "Unknown", "X4"= "UK and UK Nations","X5" = "International Security","X6" = "Energy & Consumers" ,"X7" = "International Aid", 
                        "X8" = "Trade Standards", "X9" = "Fossil Fuels & Nuclear", "X10" = "Parliamentary Terms 1", "X11" = "Economy", "X12" = "Parliamentary Terms 2", "X13" = "Parliamentary Terms 3",
                        "X14" = "Treasury & Budget", "X15" = "Local Issues", "X16" = "China & HK", "X17" = "Flooding & Water", "X18" = "Carbon & Net-Zero", "X19" = "Emissions Targets")) %>% 
  filter(party %in% c("Conservative", "Labour")) %>% 
  group_by(Topic) %>% 
  summarise(AvProbabilityTotal = mean(Probability))

ToryvsLabourTopic <- SpeechesLong %>% 
  mutate(Topic = recode(Topic, "X0" = "Wind", "X1" = "Transport", "X2" = "Conservation & Biodiversity","X3" = "Unknown", "X4"= "UK and UK Nations","X5" = "International Security","X6" = "Energy & Consumers" ,"X7" = "International Aid", 
                        "X8" = "Trade Standards", "X9" = "Fossil Fuels & Nuclear", "X10" = "Parliamentary Terms 1", "X11" = "Economy", "X12" = "Parliamentary Terms 2", "X13" = "Parliamentary Terms 3",
                        "X14" = "Treasury & Budget", "X15" = "Local Issues", "X16" = "China & HK", "X17" = "Flooding & Water", "X18" = "Carbon & Net-Zero", "X19" = "Emissions Targets")) %>% 
  filter(party %in% c("Conservative", "Labour")) %>% 
  group_by(Topic, party) %>% 
  summarise(AvProbability = mean(Probability)) %>%
  spread(key = party, value = AvProbability) %>% 
  mutate(ConMinusLabour = Conservative - Labour) %>%
  inner_join(AvProbToryLab) %>% 
  mutate(ConMinusLabourRel = ConMinusLabour/AvProbabilityTotal) %>% 
  ggplot() +
  geom_point(aes(x = ConMinusLabourRel, y = reorder(Topic, ConMinusLabourRel)), size = 2) +
  geom_vline(xintercept = 0, colour = "red", linetype = "dashed") +
  annotate("text", x = 0.4, y = 16, label = "More prevalent in Tory speeches", family = "Open Sans") +
  annotate("text", x = -0.4, y = 5, label = "More prevalent in Labour speeches", family = "Open Sans") +
  annotate("text", x = -0.2, y = 17, label = "Politically neutral", family = "Open Sans") +
  annotate("curve",x= -0.2, y = 16.5, xend = -0.01, yend = 14, curvature = 0.3, arrow = arrow(length = unit(2, "mm")))+
  theme_minimal() +
  theme(plot.title = element_text(family = "Open Sans", face = "bold",hjust = 0.5), 
        plot.subtitle = element_text(family = "Open Sans", hjust = 0.5, ), 
        text = element_text(family = "Open Sans"),
        axis.title = element_text(family = "Open Sans", face = "bold"), 
        plot.caption = element_text(family = "Open Sans", face = "italic"),
        plot.margin = unit( c(0.25,0.25,0.25,0.25), units = "in")) +
  scale_x_continuous(limits = c(-0.6, 0.6)) +
  labs(title = "Topic Distribution Between the Labour and Conservative Parties", 
       subtitle = "Relative average prevalence of each topic appearing in speeches by Conservative MPs vs Labour MPs", x = "Relative Average Prevalence (Conservative - Labour)", y = "",caption = "Own analysis, data source: https://doi.org/10.5281/zenodo.4066772")

ggsave(ToryvsLabourTopic, filename = "ToryvsLabourTopicsC1.png", dpi = 400,
       width = 10, height = 7, units = "in", type = "cairo")  

# all parties - topics by party

TopicParty <- SpeechesLong %>% 
  mutate(Topic = recode(Topic, "X0" = "Wind", "X1" = "Transport", "X2" = "Conservation & Biodiversity","X3" = "Unknown", "X4"= "UK and UK Nations","X5" = "International Security","X6" = "Energy & Consumers" ,"X7" = "International Aid", 
                        "X8" = "Trade Standards", "X9" = "Fossil Fuels & Nuclear", "X10" = "Parliamentary Terms 1", "X11" = "Economy", "X12" = "Parliamentary Terms 2", "X13" = "Parliamentary Terms 3",
                        "X14" = "Treasury & Budget", "X15" = "Local Issues", "X16" = "China & HK", "X17" = "Flooding & Water", "X18" = "Carbon & Net-Zero", "X19" = "Emissions Targets")) %>% 
  mutate(partycolchart = ifelse(!party %in% parties2keep, "Other", party)) %>% 
  group_by(Topic, partycolchart) %>% 
  summarise(AvProbability = mean(Probability)) %>%
  ggplot() +
  geom_point(aes(x = AvProbability, y = Topic, colour = partycolchart), alpha = 0.85, size= 2) +
  theme_minimal() +
  theme(legend.position ="bottom",
        plot.title = element_text(family = "Open Sans", face = "bold",hjust = 0.5), 
        plot.subtitle = element_text(family = "Open Sans", hjust = 0.5, ), 
        text = element_text(family = "Open Sans"),
        axis.title = element_text(family = "Open Sans", face = "bold"), 
        plot.caption = element_text(family = "Open Sans", face = "italic"),
        plot.margin = unit( c(0.25,0.25,0.25,0.25), units = "in")) +
  scale_colour_manual(values = BGR7x) +
  labs(colour = "Party", title = "Which Topics Do Parties Talk About the Most?", subtitle = "Average prevalence of each topic by party", x = "Average Prevalence", y = "", caption = "Own analysis, data source: https://doi.org/10.5281/zenodo.4066772")

ggsave(TopicParty, filename = "TopicParty2Zxbig.png", dpi = 400,
       width = 10, height = 7, units = "in", type = "cairo")  

# Top MPs and per MP prevalence

TopMPS <- SpeechesLong %>% 
  mutate(Topic = recode(Topic, "X0" = "Wind", "X1" = "Transport", "X2" = "Conservation & Biodiversity","X3" = "Unknown", "X4"= "UK and UK Nations","X5" = "International Security","X6" = "Energy & Consumers" ,"X7" = "International Aid", 
                        "X8" = "Trade Standards", "X9" = "Fossil Fuels & Nuclear", "X10" = "Parliamentary Terms 1", "X11" = "Economy", "X12" = "Parliamentary Terms 2", "X13" = "Parliamentary Terms 3",
                        "X14" = "Treasury & Budget", "X15" = "Local Issues", "X16" = "China & HK", "X17" = "Flooding & Water", "X18" = "Carbon & Net-Zero", "X19" = "Emissions Targets")) %>% 
  group_by(Topic, display_as) %>% 
  summarise(AvProbability = mean(Probability), CountSpeeches = n()) %>% 
  ungroup() %>% 
  group_by(Topic) %>% 
  arrange(desc(CountSpeeches)) %>% 
  top_n(n = 100, wt = CountSpeeches) %>% 
  mutate(AvProbabilityRS = rescale(AvProbability)) 


TopMPS50 <- SpeechesLong %>% 
  mutate(Topic = recode(Topic, "X0" = "Wind", "X1" = "Transport", "X2" = "Conservation & Biodiversity","X3" = "Unknown", "X4"= "UK and UK Nations","X5" = "International Security","X6" = "Energy & Consumers" ,"X7" = "International Aid", 
                        "X8" = "Trade Standards", "X9" = "Fossil Fuels & Nuclear", "X10" = "Parliamentary Terms 1", "X11" = "Economy", "X12" = "Parliamentary Terms 2", "X13" = "Parliamentary Terms 3",
                        "X14" = "Treasury & Budget", "X15" = "Local Issues", "X16" = "China & HK", "X17" = "Flooding & Water", "X18" = "Carbon & Net-Zero", "X19" = "Emissions Targets")) %>% 
  group_by(Topic, display_as) %>% 
  summarise(AvProbability = mean(Probability), CountSpeeches = n()) %>% 
  ungroup() %>% 
  group_by(Topic) %>% 
  arrange(desc(CountSpeeches)) %>% 
  top_n(n = 50, wt = CountSpeeches) %>% 
  mutate(AvProbabilityRS = rescale(AvProbability)) 


TopMPSTopic <- TopMPS50 %>% 
  filter(!Topic %in% c("Parliamentary Terms 1","Parliamentary Terms 2", "Parliamentary Terms 3", "Unknown")) %>%
  ggplot() +
  geom_tile(aes(x = Topic, y = reorder(display_as, CountSpeeches), fill = AvProbabilityRS), colour = "white", size = 0.5) +
  guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5, title.position = "top")) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        plot.title = element_text(family = "Open Sans", face = "bold",hjust = 0.5), 
        plot.subtitle = element_text(family = "Open Sans", hjust = 0.5, ), 
        text = element_text(family = "Open Sans"),
        axis.title = element_text(family = "Open Sans", face = "bold"), 
        plot.caption = element_text(family = "Open Sans", face = "italic"),
        plot.margin = unit( c(0.25,0.25,0.25,0.25), units = "in"),
        axis.text.x = element_text(angle = 45, hjust =1)) +
  scale_fill_gradientn(colours = c("black","#808080","#ff0000")) +
  labs(title = "Which Topics Have Prominent MPs Spoken About the Most?", subtitle = "Standardised average prevalence per topic over the top 50 MPs by speech count", x = "", y = "", fill = "Standardised Av. Prev.", caption = "Own analysis, data source: https://doi.org/10.5281/zenodo.4066772")

ggsave(TopMPSTopic, filename = "TopMPsTopic50xWs3.png", dpi = 400,
       width = 10, height = 12, units = "in", type = "cairo")

# Ridges

AvDateTopMPs <- SpeechesLong %>% 
  mutate(Topic = recode(Topic, "X0" = "Wind", "X1" = "Transport", "X2" = "Conservation & Biodiversity","X3" = "Unknown", "X4"= "UK and UK Nations","X5" = "International Security","X6" = "Energy & Consumers" ,"X7" = "International Aid", 
                        "X8" = "Trade Standards", "X9" = "Fossil Fuels & Nuclear", "X10" = "Parliamentary Terms 1", "X11" = "Economy", "X12" = "Parliamentary Terms 2", "X13" = "Parliamentary Terms 3",
                        "X14" = "Treasury & Budget", "X15" = "Local Issues", "X16" = "China & HK", "X17" = "Flooding & Water", "X18" = "Carbon & Net-Zero", "X19" = "Emissions Targets")) %>% 
  filter(display_as %in% TopMPS$display_as) %>% 
  group_by(display_as) %>% 
  summarise(avDate = mean(date))

TopMPsRidgeLine2 <- SpeechesLong %>% 
  mutate(Topic = recode(Topic, "X0" = "Wind", "X1" = "Transport", "X2" = "Conservation & Biodiversity","X3" = "Unknown", "X4"= "UK and UK Nations","X5" = "International Security","X6" = "Energy & Consumers" ,"X7" = "International Aid", 
                        "X8" = "Trade Standards", "X9" = "Fossil Fuels & Nuclear", "X10" = "Parliamentary Terms 1", "X11" = "Economy", "X12" = "Parliamentary Terms 2", "X13" = "Parliamentary Terms 3",
                        "X14" = "Treasury & Budget", "X15" = "Local Issues", "X16" = "China & HK", "X17" = "Flooding & Water", "X18" = "Carbon & Net-Zero", "X19" = "Emissions Targets")) %>% 
  filter(display_as %in% TopMPS$display_as) %>% 
  inner_join(AvDateTopMPs, by = "display_as") %>% 
  ggplot(aes(y = reorder(display_as, desc(avDate)), x = date, fill = stat(x))) +
  geom_density_ridges_gradient() +
  scale_fill_gradientn(colours = c("black","#808080","#ff0000")) +
  guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5, title.position = "top")) +
  theme_minimal() +
  scale_y_discrete(position = "right") +
  theme(legend.position = "none",
        axis.text.y.right = element_text(hjust = 0.5),
        panel.grid = element_blank(),
        plot.title = element_text(family = "Open Sans", face = "bold"), 
        plot.subtitle = element_text(family = "Open Sans" ), 
        text = element_text(family = "Open Sans"),
        axis.title = element_text(family = "Open Sans", face = "bold"), 
        plot.caption = element_text(family = "Open Sans", face = "italic"),
        plot.margin = unit( c(0.25,0,0.25,0.25), units = "in"),
  ) +
  labs(title = "Speeches Over Time for The Top 100 MPs by Speech Count", subtitle = "Ordered and coloured by average speech date (left) and coloured by speech count (right)",
       x = "Date", y = "", caption = "Own analysis, data source: https://doi.org/10.5281/zenodo.4066772", fill = "Date")

SpeechCBars <- Speeches %>% 
  filter(display_as %in% TopMPS$display_as) %>% 
  group_by(display_as) %>% 
  summarise(CountSpeeches = n(),) %>% 
  inner_join(AvDateTopMPs, by = "display_as") %>% 
  ggplot(aes(y = reorder(display_as, desc(avDate)), x = CountSpeeches)) +
  scale_fill_gradientn(colours = c("black","#808080","#ff0000")) +
  geom_col(aes(fill = CountSpeeches))+
  scale_x_continuous(breaks = c(0, 50, 100 ,150)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(family = "Open Sans", face = "bold",hjust = 0.5), 
        plot.subtitle = element_text(family = "Open Sans", hjust = 0.5, ), 
        text = element_text(family = "Open Sans"),
        axis.title = element_text(family = "Open Sans", face = "bold"), 
        plot.caption = element_text(family = "Open Sans", face = "italic"),
        plot.margin = unit( c(0.25,0.25,0.25,0), units = "in")) +
  labs(x = "No. Speeches", y = "")

TopMPsRidgeLineBars <- TopMPsRidgeLine2 + SpeechCBars + plot_layout(widths = c(1, 0.25))

ggsave(TopMPsRidgeLineBars, filename = "TopMPsRidgelineBars2.png", dpi = 400,
       width = 10, height = 15, units = "in", type = "cairo")

# speech length

Speeches["speechlength"] <- sapply(strsplit(Speeches$X0.1, " "), length)

Speeches %>% 
  select(display_as, party, X0.1, speechlength) %>% 
  arrange(desc(speechlength))

AvPartySpLC <- Speeches %>% 
  mutate(partycolchart = ifelse(!party %in% parties2keep, "Other", party)) %>% 
  group_by(partycolchart) %>% 
  summarise(AvSpeechLength = median(speechlength), MeanSpeechLength = mean(speechlength))

SpeechLenghtR <- Speeches %>% 
  mutate(partycolchart = ifelse(!party %in% parties2keep, "Other", party)) %>% 
  inner_join(AvPartySpLC, by = "partycolchart") %>% 
  ggplot(aes(y = speechlength, x = reorder(partycolchart, AvSpeechLength)))+
  geom_jitter(colour = "#ff0000", alpha = 0.2, size = 1, width = 0.135)  +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), colour = "black", fill = NA) +
  geom_boxplot(colour ="black", fill = NA, width = 0.135, outlier.shape = NA ) +
  scale_y_log10(breaks = c(10,25,50, 100, 250, 500, 1000, 2500, 5000)) +
  theme_minimal() +
  coord_flip() +
  theme(legend.position = "none",
        panel.grid.major.y  = element_blank(),
        panel.grid.minor.y  = element_blank(),
        panel.grid.minor.x  = element_blank(),
        plot.title = element_text(family = "Open Sans", face = "bold",hjust = 0.5), 
        plot.subtitle = element_text(family = "Open Sans", hjust = 0.5, ), 
        text = element_text(family = "Open Sans"),
        axis.title = element_text(family = "Open Sans", face = "bold"), 
        plot.caption = element_text(family = "Open Sans", face = "italic"),
        plot.margin = unit( c(0.25,0.25,0.25,0.25), units = "in"),
  ) +
  labs(title = "Distribution of Speech Length by Party", subtitle = "Log scaled raincloud plots, sorted by median",  caption = "Own analysis, data source: https://doi.org/10.5281/zenodo.4066772", y = "Length of Speeches (Terms)", x = "")

ggsave(SpeechLenghtR, filename = "SpeechLengthPartyCleaned2R3.png", dpi = 400,
       width = 10, height = 7, units = "in", type = "cairo") 

#t-SNE

Dominant_Topic_MP <-  SpeechesLong %>% 
  mutate(Topic = recode(Topic, "X0" = "Wind", "X1" = "Transport", "X2" = "Conservation & Biodiversity","X3" = "Unknown", "X4"= "UK and UK Nations","X5" = "International Security","X6" = "Energy & Consumers" ,"X7" = "International Aid", 
                        "X8" = "Trade Standards", "X9" = "Power Generation", "X10" = "Parliamentary Terms 1", "X11" = "Economy", "X12" = "Parliamentary Terms 2", "X13" = "Parliamentary Terms 3",
                        "X14" = "Treasury & Budget", "X15" = "Local Issues", "X16" = "China & HK", "X17" = "Flooding & Water", "X18" = "Carbon & Net-Zero", "X19" = "Emissions Targets")) %>% 
  filter(!Topic %in% c("Parliamentary Terms 1","Parliamentary Terms 2", "Parliamentary Terms 3", "Unknown")) %>%
  group_by(Topic, display_as) %>% 
  summarise(AvProbability = mean(Probability), CountSpeeches = n()) %>%   
  ungroup() %>% 
  group_by(display_as) %>% 
  mutate(Dominant_topic = if_else(AvProbability == max(AvProbability), "Yes", "No")) %>% 
  filter(Dominant_topic == "Yes") %>% 
  dplyr::select(display_as, Topic)

tsne <- Rtsne(SpeechesMDS[-1], dims = 2, perplexity = 50, max_iter = 10000, check_duplicates = FALSE, )
tsne2 <- tsne$Y
tsne2 <- as_tibble(tsne2, .name_repair = "unique") %>% 
  rename(Dim1 =1, Dim2 = 2)
tsne3 <- SpeechesMDSx %>% dplyr::select(display_as, CountSpeeches) %>% 
  cbind(tsne2)
tsne3  <- tsne3 %>% inner_join(Dominant_Topic_MP)

Tsneplot <-  tsne3 %>% 
  mutate(Topic = ifelse(!Topic %in% c("Economy", "Emissions Targets", "Energy & Consumers", "Flooding & Water", "International Aid", "Transport", "Treasury & Budget"), "Other", Topic)) %>% 
  ggplot(aes(x = Dim1, y = Dim2)) +
  geom_point(aes(size = CountSpeeches, colour = Topic),alpha = 0.75) +
  #geom_text(size = 2, colour = "#ff0000", aes(label = display_as)) +
  theme_minimal () +
  scale_size(range = c(1,10), breaks = c(1,50,100,150)) +
  geom_text_repel(data = subset(tsne3, CountSpeeches >= 50), aes(label = display_as), size = 3, colour = "black", box.padding = 0.5) +
  theme(legend.position = "bottom",
        plot.title = element_text(family = "Open Sans", face = "bold",hjust = 0.5), 
        plot.subtitle = element_text(family = "Open Sans", hjust = 0.5, ), 
        text = element_text(family = "Open Sans"),
        axis.title = element_text(family = "Open Sans", face = "bold"), 
        plot.caption = element_text(family = "Open Sans", face = "italic"),
        plot.margin = unit( c(0.25,0.25,0.25,0.25), units = "in")) +
  scale_colour_manual(values = BGRz8) +
  guides(colour = guide_legend(override.aes = list(size=3.5))) +
  labs(title = "MPs Clustered by The Topics They Talk About", subtitle = "2 dimensional t-SNE model of MPs and their average topic prevalence, coloured by dominant topic", x = "Dimension 1", y = "Dimension 2", colour = "Dominant Topic", size = "Number of Speeches", caption = "Own analysis, data source: https://doi.org/10.5281/zenodo.4066772")

ggsave(Tsneplot, filename = "tsneviz3.png", dpi = 400,
       width = 15, height = 10.5, units = "in", type = "cairo")

