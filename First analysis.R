#########################################################
####   Script for analysing questionnaire responses  ####
#########################################################

##### script setup ####

### Clear the current workspace (DOESN'T CLEAR LOADED LIBRARIES)
rm(list=ls())

### install if necessary and then load the libraries you need

j <- c("rstudioapi","plyr","dplyr","multcomp","reshape2","vegan","lme4","MASS","effects","ggplot2","ggstance","scales","gridExtra","ggpubr","ggsignif","corrplot","stringr","glmmTMB")

new.packages <- j[!(j %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


lapply(j, require, character.only = TRUE)  # loads up any libraries that aren't already loaded

### set working directory to script's saved location

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## read in some functions we might need

source("CheckResidsFunction.R")
source("CheckConvergenceFunction.R")

### read in the datasets

# main data file

data <- read.csv("../Raw data/Brownfields_raw.csv", header = T)

summary(data)

head(data)

# we want to be able to label these columns more clearly

labels <- read.csv("../Raw data/Brownfields_key_tidied.csv", header = T)

head(labels)

vector_colnames <- labels[,2]
vector_descriptors <- labels[,3]


### Question 1: What features do people associate with brownfield sites?

# start by picking out just the columns associated with q1

data_q1 <- data[,c(1:16, 19, 23)]

# summarise these data

summary_q1 <- data.frame()

for (x in 5:16){
  variable <- vector_colnames[[x]]
  label <- vector_descriptors[[x]]
  perc.chosen <- mean(data_q1[,x])

  out <- cbind(variable,label,perc.chosen)
  summary_q1 <- rbind(summary_q1,out)  
}


summary_q1

summary_q1$Percentage <- as.numeric(summary_q1$perc.chosen)*100

summary_q1$Keyword <- factor(summary_q1$label, levels = c(vector_descriptors[5:16]))

# add a column for whether these features are positive, negative or ambiguous

summary_q1$Group <- as.factor(c("Negative","Positive","Positive","Negative","Positive","Ambiguous",
                      "Negative","Negative","Ambiguous","Positive","Ambiguous","Ambiguous"))


summary(summary_q1)

# plot a bar chart

plot_q1 <- ggplot(summary_q1, 
                  aes(x = Keyword, y = Percentage, fill = Group))+
  geom_bar(stat = "identity",
           colour = "black")+
  theme_classic()+
  scale_fill_manual(values = c("grey70","royalblue","goldenrod"))+
  scale_y_continuous(breaks = seq(0,100,25), limits = c(0,100))+
  labs(x = "Keyword", y = "Percentage of respondents", fill = "Connotations of keyword")+
  theme(text = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),
        panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

plot_q1


# export it

ggsave("Plots/Q1.svg", plot = plot_q1, device = svg, width = 250, height = 150, units = "mm", limitsize = F)




### Q2 - Do people generally feel positive, negative or neutral about a brownfield site in their local area?

summary(data_q1$HBU_Q2) 

q2match <- data.frame(
  HBU_Q2 = c(1:6),
  Opinion = c("Very negative","Fairly negative","Neutral","Fairly positive","Very positive","Don't know")
)

q2match

data_q1 <- merge(data_q1, q2match)

data_q1$Opinion <- factor(data_q1$Opinion, levels = c("Very negative","Fairly negative","Neutral","Fairly positive","Very positive","Don't know"))

summary(data_q1$Opinion)

summary_q2_tab <- table(data_q1$Opinion)

summary_q2_prop <- prop.table(summary_q2_tab)

summary_q2 <- data.frame(summary_q2_prop)

summary_q2$Perc <- summary_q2$Freq * 100

# plot a bar chart

plot_q2 <- ggplot(summary_q2, 
                  aes(x = Var1, y = Perc, fill = Var1))+
  geom_col(colour = "black")+
  theme_classic()+
  scale_fill_manual(values = c("royalblue","skyblue3","paleturquoise","gold","goldenrod","grey70"))+
  labs(x = "Opinion", y = "Percentage of respondents")+
  theme(text = element_text(size = 15),
        panel.grid.major.y = element_line(colour = "grey90", size = 0.5),
        legend.position = "none")

plot_q2


# export it

ggsave("Plots/Q2.svg", plot = plot_q2, device = svg, width = 250, height = 150, units = "mm", limitsize = F)


## Q2a - Does this align with their selected features?

# we want to do some jiggery-pokery here to get an average 'keyword score' for each respondent
# they get +1 for every "positive" keyword and -1 for every "negative" keyword they selected

# first we need to change all 1s to -1s in columns for negative keywords
data_q2 <- data_q1

data_q2$HBU_Q1_1 <- ifelse(data_q2$HBU_Q1_1 == 1, -1, 0) # flytipping
data_q2$HBU_Q1_4 <- ifelse(data_q2$HBU_Q1_4 == 1, -1, 0) # abandoned buildings
data_q2$HBU_Q1_7 <- ifelse(data_q2$HBU_Q1_7 == 1, -1, 0) # overgrown
data_q2$HBU_Q1_8 <- ifelse(data_q2$HBU_Q1_8 == 1, -1, 0) # pollution

# now sum up all the positive/negative columns, avoiding the ambiguous ones

data_q2$KeywordScore <- rowSums(data_q2[,c(6:10,12:13,15)])


## now test the significance using a Gaussian LM (because only one answer per respondent)


mod2a <- lm(KeywordScore ~ Opinion,
            data = data_q2)

summary(mod2a)
drop1(mod2a, test = "F")


summary(glht(mod2a, mcp(Opinion="Tukey")))


labels_q2a <- data.frame(
  Opinion = c("Very negative","Fairly negative","Neutral","Fairly positive","Very positive","Don't know"),
  Label = c("a","b","c","d","d","d")
)


# plot a violin plot

plot_q2a <- ggplot(data_q2, 
                  aes(x = Opinion, y = KeywordScore))+
  geom_violin(trim = T,
              adjust = 2,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  theme_classic()+
  labs(x = "Opinion", y = "Keyword Score", tag = "a")+
  geom_text(data = labels_q2a, aes(x = Opinion, y = 4.5, label = Label))+
  scale_y_continuous(limits = c(-5,5), breaks = c(-4:4))+
  theme(text = element_text(size = 15),
        panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

plot_q2a

# slightly different version
plot_q2a.1 <- ggplot(data_q2, 
                   aes(x = Opinion, y = KeywordScore))+
  geom_point(colour = "grey60", alpha = 0.5,
             position = position_jitter(width = 0.2, height = 0.1))+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  theme_classic()+
  labs(x = "Opinion", y = "Keyword Score", tag = "a")+
  geom_text(data = labels_q2a, aes(x = Opinion, y = 4.5, label = Label))+
  scale_y_continuous(limits = c(-5,5), breaks = c(-4:4))+
  theme(text = element_text(size = 15),
        panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

plot_q2a.1


# export it

ggsave("Plots/Q2a.svg", plot = plot_q2a, device = svg, width = 250, height = 150, units = "mm", limitsize = F)


## focussing only on wildlife-related keywords...

# now sum up all the wildlife-related columns

data_q2$WildlifeScore <- rowSums(data_q2[,c(7, 10:11,15)])


## now test the significance using a Gaussian LM (because only one answer per respondent)


mod2b <- lm(WildlifeScore ~ Opinion,
            data = data_q2)

summary(mod2b)
drop1(mod2b, test = "F")


summary(glht(mod2b, mcp(Opinion="Tukey")))


labels_q2b <- data.frame(
  Opinion = c("Very negative","Fairly negative","Neutral","Fairly positive","Very positive","Don't know"),
  Label = c("ad","a","b","c","c","d")
)


# plot a violin plot

plot_q2b <- ggplot(data_q2, 
                   aes(x = Opinion, y = WildlifeScore))+
  geom_violin(trim = T,
              adjust = 2,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  theme_classic()+
  labs(x = "Opinion", y = "Wildlife-related keywords chosen", tag = "b")+
  geom_text(data = labels_q2b, aes(x = Opinion, y = 4.5, label = Label))+
  scale_y_continuous(limits = c(-1,5), breaks = c(0:4))+
  theme(text = element_text(size = 15),
        panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

plot_q2b


# export it

ggsave("Plots/Q2b.svg", plot = plot_q2a, device = svg, width = 250, height = 150, units = "mm", limitsize = F)


m2 <- grid.arrange(plot_q2a,plot_q2b)


ggsave("Plots/Q2ab.svg", plot = m2, device = svg, width = 250, height = 300, units = "mm", limitsize = F)





summary(data_q2$HBU_Q4) 

q3match <- data.frame(
  HBU_Q4 = c(1:6),
  Change_in_opinion = c("Much more negatively","A little more negatively",
                        "Makes no difference","A little more positively",
                        "Much more positively","Don't know"))

q3match

data_q2 <- merge(data_q2, q3match)

data_q2$Change_in_opinion <- factor(data_q2$Change_in_opinion, levels = c("Much more negatively","A little more negatively",
                                                                          "Makes no difference","A little more positively",
                                                                          "Much more positively","Don't know"))

summary(data_q2$Change_in_opinion)

summary_q3_tab <- table(data_q2$Change_in_opinion)

summary_q3_prop <- prop.table(summary_q3_tab)

summary_q3 <- data.frame(summary_q3_prop)

summary_q3$Perc <- summary_q3$Freq * 100

# plot a bar chart

plot_q3 <- ggplot(data_q2, 
                  aes(x = Change_in_opinion, fill = Change_in_opinion))+
  geom_bar(stat = "count", colour = "black")+
  theme_classic()+
  scale_fill_manual(values = c("royalblue","skyblue3","paleturquoise","gold","goldenrod","grey70"))+
  labs(x = "Change in opinion", y = "Percentage of respondents")+
  theme(text = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.major.y = element_line(colour = "grey90", size = 0.5),
        legend.position = "none")

plot_q3

# export it

ggsave("Plots/Q3.svg", plot = plot_q3, device = svg, width = 250, height = 150, units = "mm", limitsize = F)


# break this down further by how they responded to q2

data_q2$Opinion_simp <- factor(ifelse(data_q2$Opinion %in% c("Very negative","Fairly negative"), "Negative",
                               ifelse(data_q2$Opinion %in% c("Very positive","Fairly positive"), "Positive",
                                      ifelse(data_q2$Opinion %in% c("Neutral","Don't know"), "Neutral","Break"))))

summary(data_q2$Opinion_simp)

# plot a bar chart

plot_q3a <- ggplot(data_q2, 
                  aes(x = Change_in_opinion, fill = Opinion_simp, group = Opinion_simp))+
  geom_bar(stat = "count", colour = "black", position = "dodge")+
  theme_classic()+
  scale_fill_manual(values = c("royalblue","grey70","goldenrod"))+
  labs(x = "Change in opinion", y = "Number of respondents", fill = "Original opinion")+
  theme(text = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

plot_q3a

# export it

ggsave("Plots/Q3a.svg", plot = plot_q3a, device = svg, width = 250, height = 150, units = "mm", limitsize = F)



## do a chi-squared test of this

# summarise the data we need, make it wide-form, and place labels into rownames

data_q2$Count <- 1

summary_q2a <- ddply(data_q2, .(Change_in_opinion,Opinion_simp), summarise,
                    Frequency = sum(Count))

summary_q2a_wide <- dcast(summary_q2a, Opinion_simp ~ Change_in_opinion)

rownames(summary_q2a_wide) <- summary_q2a_wide$Opinion_simp

summary_q2a_wide <- summary_q2a_wide[,-1]


## now construst and assess the test

test_q2a <- chisq.test(summary_q2a_wide)

test_q2a

corrplot(test_q2a$residuals, is.cor = FALSE,
         tl.col = "black",
         cl.pos = "n",
         col=colorRampPalette(c("royalblue","grey70","goldenrod"))(200))

# export that plot

svg(height = 5, width = 10, file = "Plots/Q2acorr.svg")
corrplot(test_q2a$residuals, is.cor = FALSE,
         tl.col = "black",
         cl.pos = "n",
         col=colorRampPalette(c("royalblue","grey70","goldenrod"))(200))
dev.off()




### Q4: Can people correctly identify brownfield vs undeveloped sites from photos?

data_q4 <- data[,c(1,24:31)]

# first, Kiplingcotes Chalk Quarry (brownfield)

data_q4$Brownfield_1 <- factor(ifelse(data_q4$HBU_Q5b == 1, "Brownfield", "Undeveloped")) # Kip
data_q4$Undeveloped_1 <- factor(ifelse(data_q4$HBU_Q6b == 1, "Brownfield", "Undeveloped")) # Ledsham
data_q4$Undeveloped_2 <- factor(ifelse(data_q4$HBU_Q7b == 1, "Brownfield", "Undeveloped")) # Townclose
data_q4$Brownfield_2 <- factor(ifelse(data_q4$HBU_Q8b == 1, "Brownfield", "Undeveloped")) # Barlow

summary(data_q4)


# make them long-form

data_q4_long <- melt(data_q4,
                     id.vars = "RecordNo",
                     measure.vars = c("Brownfield_1","Brownfield_2","Undeveloped_1","Undeveloped_2"),
                     variable.name = "Site",
                     value.name = "Answer")


# plot a bar chart

plot_q4 <- ggplot(data_q4_long, 
                   aes(x = Site, group = Answer, fill = Answer))+
  geom_bar(stat = "count", colour = "black", position = "dodge")+
  theme_classic()+
  scale_fill_manual(values = c("sienna4","springgreen2"))+
  labs(x = "Site depicted", y = "Number of respondents", fill = "Guess")+
  theme(text = element_text(size = 15),
        panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

plot_q4

# export it

ggsave("Plots/Q4.svg", plot = plot_q4, device = svg, width = 250, height = 150, units = "mm", limitsize = F)


## Q4a - does people's positive/negative opinion of brownfield sites affect this?

# rate every answer as correct/incorrect

data_q4_long$Correct <- factor(ifelse(data_q4_long$Site %in% c("Brownfield_1","Brownfield_2") & data_q4_long$Answer == "Brownfield", "Correct",
                                      ifelse(data_q4_long$Site %in% c("Undeveloped_1","Undeveloped_2") & data_q4_long$Answer == "Undeveloped", "Correct", "Incorrect")))

summary(data_q4_long$Correct)


## apply a sign test to see if people are better than chance at identifying brownfield sites

summary(data_q4_long)

binom.test(5005, (5005+3983))

# test whether leaving out Townclose Hills (which most people guessed correctly) changes this

summary(data_q4_long[which(data_q4_long$Site != "Undeveloped_2"), ])

binom.test(3229, (3229+3512))


# merge in people's opinions and keyword scores

opinions <- data_q2[,c(3,19,20,22)]

data_q4_opinions <- merge(data_q4_long, opinions)


# plot a box plot


plot_q4a <- ggplot(data_q4_opinions, 
                  aes(x = Opinion, group = Correct, fill = Correct))+
  geom_bar(stat = "count", colour = "black", position = "dodge")+
  theme_classic()+
  scale_fill_manual(values = c("goldenrod","royalblue"))+
  labs(x = "Answer given", y = "Number of respondents", fill = "Guess")+
  theme(text = element_text(size = 15),
        panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

plot_q4a


# export it

ggsave("Plots/Q4a.svg", plot = plot_q4a, device = svg, width = 250, height = 150, units = "mm", limitsize = F)


## test this using a binomial GLM 
# (not a GLMM despite four answers per respondent, because each respondent is always in the same category of Opinion)
# first need to make the output variable 0/1
data_q4_opinions$CorrectBinom <- ifelse(data_q4_opinions$Correct == "Correct", 1, 0)


mod4a <- glm(CorrectBinom ~ Opinion,
               family = binomial (link = "logit"),
               data = data_q4_opinions)

summary(mod4a)
drop1(mod4a, test = "Chi")




### Q5 - Does a site's actual brownfield vs undeveloped status affect people's opinion of the site?

data_q5 <- data[,c(1,24:31)]


# Brownfield 1

q5match5 <- data.frame(
  HBU_Q5 = c(1:6),
  Brownfield_1 = c("Very negative","Fairly negative","Neutral","Fairly positive","Very positive","Don't know")
)

q5match5


data_q5 <- merge(data_q5, q5match5)

# Brownfield 2

q5match8 <- data.frame(
  HBU_Q8 = c(1:6),
  Brownfield_2 = c("Very negative","Fairly negative","Neutral","Fairly positive","Very positive","Don't know")
)

q5match8


data_q5 <- merge(data_q5, q5match8)


# Undeveloped 1

q5match6 <- data.frame(
  HBU_Q6 = c(1:6),
  Undeveloped_1 = c("Very negative","Fairly negative","Neutral","Fairly positive","Very positive","Don't know")
)

q5match6


data_q5 <- merge(data_q5, q5match6)


# Undeveloped 2

q5match7 <- data.frame(
  HBU_Q7 = c(1:6),
  Undeveloped_2 = c("Very negative","Fairly negative","Neutral","Fairly positive","Very positive","Don't know")
)

q5match7


data_q5 <- merge(data_q5, q5match7)


summary(data_q5)


# make them long-form

data_q5_long <- melt(data_q5,
                     id.vars = "RecordNo",
                     measure.vars = c("Brownfield_1","Brownfield_2","Undeveloped_1","Undeveloped_2"),
                     variable.name = "Site",
                     value.name = "Opinion")




data_q5_long$Opinion <- factor(data_q5_long$Opinion, levels = c("Very negative","Fairly negative","Neutral","Fairly positive","Very positive","Don't know"))

data_q5_long$Type <- factor(ifelse(data_q5_long$Site %in% c("Brownfield_1","Brownfield_2"), "Brownfield","Undeveloped"))

summary(data_q5_long)


# plot a bar chart

plot_q5 <- ggplot(data_q5_long, 
                   aes(x = Site, fill = Opinion, group = Opinion))+
  geom_bar(stat = "count", colour = "black", position = "dodge")+
  theme_classic()+
  scale_fill_manual(values = c("royalblue","skyblue3","paleturquoise","gold","goldenrod","grey70"))+
  labs(x = "Site", y = "Number of respondents", fill = "Opinion")+
  theme(text = element_text(size = 15),
        panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

plot_q5

# export it

ggsave("Plots/Q5.svg", plot = plot_q5, device = svg, width = 250, height = 100, units = "mm", limitsize = F)


# repeat for simpler brownfield vs undeveloped comparison (blind to sites)

plot_q5a <- ggplot(data_q5_long, 
                  aes(x = Type, fill = Opinion, group = Opinion))+
  geom_bar(stat = "count", colour = "black", position = "dodge")+
  theme_classic()+
  scale_fill_manual(values = c("royalblue","skyblue3","paleturquoise","gold","goldenrod","grey70"))+
  labs(x = "Site type", y = "Number of respondents", fill = "Opinion")+
  theme(text = element_text(size = 15),
        panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

plot_q5a

# export it

ggsave("Plots/Q5a.svg", plot = plot_q5a, device = svg, width = 250, height = 100, units = "mm", limitsize = F)


## do a chi-squared test of this

# summarise the data we need, make it wide-form, and place labels into rownames

data_q5_long$Count <- 1

summary_q5 <- ddply(data_q5_long, .(Opinion,Type), summarise,
                    Frequency = sum(Count))

summary_q5_wide <- dcast(summary_q5, Type ~ Opinion)

rownames(summary_q5_wide) <- summary_q5_wide$Type

summary_q5_wide <- summary_q5_wide[,-1]


## now construst and assess the test

test_q5 <- chisq.test(summary_q5_wide)

test_q5

corrplot(test_q5$residuals, is.cor = FALSE,
         tl.col = "black",
         cl.pos = "n",
         col=colorRampPalette(c("royalblue","grey70","goldenrod"))(200))

# export that plot

svg(height = 5, width = 10, file = "Plots/Q5corr.svg")
corrplot(test_q5$residuals, is.cor = FALSE,
         tl.col = "black",
         cl.pos = "n",
         col=colorRampPalette(c("royalblue","grey70","goldenrod"))(200))
dev.off()



### Q6 - Does people's assessment (correct or incorrect) of brownfield vs undeveloped status affect their opinion of the site?


data_q6 <- merge(data_q4_long,data_q5_long)

# make a bar chart

plot_q6 <- ggplot(data_q6, 
                   aes(x = Answer, fill = Opinion, group = Opinion))+
  geom_bar(stat = "count", colour = "black", position = "dodge")+
  theme_classic()+
  scale_fill_manual(values = c("royalblue","skyblue3","paleturquoise","gold","goldenrod","grey70"))+
  labs(x = "Guessed site type", y = "Number of respondents", fill = "Opinion")+
  theme(text = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

plot_q6

# export it

ggsave("Plots/Q6.svg", plot = plot_q6, device = svg, width = 250, height = 100, units = "mm", limitsize = F)


## repeat this with brownfield sites only for a slightly different framing -
# i.e. for *brownfield sites only*, do opinions differ depending on whether people think they are or aren't

data_q6b <- data_q6[which(data_q6$Type == "Brownfield"), ]

summary(data_q6b)


# make a bar chart

plot_q6b <- ggplot(data_q6b, 
                  aes(x = Answer, fill = Opinion, group = Opinion))+
  geom_bar(stat = "count", colour = "black", position = "dodge")+
  theme_classic()+
  scale_fill_manual(values = c("royalblue","skyblue3","paleturquoise","gold","goldenrod","grey70"))+
  labs(x = "Guessed site type", y = "Number of respondents", fill = "Opinion", tag = "a")+
  theme(text = element_text(size = 15),
        panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

plot_q6b

# export it

ggsave("Plots/Q6b.svg", plot = plot_q6b, device = svg, width = 250, height = 100, units = "mm", limitsize = F)



## do a chi-squared test of this

# summarise the data we need, make it wide-form, and place labels into rownames


summary_q6b <- ddply(data_q6b, .(Opinion,Answer), summarise,
                    Frequency = sum(Count))

summary_q6b_wide <- dcast(summary_q6b, Answer ~ Opinion)

rownames(summary_q6b_wide) <- summary_q6b_wide$Answer

summary_q6b_wide <- summary_q6b_wide[,-1]


## now construst and assess the test

test_q6b <- chisq.test(summary_q6b_wide)

test_q6b

corrplot(test_q6b$residuals, is.cor = FALSE,
         tl.col = "black",
         cl.pos = "n",
         col=colorRampPalette(c("royalblue","grey70","goldenrod"))(200))

# export that plot

svg(height = 5, width = 10, file = "Plots/Q6bcorr.svg")
corrplot(test_q6b$residuals, is.cor = FALSE,
         tl.col = "black",
         cl.pos = "n",
         col=colorRampPalette(c("royalblue","grey70","goldenrod"))(200))
dev.off()


## and repeat for undeveloped sites only - this time also exporting multiplots


## repeat this with brownfield sites only for a slightly different framing -
# i.e. for *brownfield sites only*, do opinions differ depending on whether people think they are or aren't

data_q6u <- data_q6[which(data_q6$Type == "Undeveloped"), ]

summary(data_q6u)


# make a bar chart

plot_q6u <- ggplot(data_q6u, 
                   aes(x = Answer, fill = Opinion, group = Opinion))+
  geom_bar(stat = "count", colour = "black", position = "dodge")+
  theme_classic()+
  scale_fill_manual(values = c("royalblue","skyblue3","paleturquoise","gold","goldenrod","grey70"))+
  labs(x = "Guessed site type", y = "Number of respondents", fill = "Opinion", tag = "b")+
  theme(text = element_text(size = 15),
        panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

plot_q6u

# export it

ggsave("Plots/Q6u.svg", plot = plot_q6b, device = svg, width = 250, height = 100, units = "mm", limitsize = F)



## do a chi-squared test of this

# summarise the data we need, make it wide-form, and place labels into rownames


summary_q6u <- ddply(data_q6u, .(Opinion,Answer), summarise,
                     Frequency = sum(Count))

summary_q6u_wide <- dcast(summary_q6u, Answer ~ Opinion)

rownames(summary_q6u_wide) <- summary_q6u_wide$Answer

summary_q6u_wide <- summary_q6u_wide[,-1]


## now construst and assess the test

test_q6u <- chisq.test(summary_q6u_wide)

test_q6u

corrplot(test_q6u$residuals, is.cor = FALSE,
         tl.col = "black",
         cl.pos = "n",
         col=colorRampPalette(c("royalblue","grey70","goldenrod"))(200))

# export that plot

svg(height = 5, width = 10, file = "Plots/Q6ucorr.svg")
corrplot(test_q6u$residuals, is.cor = FALSE,
         tl.col = "black",
         cl.pos = "n",
         col=colorRampPalette(c("royalblue","grey70","goldenrod"))(200))
dev.off()


## make and export the multiplots

m6 <- grid.arrange(plot_q6b, plot_q6u)

#extract legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

legm6 <-g_legend(plot_q6b)

m6 <- grid.arrange(arrangeGrob(plot_q6b + theme(legend.position="none", axis.title.x = element_blank()),
                               plot_q6u + theme(legend.position="none"),
                               ncol=1),
                   legm6, ncol=2,widths=c(8, 2))

ggsave("Plots/Q6bu.svg", plot = m6, device = svg, width = 200, height = 160, units = "mm", limitsize = F)


## and the corrplots

svg(height = 10, width = 10, file = "Plots/Q6bucorr.svg")
par(mfrow = c(2, 1))
corrplot(test_q6b$residuals, is.cor = FALSE,
         tl.col = "black",
         cl.pos = "n",
         col=colorRampPalette(c("royalblue","grey70","goldenrod"))(200))
corrplot(test_q6u$residuals, is.cor = FALSE,
         tl.col = "black",
         cl.pos = "n",
         col=colorRampPalette(c("royalblue","grey70","goldenrod"))(200))
dev.off()




### Q7 - What benefits/disbenefits do people perceive from brownfield sites compared to:
# arable farms, nature reserves, and renewable energy facilities

data_q7 <- data[,c(1, 32:67)]

summary_q7 <- data.frame()
data_q7_long <- data.frame()

for (x in 2:37){
  Variable <- vector_colnames[[(x+30)]]
  Statement <- vector_descriptors[[(x+30)]]
  Type <- ifelse(grepl("Q9",Variable),"Brownfield site",
                 ifelse(grepl("Q10",Variable),"Nature reserve",
                        ifelse(grepl("Q11",Variable), "Arable farm",
                               ifelse(grepl("Q12",Variable), "Renewables facility","Fail"))))
  
  NumberAgreed <- sum(summary(factor(data_q7[,x]))[1:2])
  PropAgreed <- NumberAgreed/2247
  PercAgreed <-  (NumberAgreed*100)/2247
  
  out_summary <- cbind(Variable,Statement,Type,NumberAgreed,PropAgreed,PercAgreed)
  summary_q7 <- rbind(summary_q7,out_summary)
  
  out_long <- data_q7[,c(1,x)]
  colnames(out_long) <- c("RecordNo","Answer")
  out_long$Type <- Type
  out_long$Statement <- Statement
  out_long$Variable <- Variable
  
  data_q7_long <- rbind(data_q7_long, out_long)
}

summary(summary_q7)

summary_q7$PercAgreed <- as.numeric(summary_q7$PercAgreed)
summary_q7$PropAgreed <- as.numeric(summary_q7$PropAgreed)

summary(summary_q7)


summary(data_q7_long)

data_q7_long$Agree <- ifelse(data_q7_long$Answer %in% c(4,5), 1, 0)
data_q7_long$Type <- factor(data_q7_long$Type)
data_q7_long$Statement <- factor(data_q7_long$Statement)
data_q7_long$Variable <- factor(data_q7_long$Variable)

summary(data_q7_long)


# for labelling purposes, we want to label the options A-I
# first generate a vector of the options

# first make a vector of the 9 statements to work through
vector_statements <- levels(droplevels(factor(summary_q7$Statement)))
vector_labels7 <- c("A","B","C","D","E","F","G","H","J")

statements_labels <- data.frame(cbind(vector_statements, vector_labels7))
colnames(statements_labels) <- c("Statement","Tag")


summary_q7 <- merge(summary_q7, statements_labels)
data_q7_long <- merge(data_q7_long, statements_labels)


## for each of these statements, we want to test with a binomial glmm


# A
data_sA <- data_q7_long[which(data_q7_long$Tag == "A"), ]

mod7a <- glmer(Agree ~ Type + (1| RecordNo),
               family = binomial (link = "logit"),
               data = data_sA)

summary(mod7a)
drop1(mod7a, test = "Chi")

summary(glht(mod7a, mcp(Type="Tukey")))

labels_q7a <- data.frame(
  Tag = "A",
  Type = c("Arable farm","Brownfield site","Nature reserve","Renewables facility"),
  Label = c("a","b","c","d")
)


# B
data_sB <- data_q7_long[which(data_q7_long$Tag == "B"), ]

mod7b <- glmer(Agree ~ Type + (1| RecordNo),
               family = binomial (link = "logit"),
               data = data_sB)

summary(mod7b)
drop1(mod7b, test = "Chi")

summary(glht(mod7b, mcp(Type="Tukey")))

labels_q7b <- data.frame(
  Tag = "B",
  Type = c("Arable farm","Brownfield site","Nature reserve","Renewables facility"),
  Label = c("a","b","c","d")
)


# C
data_sC <- data_q7_long[which(data_q7_long$Tag == "C"), ]

mod7c <- glmer(Agree ~ Type + (1| RecordNo),
               family = binomial (link = "logit"),
               data = data_sC)

summary(mod7c)
drop1(mod7c, test = "Chi")

summary(glht(mod7c, mcp(Type="Tukey")))

labels_q7c <- data.frame(
  Tag = "C",
  Type = c("Arable farm","Brownfield site","Nature reserve","Renewables facility"),
  Label = c("a","b","c","c")
)


# D
data_sD <- data_q7_long[which(data_q7_long$Tag == "D"), ]

mod7d <- glmer(Agree ~ Type + (1| RecordNo),
               family = binomial (link = "logit"),
               data = data_sD)

summary(mod7d)
drop1(mod7d, test = "Chi")

summary(glht(mod7d, mcp(Type="Tukey")))

labels_q7d <- data.frame(
  Tag = "D",
  Type = c("Arable farm","Brownfield site","Nature reserve","Renewables facility"),
  Label = c("a","b","c","c")
)


# E
data_sE <- data_q7_long[which(data_q7_long$Tag == "E"), ]

mod7e <- glmer(Agree ~ Type + (1| RecordNo),
               family = binomial (link = "logit"),
               data = data_sE)

summary(mod7e)
drop1(mod7e, test = "Chi")

summary(glht(mod7e, mcp(Type="Tukey")))

labels_q7e <- data.frame(
  Tag = "E",
  Type = c("Arable farm","Brownfield site","Nature reserve","Renewables facility"),
  Label = c("a","b","c","d")
)



# F
data_sF <- data_q7_long[which(data_q7_long$Tag == "F"), ]

mod7f <- glmer(Agree ~ Type + (1| RecordNo),
               family = binomial (link = "logit"),
               data = data_sF)

summary(mod7f)
drop1(mod7f, test = "Chi")

summary(glht(mod7f, mcp(Type="Tukey")))

labels_q7f <- data.frame(
  Tag = "F",
  Type = c("Arable farm","Brownfield site","Nature reserve","Renewables facility"),
  Label = c("a","b","c","d")
)


# G
data_sG <- data_q7_long[which(data_q7_long$Tag == "G"), ]

mod7g <- glmer(Agree ~ Type + (1| RecordNo),
               family = binomial (link = "logit"),
               data = data_sG)

summary(mod7g)
drop1(mod7g, test = "Chi")

summary(glht(mod7g, mcp(Type="Tukey")))

labels_q7g <- data.frame(
  Tag = "G",
  Type = c("Arable farm","Brownfield site","Nature reserve","Renewables facility"),
  Label = c("a","b","a","c")
)




# H
data_sH <- data_q7_long[which(data_q7_long$Tag == "H"), ]

mod7h <- glmer(Agree ~ Type + (1| RecordNo),
               family = binomial (link = "logit"),
               data = data_sH)

summary(mod7h)
drop1(mod7h, test = "Chi")

summary(glht(mod7h, mcp(Type="Tukey")))

labels_q7h <- data.frame(
  Tag = "H",
  Type = c("Arable farm","Brownfield site","Nature reserve","Renewables facility"),
  Label = c("a","a","b","c")
)



# J
data_sJ <- data_q7_long[which(data_q7_long$Tag == "J"), ]

mod7j <- glmer(Agree ~ Type + (1| RecordNo),
               family = binomial (link = "logit"),
               data = data_sJ)

summary(mod7j)
drop1(mod7j, test = "Chi")

summary(glht(mod7j, mcp(Type="Tukey")))

labels_q7j <- data.frame(
  Tag = "J",
  Type = c("Arable farm","Brownfield site","Nature reserve","Renewables facility"),
  Label = c("a","b","c","d")
)


# now bind all those labels together

labels_q7 <- rbind(labels_q7a, labels_q7b, labels_q7c,
                   labels_q7d, labels_q7e, labels_q7f,
                   labels_q7g, labels_q7h, labels_q7j)


summary_q7 <- merge(summary_q7, labels_q7)

# quick little loop to pick a height at which to set each set of labels on the graph

summary_q7_max <- ddply(summary_q7, .(Statement), summarise,
                        Height = max(PercAgreed))

summary_q7 <- merge(summary_q7, summary_q7_max)
summary_q7$Statement_wrap <- str_wrap(summary_q7$Statement, width = 30)


# make a bar plot

plot_q7 <- ggplot(summary_q7, 
                  aes(x = Statement_wrap, fill = Type, group = Type, y = PercAgreed))+
  geom_bar(stat = "identity", colour = "black", position = "dodge")+
  geom_text(aes(x = Statement_wrap, y = 10+Height, group = Type, label = Label),
            position = position_dodge(width = 0.9))+
  theme_classic()+
  scale_fill_manual(values = c("gold","sienna4","springgreen2","skyblue3"))+
  labs(x = "Statement", y = "Percentage of respondents agreeing", fill = "Site type")+
  scale_y_continuous(limits = c(0,100))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5),
        text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1))

plot_q7


# export it

ggsave("Plots/Q7.svg", plot = plot_q7, device = svg, width = 250, height = 150, units = "mm", limitsize = F)



### Q8 - From the specified options, what do people prefer in terms of future land use?

data_q8 <- data[,c(1, 68:99)]

summary_q8 <- data.frame()

for (x in 2:33){
  Variable <- vector_colnames[[(x+66)]]
  Option <- vector_descriptors[[(x+66)]]
  Type <- ifelse(grepl("Q13",Variable),"Brownfield",
                 ifelse(grepl("Q14",Variable),"Undeveloped",
                        ifelse(grepl("Q15",Variable), "Brownfield",
                               ifelse(grepl("Q16",Variable), "Undeveloped","Fail"))))
  
  WildlifeValue <- ifelse(grepl("Q13",Variable),"Little",
                 ifelse(grepl("Q14",Variable),"Little",
                        ifelse(grepl("Q15",Variable), "Rich",
                               ifelse(grepl("Q16",Variable), "Rich","Fail"))))
  
  MeanRanking <- mean(data_q8[,x])
  SERanking <- sd(data_q8[,x])/sqrt(2247)
  
  out <- cbind(Variable,Option,Type,WildlifeValue,MeanRanking,SERanking)
  summary_q8 <- rbind(summary_q8,out)  
}

summary(summary_q8)

summary_q8$MeanRanking <- as.numeric(summary_q8$MeanRanking)
summary_q8$SERanking <- as.numeric(summary_q8$SERanking)

summary_q8$Option <- factor(summary_q8$Option)
summary_q8$Type <- factor(summary_q8$Type)
summary_q8$WildlifeValue <- factor(summary_q8$WildlifeValue)

summary(summary_q8)


# for labelling purposes, we want to label the options A-H
# first generate a vector of the options

# first make a vector of the 8 options to work through
vector_options <- levels(droplevels(summary_q8$Option))
vector_labels <- c("A","B","C","D","E","F","G","H")

option_labels <- data.frame(cbind(vector_options, vector_labels))
colnames(option_labels) <- c("Option","Tag")


summary_q8 <- merge(summary_q8, option_labels)


## first, compare the mean rankings for each type of site regardless of site features 

plot_q8 <- ggplot(summary_q8, 
                   aes(x = Tag, y = MeanRanking))+
  geom_point(colour = "black")+
  geom_boxplot(alpha = 0.2)+
  theme_classic()+
  labs(x = "Land use option", y = "Mean ranking")+
  scale_y_continuous(limits = c(1,8))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

plot_q8


## on review, I'd rather show the raw data here, so let's try to make that happen
# probably need to start by making the data long-form

# looking at the data structure, we have four sites, eight options per site, and a ranking (1-8) in each cell
# tbh it might possibly be easiest to wrap this all up in a slightly clunky loop

data_q8_long <- data.frame()

# first loop over sites - for which I'll need a vector of key character strings (easy enough)

vector_sites <- c("Q13","Q14","Q15","Q16")
vector_types <- c("Brownfield","Undeveloped","Brownfield","Undeveloped")
vector_wildlife <- c("Little","Little","Rich","Rich")


for (x in 1:4){
  
  data_site <- data.frame(data_q8[,1])
  colnames(data_site) <- "RecordNo"
  
  data_site$Site <- vector_sites[[x]]
  data_site$Type <- vector_types[[x]]
  data_site$WildlifeValue <- vector_wildlife[[x]]
  
  # mini-loop to pick out columns associated with that site by their colnames
  for (y in 2:33){
    
    data_site_col <- data_q8[,c(1,y)]
    
    if (grepl(vector_sites[[x]], colnames(data_site_col)[2])){
      data_site <- merge(data_site,data_site_col)
    }
  }
  
  # now melt everything into longform by option
  
  data_site_long <- melt(data_site,
                       id.vars = c(1:4),
                       measure.vars = c(5:12),
                       variable.name = "QuestionNo",
                       value.name = "Ranking")
  
  # and export the output
  
  data_q8_long <- rbind(data_q8_long, data_site_long)
  
}

data_q8_long$Site <- factor(data_q8_long$Site)
data_q8_long$Type <- factor(data_q8_long$Type)
data_q8_long$WildlifeValue <- factor(data_q8_long$WildlifeValue)

summary(data_q8_long)

# excellent!
# next step is to match up the questions with their actual text
# this information is actually there in "labels"

data_q8_lab <- merge(data_q8_long, labels, by.x = "QuestionNo", by.y = "Variable")

data_q8_lab$Label <- factor(data_q8_lab$Label)

summary(data_q8_lab)


data_q8_lab <- merge(data_q8_lab, option_labels, by.x = "Label", by.y = "Option")


## construct and test a model - a GLMM as multiple rows per respondent

hist(data_q8_lab$Ranking) # we might able to get away with a Gaussian model since it's a uniform distribution - 
# but need to check residuals carefully

# update Oct 2021: beta models are now broken, it seems, so I have to rejig the next bit of code a bit to get it working again
# first, standardize the rankings between 0 and 1

standardize.var <- function(x){(x-min(x))/(max(x)-min(x))}

data_q8_lab$Ranking.std <- standardize.var(data_q8_lab$Ranking)

summary(data_q8_lab$Ranking.std)

mod8 <- lmer(Ranking ~ Tag + (1|RecordNo),
             data = data_q8_lab)

summary(mod8)
drop1(mod8, test = "Chi")

chkres(mod8)  # residuals are not perfect but also not too bad

summary(glht(mod8, mcp(Tag="Tukey")))

data_q8_lab$Label_wrap <- str_wrap(data_q8_lab$Label, width = 30)

labels_q8 <- data.frame(
  Label_wrap = unique(data_q8_lab$Label_wrap),
  SigMark = c("a","b","c","d","e","f","g","g")
)



## great! Now we're ready to try plotting that figure from above again, this time with raw data...

plot_q8a <- ggplot(data_q8_lab, 
                  aes(x = Label_wrap, y = Ranking))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  geom_text(data = labels_q8, aes(x = Label_wrap, y = 8.5, label = SigMark))+
  theme_classic()+
  labs(x = "Land use option", y = "Ranking")+
  scale_y_continuous(limits = c(0,9), breaks = c(1:8))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5),
        text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1))

plot_q8a


ggsave("Plots/Q8.svg", plot = plot_q8a, device = svg, width = 250, height = 180, units = "mm", limitsize = F)



# plot the summary averages + SE (from above) for each of the four sites and multiplot them

summary_q8$MinRanking <- summary_q8$MeanRanking - (2*summary_q8$SERanking)
summary_q8$MaxRanking <- summary_q8$MeanRanking + (2*summary_q8$SERanking)


plot_q8b <- ggplot(summary_q8, 
                   aes(x = Tag, y = MeanRanking))+
  geom_point(colour = "black")+
  geom_errorbar(aes(ymin = MinRanking, ymax = MaxRanking))+
  theme_classic()+
  labs(x = "Land use option", y = "Mean ranking")+
  scale_y_continuous(limits = c(0,9), breaks = c(1:8))+
  facet_wrap(~Type*WildlifeValue, ncol = 2, scales = "free_x")+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

plot_q8b




## hmm - try this as points and violins instead, see if it looks better...

plot_q8b1 <- ggplot(data_q8_lab, 
                   aes(x = Tag, y = Ranking))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  theme_classic()+
  labs(x = "Land use option", y = "Ranking")+
  scale_y_continuous(limits = c(0,9), breaks = c(1:8))+
  facet_wrap(~Type*WildlifeValue, ncol = 2, scales = "free_x")+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

plot_q8b1 ## much clearer



ggsave("Plots/Q8b.svg", plot = plot_q8b, device = svg, width = 250, height = 200, units = "mm", limitsize = F)
ggsave("Plots/Q8b1.svg", plot = plot_q8b1, device = svg, width = 250, height = 200, units = "mm", limitsize = F)




### Q8a - Is this affected by brownfield vs undeveloped status?
### Q8b - Is this affected by wildlife value?

# for each of the 8 options, we want to compare rankings between different site types
# as laborious as it might seem, I think this has to be done manually outside a loop


## option 1

option1 <- vector_options[[1]]
option1

data_option1 <- data_q8_lab[which(data_q8_lab$Label == option1), ]


# run a Wilcoxon test

data_option_TypeCast1 <- dcast(data_option1, RecordNo * WildlifeValue ~ Type, value.var = "Ranking")

w1a <- wilcox.test(data_option_TypeCast1$Undeveloped, data_option_TypeCast1$Brownfield, paired = TRUE)
w1a

t1a <- t.test(data_option_TypeCast1$Undeveloped, data_option_TypeCast1$Brownfield, paired = TRUE)
t1a



# first plot brownfield vs undeveloped ranking

p1a <- ggplot(data_option1, 
              aes(x = Type, y = Ranking))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  geom_signif(comparisons = list(c("Brownfield","Undeveloped")),
              annotations = "***", tip_length = 0, y_position = 8.5)+
  theme_classic()+
  labs(x = "Site type", y = "Ranking")+
  scale_y_continuous(limits = c(0,9), breaks = c(1:8))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5),
        text = element_text(size = 15))

p1a


# then plot little vs rich wildlife ranking

p1b <- ggplot(data_option1, 
              aes(x = WildlifeValue, y = Ranking))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  geom_signif(comparisons = list(c("Little","Rich")),
              annotations = "***", tip_length = 0, y_position = 8.5)+
  theme_classic()+
  labs(x = "Site wildlife value", y = "Ranking")+
  scale_y_continuous(limits = c(0,9), breaks = c(1:8))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5),
        text = element_text(size = 15))

p1b

m1 <- grid.arrange(p1a+theme(axis.title.x = element_blank()),
                   p1b+theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank()), 
                   ncol = 2, top = option1)

# run a Wilcoxon test

data_option_ValueCast1 <- dcast(data_option1, RecordNo * Type ~ WildlifeValue, value.var = "Ranking")

w1b <- wilcox.test(data_option_ValueCast1$Rich, data_option_ValueCast1$Little, paired = TRUE)
w1b

t1b <- t.test(data_option_ValueCast1$Rich, data_option_ValueCast1$Little, paired = TRUE)
t1b


## option 2

option2 <- vector_options[[2]]
option2

data_option2 <- data_q8_lab[which(data_q8_lab$Label == option2), ]

# first plot brownfield vs undeveloped ranking

p2a <- ggplot(data_option2, 
              aes(x = Type, y = Ranking))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  geom_signif(comparisons = list(c("Brownfield","Undeveloped")),
              annotations = "NS", tip_length = 0, y_position = 8.5)+
  theme_classic()+
  labs(x = "Site type", y = "Ranking")+
  scale_y_continuous(limits = c(0,9), breaks = c(1:8))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5),
        text = element_text(size = 15))

p2a

# run a Wilcoxon test

data_option_TypeCast2 <- dcast(data_option2, RecordNo * WildlifeValue ~ Type, value.var = "Ranking")

w2a <- wilcox.test(data_option_TypeCast2$Undeveloped, data_option_TypeCast2$Brownfield, paired = TRUE)
w2a

t2a <- t.test(data_option_TypeCast2$Undeveloped, data_option_TypeCast2$Brownfield, paired = TRUE)
t2a



# then plot little vs rich wildlife ranking

p2b <- ggplot(data_option2, 
              aes(x = WildlifeValue, y = Ranking))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  geom_signif(comparisons = list(c("Little","Rich")),
              annotations = "***", tip_length = 0, y_position = 8.5)+
  theme_classic()+
  labs(x = "Site wildlife value", y = "Ranking")+
  scale_y_continuous(limits = c(0,9), breaks = c(1:8))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5),
        text = element_text(size = 15))

p2b

m2 <- grid.arrange(p2a+theme(axis.title.x = element_blank()),
                   p2b+theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank()), 
                   ncol = 2, top = option2)

# run a Wilcoxon test

data_option_ValueCast2 <- dcast(data_option2, RecordNo * Type ~ WildlifeValue, value.var = "Ranking")

w2b <- wilcox.test(data_option_ValueCast2$Rich, data_option_ValueCast2$Little, paired = TRUE)
w2b

t2b <- t.test(data_option_ValueCast2$Rich, data_option_ValueCast2$Little, paired = TRUE)
t2b


## option 3

option3 <- vector_options[[3]]
option3

data_option3 <- data_q8_lab[which(data_q8_lab$Label == option3), ]

# first plot brownfield vs undeveloped ranking

p3a <- ggplot(data_option3, 
              aes(x = Type, y = Ranking))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  geom_signif(comparisons = list(c("Brownfield","Undeveloped")),
              annotations = "***", tip_length = 0, y_position = 8.5)+
  theme_classic()+
  labs(x = "Site type", y = "Ranking")+
  scale_y_continuous(limits = c(0,9), breaks = c(1:8))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5),
        text = element_text(size = 15))

p3a

# run a Wilcoxon test

data_option_TypeCast3 <- dcast(data_option3, RecordNo * WildlifeValue ~ Type, value.var = "Ranking")

w3a <- wilcox.test(data_option_TypeCast3$Undeveloped, data_option_TypeCast3$Brownfield, paired = TRUE)
w3a

t3a <- t.test(data_option_TypeCast3$Undeveloped, data_option_TypeCast3$Brownfield, paired = TRUE)
t3a


# then plot little vs rich wildlife ranking

p3b <- ggplot(data_option3, 
              aes(x = WildlifeValue, y = Ranking))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  geom_signif(comparisons = list(c("Little","Rich")),
              annotations = "***", tip_length = 0, y_position = 8.5)+
  theme_classic()+
  labs(x = "Site wildlife value", y = "Ranking")+
  scale_y_continuous(limits = c(0,9), breaks = c(1:8))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5),
        text = element_text(size = 15))

p3b

m3 <- grid.arrange(p3a+theme(axis.title.x = element_blank()),
                   p3b+theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank()), 
                   ncol = 2, top = option3)

# run a Wilcoxon test

data_option_ValueCast3 <- dcast(data_option3, RecordNo * Type ~ WildlifeValue, value.var = "Ranking")

w3b <- wilcox.test(data_option_ValueCast3$Rich, data_option_ValueCast3$Little, paired = TRUE)
w3b

t3b <- t.test(data_option_ValueCast3$Rich, data_option_ValueCast3$Little, paired = TRUE)
t3b


## option 4

option4 <- vector_options[[4]]
option4

data_option4 <- data_q8_lab[which(data_q8_lab$Label == option4), ]

# first plot brownfield vs undeveloped ranking

p4a <- ggplot(data_option4, 
              aes(x = Type, y = Ranking))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  geom_signif(comparisons = list(c("Brownfield","Undeveloped")),
              annotations = "***", tip_length = 0, y_position = 8.5)+
  theme_classic()+
  labs(x = "Site type", y = "Ranking")+
  scale_y_continuous(limits = c(0,9), breaks = c(1:8))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5),
        text = element_text(size = 15))

p4a


# run a Wilcoxon test

data_option_TypeCast4 <- dcast(data_option4, RecordNo * WildlifeValue ~ Type, value.var = "Ranking")

w4a <- wilcox.test(data_option_TypeCast4$Undeveloped, data_option_TypeCast4$Brownfield, paired = TRUE)
w4a

t4a <- t.test(data_option_TypeCast4$Undeveloped, data_option_TypeCast4$Brownfield, paired = TRUE)
t4a


# then plot little vs rich wildlife ranking

p4b <- ggplot(data_option4, 
              aes(x = WildlifeValue, y = Ranking))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  geom_signif(comparisons = list(c("Little","Rich")),
              annotations = "***", tip_length = 0, y_position = 8.5)+
  theme_classic()+
  labs(x = "Site wildlife value", y = "Ranking")+
  scale_y_continuous(limits = c(0,9), breaks = c(1:8))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5),
        text = element_text(size = 15))

p4b

m4 <- grid.arrange(p4a+theme(axis.title.x = element_blank()),
                   p4b+theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank()), 
                   ncol = 2, top = option4)

# run a Wilcoxon test

data_option_ValueCast4 <- dcast(data_option4, RecordNo * Type ~ WildlifeValue, value.var = "Ranking")

w4b <- wilcox.test(data_option_ValueCast4$Rich, data_option_ValueCast4$Little, paired = TRUE)
w4b

t4b <- t.test(data_option_ValueCast4$Rich, data_option_ValueCast4$Little, paired = TRUE)
t4b



## option 5

option5 <- vector_options[[5]]
option5

data_option5 <- data_q8_lab[which(data_q8_lab$Label == option5), ]

# first plot brownfield vs undeveloped ranking

p5a <- ggplot(data_option5, 
              aes(x = Type, y = Ranking))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  geom_signif(comparisons = list(c("Brownfield","Undeveloped")),
              annotations = "NS", tip_length = 0, y_position = 8.5)+
  theme_classic()+
  labs(x = "Site type", y = "Ranking")+
  scale_y_continuous(limits = c(0,9), breaks = c(1:8))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5),
        text = element_text(size = 15))

p5a

# run a Wilcoxon test

data_option_TypeCast5 <- dcast(data_option5, RecordNo * WildlifeValue ~ Type, value.var = "Ranking")

w5a <- wilcox.test(data_option_TypeCast5$Undeveloped, data_option_TypeCast5$Brownfield, paired = TRUE)
w5a

t5a <- t.test(data_option_TypeCast5$Undeveloped, data_option_TypeCast5$Brownfield, paired = TRUE)
t5a


# then plot little vs rich wildlife ranking

p5b <- ggplot(data_option5, 
              aes(x = WildlifeValue, y = Ranking))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  geom_signif(comparisons = list(c("Little","Rich")),
              annotations = "***", tip_length = 0, y_position = 8.5)+
  theme_classic()+
  labs(x = "Site wildlife value", y = "Ranking")+
  scale_y_continuous(limits = c(0,9), breaks = c(1:8))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5),
        text = element_text(size = 15))

p5b

m5 <- grid.arrange(p5a+theme(axis.title.x = element_blank()),
                   p5b+theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank()), 
                   ncol = 2, top = option5)

# run a Wilcoxon test

data_option_ValueCast5 <- dcast(data_option5, RecordNo * Type ~ WildlifeValue, value.var = "Ranking")

w5b <- wilcox.test(data_option_ValueCast5$Rich, data_option_ValueCast5$Little, paired = TRUE)
w5b

t5b <- t.test(data_option_ValueCast5$Rich, data_option_ValueCast5$Little, paired = TRUE)
t5b


## option 6

option6 <- vector_options[[6]]
option6

data_option6 <- data_q8_lab[which(data_q8_lab$Label == option6), ]

# first plot brownfield vs undeveloped ranking

p6a <- ggplot(data_option6, 
              aes(x = Type, y = Ranking))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  geom_signif(comparisons = list(c("Brownfield","Undeveloped")),
              annotations = "NS", tip_length = 0, y_position = 8.5)+
  theme_classic()+
  labs(x = "Site type", y = "Ranking")+
  scale_y_continuous(limits = c(0,9), breaks = c(1:8))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5),
        text = element_text(size = 15))

p6a

# run a Wilcoxon test

data_option_TypeCast6 <- dcast(data_option6, RecordNo * WildlifeValue ~ Type, value.var = "Ranking")

w6a <- wilcox.test(data_option_TypeCast6$Undeveloped, data_option_TypeCast6$Brownfield, paired = TRUE)
w6a

t6a <- t.test(data_option_TypeCast6$Undeveloped, data_option_TypeCast6$Brownfield, paired = TRUE)
t6a


# then plot little vs rich wildlife ranking

p6b <- ggplot(data_option6, 
              aes(x = WildlifeValue, y = Ranking))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  geom_signif(comparisons = list(c("Little","Rich")),
              annotations = "***", tip_length = 0, y_position = 8.5)+
  theme_classic()+
  labs(x = "Site wildlife value", y = "Ranking")+
  scale_y_continuous(limits = c(0,9), breaks = c(1:8))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5),
        text = element_text(size = 15))

p6b

m6 <- grid.arrange(p6a+theme(axis.title.x = element_blank()),
                   p6b+theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank()), 
                   ncol = 2, top = option6)

# run a Wilcoxon test

data_option_ValueCast6 <- dcast(data_option6, RecordNo * Type ~ WildlifeValue, value.var = "Ranking")

w6b <- wilcox.test(data_option_ValueCast6$Rich, data_option_ValueCast6$Little, paired = TRUE)
w6b

t6b <- t.test(data_option_ValueCast6$Rich, data_option_ValueCast6$Little, paired = TRUE)
t6b



## option 7

option7 <- vector_options[[7]]
option7

data_option7 <- data_q8_lab[which(data_q8_lab$Label == option7), ]

# first plot brownfield vs undeveloped ranking

p7a <- ggplot(data_option7, 
              aes(x = Type, y = Ranking))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  geom_signif(comparisons = list(c("Brownfield","Undeveloped")),
              annotations = "***", tip_length = 0, y_position = 8.5)+
  theme_classic()+
  labs(x = "Site type", y = "Ranking")+
  scale_y_continuous(limits = c(0,9), breaks = c(1:8))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5),
        text = element_text(size = 15))

p7a




# run a Wilcoxon test

data_option_TypeCast7 <- dcast(data_option7, RecordNo * WildlifeValue ~ Type, value.var = "Ranking")

w7a <- wilcox.test(data_option_TypeCast7$Undeveloped, data_option_TypeCast7$Brownfield, paired = TRUE)
w7a

t7a <- t.test(data_option_TypeCast7$Undeveloped, data_option_TypeCast7$Brownfield, paired = TRUE)
t7a


# then plot little vs rich wildlife ranking

p7b <- ggplot(data_option7, 
              aes(x = WildlifeValue, y = Ranking))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  geom_signif(comparisons = list(c("Little","Rich")),
              annotations = "***", tip_length = 0, y_position = 8.5)+
  theme_classic()+
  labs(x = "Site wildlife value", y = "Ranking")+
  scale_y_continuous(limits = c(0,9), breaks = c(1:8))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5),
        text = element_text(size = 15))

p7b

m7 <- grid.arrange(p7a,
                   p7b+theme(axis.title.y = element_blank()), 
                   ncol = 2, top = option7)

# run a Wilcoxon test

data_option_ValueCast7 <- dcast(data_option7, RecordNo * Type ~ WildlifeValue, value.var = "Ranking")

w7b <- wilcox.test(data_option_ValueCast7$Rich, data_option_ValueCast7$Little, paired = TRUE)
w7b

t7b <- t.test(data_option_ValueCast7$Rich, data_option_ValueCast7$Little, paired = TRUE)
t7b


## option 8

option8 <- vector_options[[8]]
option8

data_option8 <- data_q8_lab[which(data_q8_lab$Label == option8), ]

# first plot brownfield vs undeveloped ranking

p8a <- ggplot(data_option8, 
              aes(x = Type, y = Ranking))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  geom_signif(comparisons = list(c("Brownfield","Undeveloped")),
              annotations = "NS", tip_length = 0, y_position = 8.5)+
  theme_classic()+
  labs(x = "Site type", y = "Ranking")+
  scale_y_continuous(limits = c(0,9), breaks = c(1:8))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5),
        text = element_text(size = 15))

p8a

# run a Wilcoxon test

data_option_TypeCast8 <- dcast(data_option8, RecordNo * WildlifeValue ~ Type, value.var = "Ranking")

w8a <- wilcox.test(data_option_TypeCast8$Undeveloped, data_option_TypeCast8$Brownfield, paired = TRUE)
w8a

t8a <- t.test(data_option_TypeCast8$Undeveloped, data_option_TypeCast8$Brownfield, paired = TRUE)
t8a


# then plot little vs rich wildlife ranking

p8b <- ggplot(data_option8, 
              aes(x = WildlifeValue, y = Ranking))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  geom_signif(comparisons = list(c("Little","Rich")),
              annotations = "***", tip_length = 0, y_position = 8.5)+
  theme_classic()+
  labs(x = "Site wildlife value", y = "Ranking")+
  scale_y_continuous(limits = c(0,9), breaks = c(1:8))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5),
        text = element_text(size = 15))

p8b

m8 <- grid.arrange(p8a,
                   p8b+theme(axis.title.y = element_blank()), 
                   ncol = 2, top = option8)

# run a Wilcoxon test

data_option_ValueCast8 <- dcast(data_option8, RecordNo * Type ~ WildlifeValue, value.var = "Ranking")

w8b <- wilcox.test(data_option_ValueCast8$Rich, data_option_ValueCast8$Little, paired = TRUE)
w8b

t8b <- t.test(data_option_ValueCast8$Rich, data_option_ValueCast8$Little, paired = TRUE)
t8b


# multiplot the whole darn thing

plot_q8c <- grid.arrange(m1,m2,m3,m4,m5,m6,m7,m8, ncol = 2)

plot_q8c

ggsave("Plots/Q8c.svg", plot = plot_q8c, device = svg, width = 250, height = 300, units = "mm", limitsize = F)





### Q9 - are opinions of brownfield sites shaped by demographics at all?

# for this we return to the basics of q1

summary(data_q1)

# pick out the demographics

demographics <- data[,c(1, 101:192)]

data_dem <- merge(data_q1, demographics)

## we want to use the answers to Q2 (how positive/negative do they feel?) as a numeric,
# from 1 (very negative) to 5 (very positive) and excluding 6 (don't know) 
# to carry out this section of the analysis

data_dem <- data_dem[which(data_dem$HBU_Q2 != 6), ]

summary(data_dem)


## first, keep it simple - gender!

summary(data_dem$profile_gender)

data_dem$Gender <- factor(ifelse(data_dem$profile_gender == 1, "Male","Female"))

summary(data_dem$Gender)

# test it - generalised linear model is sufficient
# (a t-test/Wilcoxon test might be appropriate in this case but not throughout this section, so for consistency)

mod9a <- lm(HBU_Q2 ~ Gender,
            data = data_dem)

summary(mod9a)
drop1(mod9a, test = "F")

chkres(mod9a)


# build a plot

p9a <- ggplot(data_dem, 
              aes(x = Gender, y = HBU_Q2))+
  geom_point(colour = "grey60", alpha = 0.2,
             position = position_jitter(width = 0.25, height = 0.25))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  geom_signif(comparisons = list(c("Female","Male")),
              annotations = "N.S.", tip_length = 0, y_position = 5.5)+
  theme_classic()+
  labs(x = "Gender", y = "Opinion")+
  scale_y_continuous(limits = c(0,6), breaks = c(1:5))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

p9a


## second, age

summary(data_dem$profile_julesage)

q9bmatch <- data.frame(
  profile_julesage = c(1:5),
  Age = c("18-24","25-34",
          "35-44","45-54",
          "55+"))

q9bmatch

data_dem <- merge(data_dem, q9bmatch)

data_dem$Age <- factor(data_dem$Age)

summary(data_dem$Age)


# test it - generalised linear model is sufficient
# (a t-test/Wilcoxon test might be appropriate in this case but not throughout this section, so for consistency)

mod9b <- lm(HBU_Q2 ~ Age,
            data = data_dem)

summary(mod9b)
drop1(mod9b, test = "F")

chkres(mod9b)

summary(glht(mod9b, mcp(Age="Tukey")))

labels_q9b <- data.frame(
  Age = c("18-24","25-34",
          "35-44","45-54",
          "55+"),
  Label = c("ab","ab","a","a","b")
)



# build a plot

p9b <- ggplot(data_dem, 
              aes(x = Age, y = HBU_Q2))+
  geom_point(colour = "grey60", alpha = 0.2,
             position = position_jitter(width = 0.25, height = 0.25))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  geom_text(data = labels_q9b, aes(x = Age, y = 5.5, label = Label))+
  theme_classic()+
  labs(x = "Age", y = "Opinion")+
  scale_y_continuous(limits = c(0,6), breaks = c(1:5))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

p9b


## third, social grade

summary(data_dem$profile_socialgrade_cie_rc)

data_dem$SocialGrade <- factor(ifelse(data_dem$profile_socialgrade_cie_rc == 1, "ABC1","C2DE"))

summary(data_dem$SocialGrade)

# test it - generalised linear model is sufficient
# (a t-test/Wilcoxon test might be appropriate in this case but not throughout this section, so for consistency)

mod9c <- lm(HBU_Q2 ~ SocialGrade,
            data = data_dem)

summary(mod9c)
drop1(mod9c, test = "F")

chkres(mod9c)

summary(glht(mod9c, mcp(SocialGrade="Tukey")))


# build a plot

p9c <- ggplot(data_dem, 
              aes(x = SocialGrade, y = HBU_Q2))+
  geom_point(colour = "grey60", alpha = 0.2,
             position = position_jitter(width = 0.25, height = 0.25))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  geom_signif(comparisons = list(c("ABC1","C2DE")),
              annotations = "*", tip_length = 0, y_position = 5.5)+
  theme_classic()+
  labs(x = "Social grade", y = "Opinion")+
  scale_y_continuous(limits = c(0,6), breaks = c(1:5))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

p9c


## next, region
# slightly more complicated to wrangle since it's currently held in multiple columns

# maybe not the most efficient, but could do it as a massive ifelse loop...

data_dem$Region <- factor(
                   ifelse(data_dem$profile_GOR_r2_1 == 1, "North East",
                   ifelse(data_dem$profile_GOR_r2_2 == 1, "North West",
                   ifelse(data_dem$profile_GOR_r2_3 == 1, "Yorkshire and the Humber",
                   ifelse(data_dem$profile_GOR_r2_4 == 1, "East Midlands",
                   ifelse(data_dem$profile_GOR_r2_5 == 1, "West Midlands",
                   ifelse(data_dem$profile_GOR_r2_6 == 1, "East of England",
                   ifelse(data_dem$profile_GOR_r2_7 == 1, "London",
                   ifelse(data_dem$profile_GOR_r2_8 == 1, "South East",
                   ifelse(data_dem$profile_GOR_r2_9 == 1, "South West",
                   ifelse(data_dem$profile_GOR_r2_11 == 1, "Wales",
                   ifelse(data_dem$profile_GOR_r2_12 == 1, "Scotland","Fail"))))))))))))

summary(data_dem$Region)


# test it - generalised linear model is sufficient
# (a t-test/Wilcoxon test might be appropriate in this case but not throughout this section, so for consistency)

mod9d <- lm(HBU_Q2 ~ Region,
            data = data_dem)

summary(mod9d)
drop1(mod9d, test = "F")

chkres(mod9d)

summary(glht(mod9d, mcp(Region="Tukey")))


# build a plot

p9d <- ggplot(data_dem, 
              aes(x = Region, y = HBU_Q2))+
  geom_point(colour = "grey60", alpha = 0.2,
             position = position_jitter(width = 0.25, height = 0.25))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  theme_classic()+
  labs(x = "Region", y = "Opinion")+
  scale_y_continuous(limits = c(0,6), breaks = c(1:5))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

p9d



## next, employment status
# slightly more complicated to wrangle since it's currently held in multiple columns

# maybe not the most efficient, but could do it as a massive ifelse loop...

data_dem$Employment <- factor(
  ifelse(data_dem$profile_work_stat_r_1 == 1, "Working full time",
         ifelse(data_dem$profile_work_stat_r_2 == 1, "Working part time",
                ifelse(data_dem$profile_work_stat_r_3 == 1, "Working (all)",
                       ifelse(data_dem$profile_work_stat_r_4 == 1, "Full time student",
                              ifelse(data_dem$profile_work_stat_r_5 == 1, "Retired",
                                     ifelse(data_dem$profile_work_stat_r_6 == 1, "Unemployed",
                                            ifelse(data_dem$profile_work_stat_r_7 == 1, "Other","Fail"))))))))

summary(data_dem$Employment)


# test it - generalised linear model is sufficient
# (a t-test/Wilcoxon test might be appropriate in this case but not throughout this section, so for consistency)

mod9e <- lm(HBU_Q2 ~ Employment,
            data = data_dem)

summary(mod9e)
drop1(mod9e, test = "F")

chkres(mod9e)

summary(glht(mod9e, mcp(Employment="Tukey")))


labels_q9e <- data.frame(
  Employment = c("Working full time","Working part time",
          "Full time student","Retired",
          "Unemployed","Other"),
  Label = c("a","a","ab","b","ab","ab")
)




# build a plot

p9e <- ggplot(data_dem, 
              aes(x = Employment, y = HBU_Q2))+
  geom_point(colour = "grey60", alpha = 0.2,
             position = position_jitter(width = 0.25, height = 0.25))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  geom_text(data = labels_q9e, aes(x = Employment, y = 5.5, label = Label))+
  theme_classic()+
  labs(x = "Employment status", y = "Opinion")+
  scale_y_continuous(limits = c(0,6), breaks = c(1:5))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

p9e


## next, children in household
# slightly more complicated to wrangle since it's currently held in multiple columns
# but we're only really interested in it as a yes/no, so:


data_dem$Children <- factor(
  ifelse(data_dem$profile_household_children_r_1 == 1, "No children at home",
         ifelse(data_dem$profile_household_children_r_5 == 1, "Children at home","Refused")))

summary(data_dem$Children)


# test it - generalised linear model is sufficient
# (a t-test/Wilcoxon test might be appropriate in this case but not throughout this section, so for consistency)

mod9f <- lm(HBU_Q2 ~ Children,
            data = data_dem)

summary(mod9f)
drop1(mod9f, test = "F")

chkres(mod9f)

summary(glht(mod9f, mcp(Children="Tukey")))



# build a plot

p9f <- ggplot(data_dem, 
              aes(x = Children, y = HBU_Q2))+
  geom_point(colour = "grey60", alpha = 0.2,
             position = position_jitter(width = 0.25, height = 0.25))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  theme_classic()+
  labs(x = "Children at home", y = "Opinion")+
  scale_y_continuous(limits = c(0,6), breaks = c(1:5))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

p9f




## similar but different is parent/guardian (since it allows children who've left home)
# slightly more complicated to wrangle since it's currently held in multiple columns
# but we're only really interested in it as a yes/no, so:


data_dem$Parent <- factor(
  ifelse(data_dem$omnibus_parents_rc3_1 == 1, "Parent/guardian",
         ifelse(data_dem$omnibus_parents_rc3_2, "Not parent/guardian","Fail")))

summary(data_dem$Parent)


# test it - generalised linear model is sufficient
# (a t-test/Wilcoxon test might be appropriate in this case but not throughout this section, so for consistency)

mod9g <- lm(HBU_Q2 ~ Parent,
            data = data_dem)

summary(mod9g)
drop1(mod9g, test = "F")

chkres(mod9g)

summary(glht(mod9g, mcp(Parent="Tukey")))



# build a plot

p9g <- ggplot(data_dem, 
              aes(x = Parent, y = HBU_Q2))+
  geom_point(colour = "grey60", alpha = 0.2,
             position = position_jitter(width = 0.25, height = 0.25))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  theme_classic()+
  labs(x = "Parent/guardianship", y = "Opinion")+
  scale_y_continuous(limits = c(0,6), breaks = c(1:5))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

p9g



## next, pet ownership
# (really we're only interested in dog vs no dog)

data_dem$Dog <- factor(
  ifelse(data_dem$pets_1 == 1, "Dog",
         ifelse(data_dem$pets_99 == 1, "No pets","Other pet")))

summary(data_dem$Dog)


# test it - generalised linear model is sufficient
# (a t-test/Wilcoxon test might be appropriate in this case but not throughout this section, so for consistency)

mod9h <- lm(HBU_Q2 ~ Dog,
            data = data_dem)

summary(mod9h)
drop1(mod9h, test = "F")

chkres(mod9h)

summary(glht(mod9h, mcp(Dog="Tukey")))



# build a plot

p9h <- ggplot(data_dem, 
              aes(x = Dog, y = HBU_Q2))+
  geom_point(colour = "grey60", alpha = 0.2,
             position = position_jitter(width = 0.25, height = 0.25))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  theme_classic()+
  labs(x = "Pet ownership", y = "Opinion")+
  scale_y_continuous(limits = c(0,6), breaks = c(1:5))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

p9h



## settlement size

summary(data_dem$HBUDEMOG_Q18)

q9imatch <- data.frame(
  HBUDEMOG_Q18 = c(1:7),
  Settlement = c("City (centre)","City (suburbs)",
          "Town","Village",
          "Hamlet","Countryside","Prefer not to say"))

q9imatch

data_dem <- merge(data_dem, q9imatch)

data_dem$Settlement <- factor(data_dem$Settlement)

summary(data_dem$Settlement)


# test it - generalised linear model is sufficient
# (a t-test/Wilcoxon test might be appropriate in this case but not throughout this section, so for consistency)

mod9i <- lm(HBU_Q2 ~ Settlement,
            data = data_dem)

summary(mod9i)
drop1(mod9i, test = "F")

chkres(mod9i)

summary(glht(mod9i, mcp(Settlement="Tukey")))


# build a plot


data_dem$Settlement_ord <- ordered(data_dem$Settlement, levels = c("City (centre)","City (suburbs)",
                                                                   "Town","Village",
                                                                   "Hamlet","Countryside","Prefer not to say"))


p9i <- ggplot(data_dem, 
              aes(x = Settlement_ord, y = HBU_Q2))+
  geom_point(colour = "grey60", alpha = 0.2,
             position = position_jitter(width = 0.25, height = 0.25))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  theme_classic()+
  labs(x = "Settlement size", y = "Opinion")+
  scale_y_continuous(limits = c(0,6), breaks = c(1:5))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

p9i


## settlement tenure

summary(data_dem$HBUDEMOG_Q18b)

q9jmatch <- data.frame(
  HBUDEMOG_Q18b = c(1:5,99),
  STenure = c("Less than a year","Between 1 to 2 years",
                 "Between 3 to 5 years","Between 6 to 10 years",
                 "Over 10 years","Prefer not to say"))

q9jmatch

data_dem <- merge(data_dem, q9jmatch)

data_dem$STenure <- factor(data_dem$STenure)

summary(data_dem$STenure)


# test it - generalised linear model is sufficient
# (a t-test/Wilcoxon test might be appropriate in this case but not throughout this section, so for consistency)

mod9j <- lm(HBU_Q2 ~ STenure,
            data = data_dem)

summary(mod9j)
drop1(mod9j, test = "F")

chkres(mod9j)

summary(glht(mod9j, mcp(STenure="Tukey")))


# build a plot


data_dem$STenure_ord <- ordered(data_dem$STenure, levels = c("Less than a year","Between 1 to 2 years",
                                                             "Between 3 to 5 years","Between 6 to 10 years",
                                                             "Over 10 years","Prefer not to say"))


p9j <- ggplot(data_dem, 
              aes(x = STenure_ord, y = HBU_Q2))+
  geom_point(colour = "grey60", alpha = 0.2,
             position = position_jitter(width = 0.25, height = 0.25))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  theme_classic()+
  labs(x = "Settlement tenure", y = "Opinion")+
  scale_y_continuous(limits = c(0,6), breaks = c(1:5))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

p9j




## home ownership

summary(data_dem$profile_house_tenure)

q9kmatch <- data.frame(
  profile_house_tenure = c(1:9),
  Home = c("Own – outright","Own – with a mortgage",
           "Own - shared ownership","Rent - private",
           "Rent - council","Rent - housing association",
           "Family - rent","Family - free", "Other"))

q9kmatch

data_dem <- merge(data_dem, q9kmatch)

data_dem$Home <- factor(data_dem$Home)

summary(data_dem$Home)


# test it - generalised linear model is sufficient
# (a t-test/Wilcoxon test might be appropriate in this case but not throughout this section, so for consistency)

mod9k <- lm(HBU_Q2 ~ Home,
            data = data_dem)

summary(mod9k)
drop1(mod9k, test = "F")

chkres(mod9k)

summary(glht(mod9k, mcp(Home="Tukey")))


# build a plot


data_dem$Home_ord <- ordered(data_dem$Home, levels = c("Own – outright","Own – with a mortgage",
                                                       "Own - shared ownership","Rent - private",
                                                       "Rent - council","Rent - housing association",
                                                       "Family - rent","Family - free", "Other"))


p9k <- ggplot(data_dem, 
              aes(x = Home_ord, y = HBU_Q2))+
  geom_point(colour = "grey60", alpha = 0.2,
             position = position_jitter(width = 0.25, height = 0.25))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  theme_classic()+
  labs(x = "Home ownership", y = "Opinion")+
  scale_y_continuous(limits = c(0,6), breaks = c(1:5))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

p9k


## home tenure

summary(data_dem$HBUDEMOG_Q19)

q9lmatch <- data.frame(
  HBUDEMOG_Q19 = c(1:5,99),
  HTenure = c("Less than a year","Between 1 to 2 years",
              "Between 3 to 5 years","Between 6 to 10 years",
              "Over 10 years","Prefer not to say"))

q9lmatch

data_dem <- merge(data_dem, q9lmatch)

data_dem$HTenure <- factor(data_dem$HTenure)

summary(data_dem$HTenure)


# test it - generalised linear model is sufficient
# (a t-test/Wilcoxon test might be appropriate in this case but not throughout this section, so for consistency)

mod9l <- lm(HBU_Q2 ~ HTenure,
            data = data_dem)

summary(mod9l)
drop1(mod9l, test = "F")

chkres(mod9l)

summary(glht(mod9l, mcp(HTenure="Tukey")))


# build a plot


data_dem$HTenure_ord <- ordered(data_dem$HTenure, levels = c("Less than a year","Between 1 to 2 years",
                                                             "Between 3 to 5 years","Between 6 to 10 years",
                                                             "Over 10 years","Prefer not to say"))


p9l <- ggplot(data_dem, 
              aes(x = HTenure_ord, y = HBU_Q2))+
  geom_point(colour = "grey60", alpha = 0.2,
             position = position_jitter(width = 0.25, height = 0.25))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  theme_classic()+
  labs(x = "Home tenure", y = "Opinion")+
  scale_y_continuous(limits = c(0,6), breaks = c(1:5))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

p9l


### Q10 - stakeholder affiliations

## first, conservation org staff

summary(data_dem$HBUDEMOG_Q20_1)

data_dem$ConsStaff <- factor(ifelse(data_dem$HBUDEMOG_Q20_1 == 1, "Yes","No"))

summary(data_dem$ConsStaff)

# test it - generalised linear model is sufficient
# (a t-test/Wilcoxon test might be appropriate in this case but not throughout this section, so for consistency)

mod10a <- lm(HBU_Q2 ~ ConsStaff,
            data = data_dem)

summary(mod10a)
drop1(mod10a, test = "F")

chkres(mod10a)


# build a plot

p10a <- ggplot(data_dem, 
              aes(x = ConsStaff, y = HBU_Q2))+
  geom_point(colour = "grey60", alpha = 0.2,
             position = position_jitter(width = 0.25, height = 0.25))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  geom_signif(comparisons = list(c("Yes","No")),
              annotations = "N.S.", tip_length = 0, y_position = 5.5)+
  theme_classic()+
  labs(x = "Conservation organisation (member of staff)", y = "Opinion")+
  scale_y_continuous(limits = c(0,6), breaks = c(1:5))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

p10a



## conservation org volunteer

summary(data_dem$HBUDEMOG_Q20_2)

data_dem$ConsVol <- factor(ifelse(data_dem$HBUDEMOG_Q20_2 == 1, "Yes","No"))

summary(data_dem$ConsVol)

# test it - generalised linear model is sufficient
# (a t-test/Wilcoxon test might be appropriate in this case but not throughout this section, so for consistency)

mod10b <- lm(HBU_Q2 ~ ConsVol,
             data = data_dem)

summary(mod10b)
drop1(mod10b, test = "F")

chkres(mod10b)


# build a plot

p10b <- ggplot(data_dem, 
               aes(x = ConsVol, y = HBU_Q2))+
  geom_point(colour = "grey60", alpha = 0.2,
             position = position_jitter(width = 0.25, height = 0.25))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  geom_signif(comparisons = list(c("Yes","No")),
              annotations = "N.S.", tip_length = 0, y_position = 5.5)+
  theme_classic()+
  labs(x = "Conservation organisation (volunteer)", y = "Opinion")+
  scale_y_continuous(limits = c(0,6), breaks = c(1:5))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

p10b



## conservation org member or supporter

summary(data_dem$HBUDEMOG_Q20_3)

data_dem$ConsMem <- factor(ifelse(data_dem$HBUDEMOG_Q20_3 == 1, "Yes","No"))

summary(data_dem$ConsMem)

# test it - generalised linear model is sufficient
# (a t-test/Wilcoxon test might be appropriate in this case but not throughout this section, so for consistency)

mod10c <- lm(HBU_Q2 ~ ConsMem,
             data = data_dem)

summary(mod10c)
drop1(mod10c, test = "F")

chkres(mod10c)


# build a plot

p10c <- ggplot(data_dem, 
               aes(x = ConsMem, y = HBU_Q2))+
  geom_point(colour = "grey60", alpha = 0.2,
             position = position_jitter(width = 0.25, height = 0.25))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  geom_signif(comparisons = list(c("Yes","No")),
              annotations = "N.S.", tip_length = 0, y_position = 5.5)+
  theme_classic()+
  labs(x = "Conservation organisation (member/supporter)", y = "Opinion")+
  scale_y_continuous(limits = c(0,6), breaks = c(1:5))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

p10c



## local authority (e.g. councillor)

summary(data_dem$HBUDEMOG_Q20_4)

data_dem$LA <- factor(ifelse(data_dem$HBUDEMOG_Q20_4 == 1, "Yes","No"))

summary(data_dem$LA)

# test it - generalised linear model is sufficient
# (a t-test/Wilcoxon test might be appropriate in this case but not throughout this section, so for consistency)

mod10d <- lm(HBU_Q2 ~ LA,
             data = data_dem)

summary(mod10d)
drop1(mod10d, test = "F")

chkres(mod10d)


# build a plot

p10d <- ggplot(data_dem, 
               aes(x = LA, y = HBU_Q2))+
  geom_point(colour = "grey60", alpha = 0.2,
             position = position_jitter(width = 0.25, height = 0.25))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  geom_signif(comparisons = list(c("Yes","No")),
              annotations = "N.S.", tip_length = 0, y_position = 5.5)+
  theme_classic()+
  labs(x = "Local authority (e.g. councillor)", y = "Opinion")+
  scale_y_continuous(limits = c(0,6), breaks = c(1:5))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

p10d



## community organisation (e.g. Neighbourhood Watch)

summary(data_dem$HBUDEMOG_Q20_5)

data_dem$NW <- factor(ifelse(data_dem$HBUDEMOG_Q20_5 == 1, "Yes","No"))

summary(data_dem$NW)

# test it - generalised linear model is sufficient
# (a t-test/Wilcoxon test might be appropriate in this case but not throughout this section, so for consistency)

mod10e <- lm(HBU_Q2 ~ NW,
             data = data_dem)

summary(mod10e)
drop1(mod10e, test = "F")

chkres(mod10e)


# build a plot

p10e <- ggplot(data_dem, 
               aes(x = NW, y = HBU_Q2))+
  geom_point(colour = "grey60", alpha = 0.2,
             position = position_jitter(width = 0.25, height = 0.25))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  geom_signif(comparisons = list(c("Yes","No")),
              annotations = "N.S.", tip_length = 0, y_position = 5.5)+
  theme_classic()+
  labs(x = "Community organisation (e.g. Neighbourhood Watch)", y = "Opinion")+
  scale_y_continuous(limits = c(0,6), breaks = c(1:5))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

p10e



## farmer/agriculture

summary(data_dem$HBUDEMOG_Q20_6)

data_dem$Farmer <- factor(ifelse(data_dem$HBUDEMOG_Q20_6 == 1, "Yes","No"))

summary(data_dem$Farmer)

# test it - generalised linear model is sufficient
# (a t-test/Wilcoxon test might be appropriate in this case but not throughout this section, so for consistency)

mod10f <- lm(HBU_Q2 ~ Farmer,
             data = data_dem)

summary(mod10f)
drop1(mod10f, test = "F")

chkres(mod10f)


# build a plot

p10f <- ggplot(data_dem, 
               aes(x = Farmer, y = HBU_Q2))+
  geom_point(colour = "grey60", alpha = 0.2,
             position = position_jitter(width = 0.25, height = 0.25))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  geom_signif(comparisons = list(c("Yes","No")),
              annotations = "N.S.", tip_length = 0, y_position = 5.5)+
  theme_classic()+
  labs(x = "Farmer (or agriculture-related business)", y = "Opinion")+
  scale_y_continuous(limits = c(0,6), breaks = c(1:5))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

p10f



## renewables

summary(data_dem$HBUDEMOG_Q20_7)

data_dem$Ren <- factor(ifelse(data_dem$HBUDEMOG_Q20_7 == 1, "Yes","No"))

summary(data_dem$Ren)

# test it - generalised linear model is sufficient
# (a t-test/Wilcoxon test might be appropriate in this case but not throughout this section, so for consistency)

mod10g <- lm(HBU_Q2 ~ Ren,
             data = data_dem)

summary(mod10g)
drop1(mod10g, test = "F")

chkres(mod10g)


# build a plot

p10g <- ggplot(data_dem, 
               aes(x = Ren, y = HBU_Q2))+
  geom_point(colour = "grey60", alpha = 0.2,
             position = position_jitter(width = 0.25, height = 0.25))+
  geom_violin(trim = T,
              adjust = 4,
              alpha = 0.2)+
  geom_boxplot(alpha = 0.8, width = 0.1, outlier.shape = NA)+
  geom_signif(comparisons = list(c("Yes","No")),
              annotations = "N.S.", tip_length = 0, y_position = 5.5)+
  theme_classic()+
  labs(x = "Renewable energy industry", y = "Opinion")+
  scale_y_continuous(limits = c(0,6), breaks = c(1:5))+
  theme(panel.grid.major.y = element_line(colour = "grey90", size = 0.5))

p10g



### did social grade correlate with urban/rural?

plot(data_dem$SocialGrade ~ data_dem$Settlement)



