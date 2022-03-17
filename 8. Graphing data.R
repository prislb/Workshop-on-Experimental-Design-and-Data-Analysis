setwd("~/Documents/Statistics workshop"))
options(scipen = 999) #scientific notation off; options(scipen = 0)
install.packages("tidyverse")
library(tidyverse)

### TUTORIAL
data <- read_csv("Data_Tutorials.csv")

# First of all we will factorize the continuous variable Frequency in order to split it into three groups. This will be assigned to a new column in the data frame.
data$Log_Freq_HAL_C <- scale(data$Log_Freq_HAL)

data$Freq_Fac <- ifelse(data$Log_Freq_HAL_C <= -1, "Low_Freq",
                # if frequency is smaller or equal to 1
                 ifelse(data$Log_Freq_HAL_C > -1 & 
                # if frequency is bigger than -1 AND
                 data$Log_Freq_HAL_C <= 0,"Medium_Freq",
                #smaller or equal to 0
                 ifelse(data$Log_Freq_HAL_C > 0, "High_Freq",NA)))
                # if frequency is smaller than zero, otherwise NA

# Transform into factor (categorical)
data$Freq_Fac <- as.factor(data$Freq_Fac)

# Reorganize levels of the variable
data$Freq_Fac <- factor(data$Freq_Fac, levels = c("Low_Freq","Medium_Freq", "High_Freq"))

class(data$Freq_Fac)
summary(data$Freq_Fac)
levels(data$Freq_Fac)

## Also, just in case, add a log RT column using mutate
data <- data %>%
        mutate(logRT = log(RT))

### 1. HISTOGRAM + DENSITY PLOT (Mapping vs. Setting aesthetics)
### We will visualize the distribution of  our data with a histogram combined with a density plot
### First of all, let's just visualize our dependent variable RT
ggplot(data, aes(x = RT)) + 
  geom_histogram(aes(y=..density..),
                                     # aes() takes variables as mappings INSIDE the function
  fill = "white", color = "black", binwidth = 50) + 
                                    # aesthetics that are set MANUALLY (do not refer to variables)                                                go they go OUTSIDE aes()
  geom_density(alpha=.2, fill="blue") +
                                    # overlay with transparent density plot, again we don't use                                                         aes() because we're manually setting parameters
 scale_x_continuous(breaks=seq(500,3000,by=500)) +
                                    # choose min and max for X axis as well as breaks
 labs(x="Reaction Times (ms)", y="Density") +
                                    # titles for X and Y axes
 ggtitle ("Histogram & Density Curve of Mean Reaction Times")
                                    # main title for plot 

# INSIDE aes() a mapping conveys information about our data, outside aes() we just "make it pretty". 

ggplot(data, aes(y=RT,x=Freq_Fac, fill = Freq_Fac))+          
  stat_summary (fun.y = mean,geom="bar",position = "dodge", width = .5 )+
  stat_summary(fun.data = mean_se, color="black", geom = "errorbar", position = position_dodge(width = .2)) +  scale_y_continuous(breaks=seq(100,1400,by=100))

ggplot(data, aes(y=RT,x=Freq_Fac))+          
  stat_summary (fun.y = mean,geom="bar",position = "dodge", width = .5, fill = "blue")+
  stat_summary(fun.data = mean_se, color="black", geom = "errorbar", position = position_dodge(width = .2))


### Second, let's visualize the distribution of our dependent variable as a function of frequency
ggplot(data, aes(x = RT, color= Freq_Fac, fill = Freq_Fac)) +
                                 # notice how color is INSIDE aes() since we're                                                       referencing a variable rather than setting it manually
  geom_histogram(binwidth = 50) +
  scale_x_continuous(breaks=seq(500,3000,by=1000)) +
  #facet_wrap( ~ Freq_Fac, ncol = 3)
  facet_grid(Freq_Fac ~.) 
  #facet_grid( . ~ Freq_Fac, labeller=labeller(Freq_Fac = labels)) +
                              # variables used with facet wrap (and grid) must be categorical that's                                      why we factorized frequency. It must also be a formula, hence ~
  labs(x="Reaction Times (ms)") +
  ggtitle ("Histogram of Mean Reading Times by Frequency")
  
# Extra code to modify facets
# Set labels
labels <- c(Low_Freq = "Low frequency", Medium_Freq =" Medium frequency", High_Freq = "High frequency")

# facet wrap only splits the data by ONE variable vertically (e.g.: facet-wrap (~ Freq_Fac)) but you can specify the appearance of the split

# facet grid will can split the data by one or TWO variables. Also, if choosing only one variable, we can specify if we want the split to be vertical (.~ Freq_Faq) or horizontal (Freq_Fac ~ .)



### 2. SCATTERPLOTS (Global vs. Local mappings)
### Scatterplots of the dependent variable RT in relation to predictors PND and SND

# Phonological neighborhood density: Words with higher PND are named faster
# Only global mappings
ggplot(data, aes(x=PND,y=RT)) +
                       # global mappings select which main variables we'll be plotting; goes                            INSIDE aes()
  geom_smooth(method='lm', se= TRUE, level = 0.95, color="black", fill="purple") +
  labs(x="Phonological neighborhood density",y="Reaction Time") +
  scale_y_continuous(breaks=seq(500,3000,by=25)) 

# Global AND local mappings
ggplot(data, aes(x=PND,y=RT)) +
                            # global mappings
  geom_point(aes(color = Freq_Fac, shape = Freq_Fac))+
                           # local mappings for geom_point
  geom_smooth( method = "lm",  color="black", fill="purple") +
  labs(x='Phonological neighborhood density',y='Reaction Time') +
  scale_y_continuous(breaks=seq(500,3000,by=500))
 
 
ggplot(data, aes(x=PND,y=RT)) +
                            # global mappings
  geom_point(shape=18, color = "purple", alpha = .2) +
  geom_smooth( aes(fill = Freq_Fac, linetype = Freq_Fac), method='lm', color = "black") +
                          # local mappings for geom_smooth  
  labs(x='Phonological neighborhood density',y='Reaction Time') +
  scale_y_continuous(breaks=seq(500,3000,by=500)) 


### Explore wrap and grid using a variable with more levels and a scatterplot
ggplot(data, aes(Log_Freq_HAL_C, RT)) +
  geom_point(alpha=.05, color = "blue", fill="red") +
  geom_smooth(method= "lm", se=T, color= "black", fill = "red") +
  scale_y_continuous(breaks=seq(1000,3000,by=500)) +
  xlab("Frequency") +
  facet_wrap(~ Length, ncol = 4) 
 
ggplot(data, aes(Log_Freq_HAL_C, RT)) +
  geom_point(alpha=.05, color = "blue", fill="red") +
  geom_smooth(method= "lm", se=T, color= "black", fill = "red") +
  scale_y_continuous(breaks=seq(1000,3000,by=500)) +
  xlab("Frequency") +
  facet_grid(PND_Fac ~ Length) 


### BAR PLOTS
### First, let's also create a couple categorical variables.
# Subjects groups: Slow & Fast
data <- data %>% 
        group_by(sub) %>%
        mutate(sub_meanRT= mean(RT))

data$Sub_Fac <-ifelse(data$sub_meanRT <= 1159.6  , "Fast",
                      ifelse(data$sub_meanRT > 1159.6  , "Slow", NA)) 

data$Sub_Fac <- as.factor(data$Sub_Fac)

# PND groups: High & Low
data$PND_Fac <-ifelse(data$PND <= 8.663  , "Low PND",
                      ifelse(data$PND > 8.663  , "High PND", NA)) 

data$PND_Fac <- as.factor(data$PND_Fac)


### Let's plot it
# Always check the order of the levels of a factor before labeling in a graph
levels(data$PND_Fac)

ggplot(data, aes(Freq_Fac)) +
  geom_bar(aes(fill = PND_Fac), width = .5, position = "dodge") +
   scale_x_discrete(name = "Frequency",
                  breaks=c("Low_Freq", "Medium_Freq", "High_Freq"),
                  labels=c("Low frequency", "Medium frequency", "High frequency")) +
                # modify our discrete x axis
      scale_fill_manual(name = "PND",
                      values=c("pink", "lightblue"), 
                      breaks=c("High PND", "Low PND")) +
                # modify our legend based on the variable we used for fill in aes()
          theme(axis.text.x = element_text(angle=15)) 


# USING STAT INSTEAD OF COUNT
ggplot(data, aes(y=RT,x=Freq_Fac, fill = Freq_Fac))+          
  stat_summary(fun.y = mean,geom="bar",position = "dodge", width = .5 )+
  stat_summary(fun.data = mean_se, color="black", geom = "errorbar", position = position_dodge(width = .2))+
  #scale_y_continuous(breaks=seq(100,1500,by=100)) +
  scale_x_discrete(name = "Frequency",
                   breaks=c("Low_Freq", "Medium_Freq", "High_Freq"),
                   labels=c("Low frequency", "Medium frequency", "High frequency")) +
  theme(axis.text.x = element_text(angle=15,face = "bold",size = 10)) +
  coord_cartesian(ylim = c(1000,1500)) #zoom in into the desired values (in other words it allows a big value                                             such as 1000 to be at the beginning of the axis without skewing the graph) 

ggplot(data, aes(y=RT,x=Freq_Fac, fill = Freq_Fac))+
  facet_grid(~Sub_Fac)+
  stat_summary (fun.y = mean,geom="bar",position = "dodge")+
  stat_summary(fun.data = mean_se, color="black", geom = "errorbar", position = position_dodge(width = .9)) +
  scale_y_continuous(breaks=seq(0,2000,by=100)) +
  scale_x_discrete(name = "Frequency",
                   breaks=c("Low_Freq", "Medium_Freq", "High_Freq"),
                   labels=c("Low frequency", "Medium frequency", "High frequency")) +
  theme(axis.text.x = element_text(angle=15))+ # angles text 
  theme(text = element_text(size = 15)) #changes size of all text in graph
  
                
### LINE PLOT
  ggplot(data, aes(x= PND_Fac, y=RT, group=Freq_Fac, shape=Freq_Fac, color = Freq_Fac)) +
    stat_summary (fun.y = mean, geom = "line", aes(linetype=Freq_Fac), size=1.3)+
    stat_summary (fun.y = mean, geom = "point", aes(shape=Freq_Fac),size=4) +
    theme_minimal()
    


    





