# MOD102: Intro to functions

## Learning goals

#1.  _Encapsulate_ logic in a function using _parameters_
#2.  Improve clarity by limiting your functions' _scope_
#3.  Use _immutable_ variables to reduce bugs


### Counting nucleotides

#'The frequency of each nucleotide in a gene sequence

#What is the frequency of each nucleotide
koala_gene <- "AGCCTTAAATAACGACCTTC"

#Function to get the sequence of each nucleotide
n_nucleo <- function(nucleo, sequence) {
  result <- gregexpr(nucleo, sequence)
  if (length(result[[1]]) == 1 && result[[1]] == -1) {
    result <- 0
  } else {
    result <- length(result[[1]])
  }
  result
}

**Q1**
#Make a vector with four elements corresponding to A, C, G, and T frequencies, respectively.
nA <- n_nucleo("A", koala_gene)
nC <- n_nucleo("C", koala_gene)
nG <- n_nucleo("G", koala_gene)
nT <- n_nucleo("T", koala_gene)

frequencies <- c(nA, nG, nC, nT) 
frequencies


**Q2**
#Write a function that calculates the GC content for a sequence.
gc_content <- function(sequence) {
  result <- gregexpr("G|C", sequence)
  if (length(result[[1]]) == 1 && result[[1]] == -1) {
    result <- 0
  } else {
    result <- length(result[[1]])/nchar(sequence)
  }
  result
}

#Calculate the GC content for:
gc_content("AAG")
gc_content("TGCCCATGGG")


**Q3**
#paper that found the ancestral marsupial cuteness gene
#Count up the number of point mutations:
marsupial_gene <- "AGCCTTTCAACACGACCTTC"
koala_nucleos <- strsplit(koala_gene, "")[[1]]
marsupial_nucleos <- strsplit(marsupial_gene, "")[[1]]
n_mutations <- sum(koala_nucleos != marsupial_nucleos)
n_mutations

#write a function to encapsulate this logic:
n_mutations <- function(group_gene, species_gene){
  species_nucleos <- strsplit(species_gene, "")[[1]]
  group_nucleos <- strsplit(group_gene, "")[[1]]
  sum(species_nucleos != group_nucleos)
}

n_mutations(marsupial_gene, koala_gene)#count the number of mutations


###Recap
**Q4**
#Choose a piece of code (~15-30 lines) and describe how you would parameterize it. 
#This can be code you wrote, or you can search GitHub, StackOverflow, etc. In your description, specify:

#another example: convert the following line into a function
fp_presence_abs <-  t(fp_presence_abs) #re-transpose to have right format
library(pvclust)
res.pv <- pvclust(fp_presence_abs, method.dist="binary", #here binary uses Jaccard
                  method.hclust="average")
plot(res.pv, hang = -1, cex = 0.5)# Default plot


#a)What task is the code snippet accomplishing?
#This code is transposing the data, then using the function pvclust() from pvclust package
#and using the clustering matrix output to plot a dendrogram and highlight the strongest relationship


#b)What specific variables would you turn into general parameters?

#general parameters: the dataframe

#c)What would you name this function?
#plot_dendrogram()


**Q5** 
#Encapsulation also makes it easy to read and understand your code. Read Functions 
#are for humans and computers from the R For Data Science book and take another 
#look at your function name. Would replacing the code snippet with your function's 
#name make the code more readable? After reading the R4DS section, would you revise the name?

#Yes, using the function will simplify and make the code more clear. After reading the R4DS section, 
#I would change the name from plot_dendrogram() to plot_jaccard_dendrogram() to make it more specific and
#avoid conflicts with other people's functions

#function
plot_jaccard_dendrogram <- function(df){
  t_data <- t(df)#transpose the data to the format we need
  cluster_data <- pvclust(t_data, method.dist="binary",method.hclust="average") #agglomerative clustering using jaccard
  plot(cluster_data, hang = -1, cex = 0.5) #plot the data
  pvrect(res.pv) #highlight the strongest relationship
  }


###Bills and flippers

library(tidyverse)
library(palmerpenguins)
ggplot(drop_na(penguins, flipper_length_mm, bill_length_mm), 
       aes(flipper_length_mm, bill_length_mm, color = species)) +
  geom_point(aes(shape = species), size = 2, alpha = 0.75) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(x = "Flipper length (mm)",
       y = "Bill length (mm)",
       color = "Penguin species",
       shape = "Penguin species") +
  scale_color_manual(values = c(Adelie = "#E4811E",
                                Chinstrap = "#B05CC5",
                                Gentoo = "#417175")) +
  theme_minimal() +
  theme(legend.box.background = element_rect(color = NA, fill = "white"),
        legend.justification = c(1, 0),
        legend.position = c(0.95, 0.05))

####Function to get the ith longest bill in the dataset.
ith_bill <- function(i, bills) {
  sort(bills, decreasing = TRUE)[i]
}

#Find the 3rd longest bill
ith_bill(3, penguins$bill_length_mm)



**Q6**
#How long is the 3rd longest bill for each of the three species?

# for the adelies
#ith_bill(3, penguins[penguins$species == "Adelie"]$bill_length_mm) this didn't work
penguins%>%
  group_by(species)%>%
  summarize(ith_bill(3, bill_length_mm))


#But let's say i was already defined elsewhere. What would the following return, the 2nd or 10th longest?
i <- 2

ith_bill(2, penguins$bill_length_mm)
penguins$bill_length_mm <-5
ith_bill(2, penguins$bill_length_mm)
#the code will return the second and 10th longest bill lenght, no matter how the 
#i was is defined before because, this i is outside the function, so it only gets overwritten/re-defined
#when running the line of code


#Pure functions
######ith_bill() is an example of a pure function

####Let's change ith_bill() to make it impure.

ith_bill_impure <- function(i) {
  bills <- penguins$bill_length_mm
  sort(bills, decreasing = TRUE)[i]
}

#We removed the bills parameter and instead we grab the bill_length_mm column from the penguins data frame directly.

**Q7**

#Why is ith_bill_impure() not a pure function? Give an example where the same input can lead to different return values.

ith_bill_impure(1)
penguins$bill_length_mm <-5
ith_bill_impure(1)


**Q8**
##Which of the following functions are pure? For the impure functions, how do they violate the properties of a pure function?
  
sqrt()#is pure
rnorm() # impure because it gives different outputs when giving the same inputs
lm() #pure, more complex but it will still give the same outputs when using the same inputs
read.csv() #impure? because the same name of file can be used for multiple files?
write.csv() #impure because it changes/re-write modify the files in the computer, it has side effects in the system


**Q9**
#The following code clusters penguins by morphology and assesses what fraction 
#are misclassified by species. Re-write it in three pure functions: prepare_data(), 
#find_clusters(), and assess_clusters(). If you don't understand how the code works, 
#ask another student or TA for assistance.

penguin_morpho <- select(penguins, bill_length_mm, flipper_length_mm, species)
penguin_morpho <- drop_na(penguin_morpho)
penguin_clust <- kmeans(select(penguin_morpho, -species), 
                        centers = n_distinct(penguins$species))
penguin_morpho$cluster <- penguin_clust$cluster
cluster_species <- penguin_morpho %>% 
  group_by(species) %>% 
  summarize(cluster = names(which.max(table(cluster)))) %>% 
  transmute(clustered_species = species, 
            cluster = as.numeric(cluster))
penguin_morpho <- left_join(penguin_morpho, cluster_species, by = "cluster")
n_misclass <- sum(penguin_morpho$species != penguin_morpho$clustered_species)
n_total <- nrow(penguin_morpho)
n_misclass / n_total

#Function prepare_data()
prepare_data_new <- function(penguins){
  penguins%>%select(bill_length_mm, flipper_length_mm, species)%>%
    drop_na()
}


#Function find_clusters()
find_clusters <- function(clean_data) {
  penguin_clust <- kmeans(select(clean_data, -species), 
                          centers = n_distinct(clean_data$species))  
  clean_data$cluster <-penguin_clust$cluster
  clean_data
}


#Function assess_clusters()
assess_clusters <- function(cluster_data){
  cluster_species <- cluster_data %>% 
    group_by(species) %>% 
    summarize(cluster = names(which.max(table(cluster)))) %>% 
    transmute(clustered_species = species, 
              cluster = as.numeric(cluster))
  
  penguin_morpho <- left_join(cluster_data, cluster_species, by = "cluster")
  n_misclass <- sum(penguin_morpho$species != penguin_morpho$clustered_species)
  n_total <- nrow(penguin_morpho)
  n_misclass / n_total
}

clean_data<- prepare_data_new(penguins)
cluster_data<- find_clusters(clean_data)
prop_misclassified <- assess_clusters(cluster_data)

###Leave that variable alone
x <- list(1, 2, 4)
y <- x
x[[2]] <- 8

#What's y[[2]]?
y[[2]]#it is 2, but it would be 8 if y was mutable


#In the following code, filter() returns a subset of the penguins data frame, 
#but the original data frame is left untouched
filter(penguins, species == "Gentoo")
penguins

#I can pipe the operations together without worrying if I'm altering the original data.

# What's the average bill size of penguins with longer-than-average flippers?
# (by species)
penguins %>% 
  group_by(species) %>% 
  filter(flipper_length_mm > mean(flipper_length_mm, na.rm = TRUE)) %>% 
  summarize(mean_bill_length = mean(bill_length_mm, na.rm = TRUE))

**Q10**
#Fill out the parameters and body of body_condition() and condition_trend() in the 
#following code. Make sure your functions are pure, i.e., limited scope and immutable 
#variables.

  # body_condition() quantifies the body condition of a penguin as its residual
  # (normalized) mass relative to its structural size (bill length, bill depth, 
  # and flipper length). Use lm() to fit a regression and resid() to get the 
  # residuals. 
  body_condition <- function(body_mass_g, bill_length_mm, bill_depth_mm, flipper_length_mm) {
    mass_normalized <- (body_mass_g - mean(body_mass_g)) / sd(body_mass_g)
    mod <- lm(mass_normalized ~ bill_length_mm + bill_depth_mm + flipper_length_mm)
    resid(mod)
  }

penguins_condition <- penguins %>% 
  drop_na() %>% 
  group_by(species) %>% 
  mutate(condition = body_condition(body_mass_g, bill_length_mm, bill_depth_mm, flipper_length_mm)) %>% 
  ungroup()

# condition_trend() calculates the slope of body condition over time. Use lm() 
# to fit a regression and coef() to get the slope.
condition_trend <- function(condition, year) {
  mod <- lm(condition ~ year)
  coef(mod)[1]
}

penguins_trend <- penguins_condition %>% 
  group_by(species) %>% 
  summarize(trend = condition_trend(condition, year)) %>% 
  mutate(trend_lbl = sprintf("slope=%0.2f", trend))

ggplot(penguins_condition, aes(factor(year), condition)) + 
  geom_violin() + 
  geom_smooth(aes(group = species), method = "lm", se = FALSE) + 
  geom_text(aes(label = trend_lbl), penguins_trend, x = 2, y = 0.05) +
  facet_grid(rows = vars(species)) +
  theme_minimal()


