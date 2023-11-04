library(tidyverse)
library(rstatix) #https://rpkgs.datanovia.com/rstatix/
library(ggpubr) #https://rpkgs.datanovia.com/ggpubr/

ds <- read_csv("overall.csv") %>% select(-(prone:available), -(ongroundp:bellyprop), -(crawler:sitcomp))

# Quickly see variables
glimpse(ds)

# Age group, walker group, sitter group should be factors
ds$age_group <- factor(ds$age)
ds$walker <- factor(ds$walker, levels = c(0,1), labels = c("Non-Walker","Walker"))
ds$sitter <- factor(ds$sitter, levels = c(0,1), labels = c("Non-Sitter","Sitter"))
glimpse(ds)

# Count participants by group
table(ds$age_group)
table(ds$sitter)
fct_count(ds$sitter, prop = T)
table(ds$age_group, ds$sitter)

# Summary statistics
ds %>% get_summary_stats(pronep)
ds %>% get_summary_stats(pronep:heldp)
ds %>% group_by(age_group) %>%  get_summary_stats(sittingp, type = "common")
ds %>% group_by(sitter) %>%  get_summary_stats(sittingp, type = "common")

# Independent t-test
ds %>% t_test(sittingp ~ sitter)

ggboxplot(ds, x = "sitter", y = "sittingp", add = "jitter") + stat_compare_means(method = "t.test")

ggerrorplot(ds, x = "sitter", y = "sittingp", error.plot = "errorbar", add = "mean",
            desc_stat = "mean_sd", color = "black") + stat_compare_means(method = "t.test")

ds %>% t_test(sittingp ~ age_group)
ds %>% t_test(sittingp ~ age_group, ref.group = "3")

# ANOVA & Follow-ups
ds %>% anova_test(sittingp ~ age_group)
ds %>% anova_test(dv = sittingp, between = age_group)

ds %>% anova_test(sittingp ~ age_group*sitter) 

ds %>% emmeans_test(sittingp ~ age_group)

ggboxplot(ds, x = "age_group", y = "sittingp", add = "jitter") 

my_comparisons <- list(c("3", "6"), c("6", "9"), c("9", "12") )

ggboxplot(ds, x = "age_group", y = "sittingp", add = "jitter") +
  stat_compare_means(method = "anova", label.y = .75) + 
  stat_compare_means(comparisons = my_comparisons, method = "t.test",label.y = c(.4, .6, .7))

ggboxplot(ds, x = "age_group", y = "sittingp", add = "jitter") +
  stat_compare_means(method = "anova", label.y = .75) + 
  stat_compare_means(aes(label = after_stat(p.signif)), method = "t.test",
                         comparisons = my_comparisons, label.y = c(.4, .6, .7))

my_comparisons <- list(c("3", "6"),c("6", "9"), c("9", "12"), c("3", "9"),  c("6", "12"),  c("3", "12") )
ggerrorplot(ds, x = "age_group", y = "sittingp", error.plot = "errorbar", add = "mean",
            desc_stat = "mean_sd", color = "black") + 
  stat_compare_means(method = "anova", label.y = .75) + 
  stat_compare_means(aes(label = after_stat(p.signif)), method = "t.test",
                     comparisons = my_comparisons, label.y = c(.25, .45, .49, .56, .62, .68))

# Regression
res <- lm(sittingp ~ age, data = ds)
summary(res)

# Correlations
ds %>% cor_test(age, sittingp)
ds %>% cor_test(vars = c(age), vars2 = pronep:sittingp)

ds %>% cor_mat(age:sittingp)

ds %>% cor_mat(age:sittingp) %>% replace_upper_triangle(by = "")

ds %>% cor_mat(age:sittingp) %>% 
  replace_upper_triangle(by = "") %>%
  cor_mark_significant(cutpoints = c(0, 0.05, 1), symbols = c("*",""))

ds %>% cor_mat(age:sittingp) %>% 
  pull_lower_triangle() %>%
  cor_plot(label = TRUE)


