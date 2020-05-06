library(lme4)
library(dplyr)
library(MuMIn)
processed_data <- read.csv("~/Downloads/distances_data.csv")
soa_200 <- read.csv("~/Documents/cs-clps-final_project/200SOA.csv")
soa_1200 <- read.csv("~/Documents/cs-clps-final_project/1200SOA.csv")

small_data <- processed_data %>% dplyr::select(Unnamed..0,prime, target, rel,jc_similarity, path_similarity, cosine_distance)
small_data$jc_similarity[which(small_data$jc_similarity > 1)] <- 1
soa_200_complete <- merge(soa_200, small_data, by=c("prime", "target"))

# get specific relationship column
soa_200_complete$spec_rel <- as.numeric(!is.na(soa_200_complete$relation))

# get a dataset where all values are not NA for wordnet
dataset_200 <- soa_200_complete %>% filter(!is.na(jc_similarity)) %>% filter (!is.na(cosine_distance))

# get the same data for 1200
soa_1200_complete <- merge(soa_1200, small_data, by=c("prime", "target"))
soa_1200_complete$spec_rel <- as.numeric(!is.na(soa_1200_complete$relation))
dataset_1200 <- soa_1200_complete %>% filter(!is.na(jc_similarity)) %>% filter (!is.na(cosine_distance))

# model with just jc_similarity for soa200
model_wn_jc_200 <- lmer(formula = logRT ~ jc_similarity + t_length + p_length + p_logfreq + t_logfreq + p_orthoN + t_orthoN + (1|prime) + (1|target) + (1|Subject), data=dataset_200)
summary(model_wn_jc_200)

# model with both jc_similarity and relatedness for soa200
model_wn_jc_rel_200 <- lmer(formula = logRT ~ rel.y + jc_similarity + t_length + p_length + p_logfreq + t_logfreq + p_orthoN + t_orthoN + (1|prime) + (1|target) + (1|Subject), data=dataset_200)
summary(model_wn_jc_rel_200)

# model with interaction term b/w jc_similarity & specific relation for soa200
model_wn_interaction_200 <- lmer(formula = logRT ~ rel.y + jc_similarity + jc_similarity*spec_rel + t_length + p_length + p_logfreq + t_logfreq + p_orthoN + t_orthoN + (1|prime) + (1|target) + (1|Subject), data=dataset_200)
summary(model_wn_interaction_200)

# model with just cosine for 200
model_cosine_200 <- lmer(formula = logRT ~ cosine_distance + t_length + p_length + p_logfreq + t_logfreq + p_orthoN + t_orthoN + (1|prime) + (1|target) + (1|Subject), data=dataset_200)
summary(model_cosine_200)

# model with cosine and relatedness for 200
model_cosine_rel_200 <- lmer(formula = logRT ~ cosine_distance + rel.y + t_length + p_length + p_logfreq + t_logfreq + p_orthoN + t_orthoN + (1|prime) + (1|target) + (1|Subject), data=dataset_200)
summary(model_cosine_rel_200)

# model with interaction term for 200
model_cosine_interaction_200 <- lmer(formula = logRT ~ cosine_distance + rel.y + cosine_distance*spec_rel + t_length + p_length + p_logfreq + t_logfreq + p_orthoN + t_orthoN + (1|prime) + (1|target) + (1|Subject), data=dataset_200)
summary(model_cosine_interaction_200)

# repeat it all for 1200
# model with just jc_similarity for soa1200
model_wn_jc_1200 <- lmer(formula = logRT ~ jc_similarity + t_length + p_length + p_logfreq + t_logfreq + p_orthoN + t_orthoN + (1|prime) + (1|target) + (1|Subject), data=dataset_1200)
summary(model_wn_jc_1200)

# model with both jc_similarity and relatedness for soa1200
model_wn_jc_rel_1200 <- lmer(formula = logRT ~ rel.y + jc_similarity + t_length + p_length + p_logfreq + t_logfreq + p_orthoN + t_orthoN + (1|prime) + (1|target) + (1|Subject), data=dataset_1200)
summary(model_wn_jc_rel_1200)

# model with interaction term b/w jc_similarity & specific relation for soa1200
model_wn_interaction_1200 <- lmer(formula = logRT ~ rel.y + jc_similarity + jc_similarity*spec_rel + t_length + p_length + p_logfreq + t_logfreq + p_orthoN + t_orthoN + (1|prime) + (1|target) + (1|Subject), data=dataset_1200)
summary(model_wn_interaction_1200)


# model with just cosine for soa1200
model_cosine_1200 <- lmer(formula = logRT ~ cosine_distance + t_length + p_length + p_logfreq + t_logfreq + p_orthoN + t_orthoN + (1|prime) + (1|target) + (1|Subject), data=dataset_1200)
summary(model_cosine_1200)

# model with cosine and relatedness for soa1200
model_cosine_rel_1200 <- lmer(formula = logRT ~ cosine_distance + rel.y + t_length + p_length + p_logfreq + t_logfreq + p_orthoN + t_orthoN + (1|prime) + (1|target) + (1|Subject), data=dataset_1200)
summary(model_cosine_rel_1200)

# model with interaction term for soa1200
model_cosine_interaction_1200 <- lmer(formula = logRT ~ cosine_distance + rel.y + cosine_distance*spec_rel + t_length + p_length + p_logfreq + t_logfreq + p_orthoN + t_orthoN + (1|prime) + (1|target) + (1|Subject), data=dataset_1200)
summary(model_cosine_interaction_1200)


# retrain baseline models on the smaller datasets
baseline_200 <- lmer(formula=logRT ~ rel.y + t_length + p_length + p_logfreq + t_logfreq + p_orthoN + t_orthoN + (1|prime) + (1|target) + (1|Subject), data=dataset_200)

baseline_1200 <- lmer(formula=logRT ~ rel.y + t_length + p_length + p_logfreq + t_logfreq + p_orthoN + t_orthoN + (1|prime) + (1|target) + (1|Subject), data=dataset_1200)


# r-squared for different models
r.squaredGLMM(model_wn_jc_200)
r.squaredGLMM(model_wn_jc_rel_200)
r.squaredGLMM(model_wn_interaction_200)
r.squaredGLMM(model_cosine_200)
r.squaredGLMM(model_cosine_rel_200)
r.squaredGLMM(model_cosine_interaction_200)
r.squaredGLMM(model_wn_jc_1200)
r.squaredGLMM(model_wn_jc_rel_1200)
r.squaredGLMM(model_wn_interaction_1200)
r.squaredGLMM(model_cosine_1200)
r.squaredGLMM(model_cosine_rel_1200)
r.squaredGLMM(model_cosine_interaction_1200)
r.squaredGLMM(baseline_200)
r.squaredGLMM(baseline_1200)


# anova comparisons
# baseline-200-wn vs model_wn_jc_rel_200
a1 <- anova(baseline_200, model_wn_jc_rel_200)
# model_wn_jc_rel_200 vs model_wn_interaction_200
a2 <- anova(model_wn_jc_rel_200, model_wn_interaction_200)
# and same for the other datasets
a3 <- anova(baseline_1200, model_wn_jc_rel_1200)
a4 <- anova(model_wn_jc_rel_1200, model_wn_interaction_1200)
a5 <- anova(baseline_200, model_cosine_rel_200)
a6 <- anova(model_cosine_rel_200, model_cosine_interaction_200)
a7 <- anova(baseline_1200, model_cosine_rel_1200)
a8 <- anova(model_cosine_rel_1200, model_cosine_interaction_1200)


# use AIC to determine whether wordnet or cosine is better
comp1 <- AIC(model_wn_jc_200, model_cosine_200)
comp2 <- AIC(model_wn_jc_1200, model_cosine_1200)
comp3 <- AIC(model_wn_jc_rel_200, model_cosine_rel_200)
comp4 <- AIC(model_wn_jc_rel_1200, model_cosine_rel_1200)
comp5 <- AIC(model_wn_interaction_200, model_cosine_interaction_200)
comp6 <- AIC(model_wn_interaction_1200, model_cosine_interaction_1200)
AIC(model_wn_jc_200, model_cosine_200, model_wn_jc_rel_200, model_cosine_rel_200, model_wn_interaction_200, model_cosine_interaction_200)
AIC(model_wn_jc_1200, model_cosine_1200, model_wn_jc_rel_1200, model_cosine_rel_1200, model_wn_interaction_1200, model_cosine_interaction_1200)
