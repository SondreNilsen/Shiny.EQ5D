# ====== Information about this script =====
# This script demonstrates how to:
#   1. Simulate EQ-5D data (including hospital + year effects)
#   2. (Optionally) load your own data from SPSS, Stata, Excel, or CSV into `df`
#   3. Reshape data wide→long with pivot_longer()
#   4. Create:
#        • Bar Charts (with % labels, faceted or grouped)
#        • Stacked Bar Charts (fixed to 100% + % labels inside)
#        • EQ VAS: Histogram, Density, Boxplot, Ridgeline
#        • Trend Plots: observed means, LM, LOESS (with facet)
#   5. Customize with theme_nice(), uchicago palettes, reference‐line examples
#   6. (Commented) ggsave() calls for PDF & PNG export with sensible sizes & dpi

# ===== 1. Installing and loading packages =====
packages <- c(
  "ggplot2","dplyr","tidyr","forcats","scales",
  "viridis","RColorBrewer","ggsci","ggridges","ggdist"
)
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packages, library, character.only = TRUE)

# ===== 2. Defining a custom theme =====
theme_nice <- function(base_size = 14) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid       = element_blank(),
      strip.background = element_blank(),
      strip.text       = element_text(size = base_size)
    )
}

# ------------------------------------
# 3. DATA SIMULATION: EQ-5D + Hospital + Year Effects
# ------------------------------------
set.seed(123)
n <- 2000
gender        <- sample(c("male","female"), n, replace = TRUE)
age_numeric   <- sample(18:100, n, replace = TRUE)
age_breaks    <- c(18,30,40,50,60,70,80,90,101)
age_labels    <- c("18-29","30-39","40-49","50-59","60-69","70-79","80-89","90+")
age           <- cut(age_numeric, breaks = age_breaks, labels = age_labels, right = FALSE)
treatment     <- factor(sample(c("pre","post"), n, replace = TRUE), levels = c("pre","post"), ordered = TRUE)
marital_status<- factor(sample(c("married","divorced/separated","single","widowed"), n, replace = TRUE,
                               prob = c(0.5,0.2,0.2,0.1)),
                        levels = c("married","divorced/separated","single","widowed"), ordered = TRUE)
smoking       <- sample(c("yes","no"), n, replace = TRUE, prob = c(0.3,0.7))
education     <- factor(sample(c("Low","Medium","High"), n, replace = TRUE, prob = c(0.3,0.4,0.3)),
                        levels = c("Low","Medium","High"), ordered = TRUE)
income        <- factor(sample(c("Low","Medium","High"), n, replace = TRUE, prob = c(0.3,0.4,0.3)),
                        levels = c("Low","Medium","High"), ordered = TRUE)

# Hospital effect (15 levels)
hospital_levels  <- paste0("hospital ", 1:15)
hospital         <- factor(sample(hospital_levels, n, replace = TRUE),
                           levels = hospital_levels, ordered = TRUE)
hospital_offsets <- seq(-25,25,length.out=15) + rnorm(15,0,3)
names(hospital_offsets) <- hospital_levels

simulate_dim <- function(mean_target, smoker, age, education) {
  b <- round(rnorm(1, mean = mean_target, sd = 1))
  b <- b + ifelse(smoker=="yes", 2, 0)
  b <- b + ifelse(age %in% c("60-69","70-79","80-89","90+"), 1,
                  ifelse(age %in% c("18-29","30-39"), -1, 0))
  b <- b + ifelse(education=="Low", 1, ifelse(education=="High", -1, 0))
  round(min(max(b, 1), 5))
}
MO <- mapply(simulate_dim, 2.5, smoking, age, education)
SC <- mapply(simulate_dim, 2.8, smoking, age, education)
UA <- mapply(simulate_dim, 3.1, smoking, age, education)
PD <- mapply(simulate_dim, 3.4, smoking, age, education)
AD <- mapply(simulate_dim, 3.8, smoking, age, education)

simulate_vas <- function(baseline, gender, income, education,
                         marital_status, treatment, smoker, age, hospital) {
  b <- qnorm(runif(1, pnorm(-1.2), pnorm(1.2))) * 8 + baseline
  b <- b + ifelse(gender=="male", 3, ifelse(gender=="female", -3, 0))
  b <- pmin(pmax(b, 0), 100)
  b <- b + ifelse(income=="Low",-15, ifelse(income=="High",15,0))
  b <- b + ifelse(education=="Low",-10, ifelse(education=="High",7,0))
  b <- b + ifelse(marital_status=="married",8,
                  ifelse(marital_status=="divorced/separated",-7,
                         ifelse(marital_status=="widowed",-10,0)))
  b <- b + ifelse(treatment=="post",10,0)
  b <- b + ifelse(smoker=="yes",-10,0)
  b <- b - ifelse(age %in% c("60-69","70-79","80-89","90+"), 8, 0)
  b <- b + hospital_offsets[as.character(hospital)]
  round(min(max(b, 0),100))
}
EQ_VAS <- mapply(simulate_vas, baseline = 60,
                 gender, income, education, marital_status,
                 treatment, smoking, age, hospital)

df <- data.frame(
  MO, SC, UA, PD, AD,
  gender, age, EQ_VAS, treatment,
  marital_status, smoking, education, income,
  hospital
)

# Add year + sine‐wave effect ±10 pts
df <- df %>%
  mutate(
    year = rep(2018:2025, each = n()/8),
    EQ_VAS = round(pmin(100, pmax(0,
                                  EQ_VAS + sin(2*pi*(year-2018)/7)*10
    )))
  )

# ------------------------------------
# 4. (Optional) LOADING YOUR OWN DATA
# ------------------------------------
# If you want to replace the simulated `df`, uncomment and adjust one of:
#
# # SPSS (.sav)
# # library(haven)
# # df <- read_sav("your_data.sav")
# # df <- df %>% mutate(across(where(is.labelled), as_factor))
#
# # Stata (.dta)
# # library(haven)
# # df <- read_dta("your_data.dta")
#
# # Excel (.xls/.xlsx)
# # library(readxl)
# # df <- read_excel("your_data.xlsx")
#
# # CSV (.csv)
# # library(readr)
# # df <- read_csv("your_data.csv")
#
# # VARIABLE CODING GUIDELINES
# # --------------------------
# # • Your EQ-5D profile columns must be named MO, SC, UA, PD, AD
# #   and coded 1–5 (either numeric or factor).
# # • Your EQ VAS column must be named EQ_VAS and coded 0–100 (numeric).
# # • If you use different group variables (e.g., “treatment_arm” instead of “gender”),
# #   be sure to update every reference in the plotting code:
# #     – In the data‐prep pipelines (group_by, mutate, etc.)
# #     – In aes(fill=…), aes(colour=…), facet_wrap(~…), labs(fill=…), etc.
# #

# ------------------------------------
# 5. Data Preparation: Pivot Wide→Long
# ------------------------------------
df_long <- df %>%
  pivot_longer(
    cols      = c(MO,SC,UA,PD,AD),
    names_to  = "Dimension",
    values_to = "Response"
  )

# ------------------------------------
# 6. Labels for facets & scales
# ------------------------------------
# EQ-5D dimension labels (English & Norwegian) and response levels
eq5d_dim_eng <- c(
  MO = "Mobility (MO)",
  SC = "Self-Care (SC)",
  UA = "Usual Activities (UA)",
  PD = "Pain/Discomfort (PD)",
  AD = "Anxiety/Depression (AD)"
)

eq5d_dim_nor <- c(
  MO = "Mobilitet (MO)",
  SC = "Egenomsorg (SC)",
  UA = "Vanlige aktiviteter (UA)",
  PD = "Smerte/ubehag (PD)",
  AD = "Angst/Depresjon (AD)"
)

eq5d_levels <- c("1","2","3","4","5")

eq5d_labels_eng <- c(
  "No problem", "Slight problem", "Moderate problems",
  "Severe problems", "Extreme problems"
)

eq5d_labels_nor <- c(
  "Ingen problemer", "Litt problemer", "Moderate problemer",
  "Alvorlige problemer", "Ekstreme problemer"
)
# =============================
# 7. BAR CHARTS
# =============================
df_bar <- df_long %>%
  group_by(Dimension,Response) %>%
  summarise(count=n(), .groups="drop") %>%
  group_by(Dimension) %>%
  mutate(proportion = count/sum(count)) %>%
  ungroup()

# 7A. All Dimensions
p_bar_all <- ggplot(df_bar,
                    aes(
                      x     = factor(Response, levels=eq5d_levels, labels=eq5d_labels_eng),
                      y     = proportion,
                      fill  = factor(Response, levels=eq5d_levels, labels=eq5d_labels_eng)
                    )) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(round(proportion*100,1),"%")),
            vjust=-0.3, size=3) +
  facet_wrap(~Dimension, labeller=labeller(Dimension=eq5d_dim_eng)) +
  scale_y_continuous(labels=percent_format(), expand=expansion(mult=c(0,0.05))) +
  scale_fill_brewer(palette="Blues", name="", labels=eq5d_labels_eng) +
  labs(title="Distribution of EQ-5D responses", x="", y="Percent") +
  theme_nice()
print(p_bar_all)
# ggsave("bar_all.pdf", p_bar_all, width=12, height=6)
# ggsave("bar_all.png", p_bar_all, width=12, height=6, dpi=1000)

# 7B. By Gender
df_bar_gender <- df_long %>%
  group_by(Dimension,Response,gender) %>%
  summarise(count=n(), .groups="drop") %>%
  group_by(Dimension,gender) %>%
  mutate(proportion = count/sum(count)) %>%
  ungroup()

p_bar_gender <- ggplot(df_bar_gender,
                       aes(
                         x    = factor(Response, levels=eq5d_levels, labels=eq5d_labels_eng),
                         y    = proportion,
                         fill = gender
                       )) +
  geom_bar(stat="identity", position=position_dodge(width=0.9)) +
  geom_text(aes(label=paste0(round(proportion*100,1),"%")),
            position=position_dodge(width=0.9), vjust=-0.3, size=3) +
  facet_wrap(~Dimension, labeller=labeller(Dimension=eq5d_dim_eng)) +
  scale_fill_uchicago() +
  labs(title="EQ-5D responses by gender", x="", y="Percent", fill="Gender") +
  scale_y_continuous(labels=percent_format(), expand=expansion(mult=c(0,0.05))) +
  theme_nice()
print(p_bar_gender)
# ggsave("bar_gender.pdf", p_bar_gender, width=12, height=6)
# ggsave("bar_gender.png", p_bar_gender, width=12, height=6, dpi=1000)

# 7C. Single Dimension (MO), no group
df_bar_mo <- df_bar %>% filter(Dimension=="MO")
p_bar_mo <- ggplot(df_bar_mo,
                   aes(
                     x    = factor(Response, levels=eq5d_levels, labels=eq5d_labels_eng),
                     y    = proportion,
                     fill = factor(Response, levels=eq5d_levels, labels=eq5d_labels_eng)
                   )) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(round(proportion*100,1),"%")),
            vjust=-0.3, size=3) +
  scale_fill_brewer(palette="Blues", name="", labels=eq5d_labels_eng) +
  labs(title="Mobility (MO) response distribution", x="", y="Percent") +
  scale_y_continuous(labels=percent_format(), expand=expansion(mult=c(0,0.05))) +
  theme_nice()
print(p_bar_mo)
# ggsave("bar_mo.pdf", p_bar_mo, width=8, height=5)
# ggsave("bar_mo.png", p_bar_mo, width=8, height=5, dpi=1000)

# 7D. Single MO by Gender
df_bar_mo_g <- df_long %>%
  filter(Dimension=="MO") %>%
  group_by(Response,gender) %>%
  summarise(count=n(), .groups="drop") %>%
  group_by(gender) %>%
  mutate(proportion = count/sum(count)) %>%
  ungroup()

p_bar_mo_g <- ggplot(df_bar_mo_g,
                     aes(
                       x    = factor(Response, levels=eq5d_levels, labels=eq5d_labels_eng),
                       y    = proportion,
                       fill = gender
                     )) +
  geom_bar(stat="identity", position=position_dodge(width=0.9)) +
  geom_text(aes(label=paste0(round(proportion*100,1),"%")),
            position=position_dodge(width=0.9), vjust=-0.3, size=3) +
  scale_fill_uchicago() +
  labs(title="MO response distribution by gender", x="", y="Percent", fill="Gender") +
  scale_y_continuous(labels=percent_format(), expand=expansion(mult=c(0,0.05))) +
  theme_nice()
print(p_bar_mo_g)
# ggsave("bar_mo_gender.pdf", p_bar_mo_g, width=8, height=5)
# ggsave("bar_mo_gender.png", p_bar_mo_g, width=8, height=5, dpi=1000)

# =============================
# 8. STACKED BAR CHARTS (with English labels & proper order)
# =============================
df_stack <- df_long %>%
  mutate(
    Response = factor(Response,
                      levels = eq5d_levels,
                      labels = eq5d_labels_eng,
                      ordered = TRUE
    )
  ) %>%
  arrange(Dimension, Response) %>%
  group_by(Dimension, Response) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Dimension) %>%
  mutate(pct = count / sum(count) * 100) %>%
  ungroup()

# 8A. No group
p_stack_all <- ggplot(df_stack, aes(x = Dimension, y = pct, fill = Response)) +
  geom_bar(stat = "identity", position = "stack", colour = "white") +
  geom_text(aes(label = paste0(round(pct,1),"%")),
            position = position_stack(vjust = 0.5),
            colour = "black", size = 3) +
  scale_y_continuous(labels = function(x) paste0(x,"%"),
                     expand = expansion(mult = c(0,0.02))) +
  scale_fill_brewer(palette = "Blues", name="", labels=eq5d_labels_eng) +
  labs(title = "Stacked EQ-5D Responses (no group)",
       x = "Dimension", y = "Percent") +
  theme_nice()
print(p_stack_all)
# ggsave("stack_all.pdf", p_stack_all, width=10, height=6)
# ggsave("stack_all.png", p_stack_all, width=10, height=6, dpi=1000)

# 8B. By Smoking
df_stack_smoke <- df_long %>%
  mutate(
    Response = factor(Response,
                      levels = eq5d_levels,
                      labels = eq5d_labels_eng,
                      ordered = TRUE
    )
  ) %>%
  arrange(Dimension, smoking, Response) %>%
  group_by(Dimension, smoking, Response) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Dimension, smoking) %>%
  mutate(pct = count / sum(count) * 100) %>%
  ungroup()

p_stack_smoke <- ggplot(df_stack_smoke,
                        aes(x = smoking, y = pct, fill = Response)) +
  geom_bar(stat = "identity", position = "stack", colour = "white") +
  geom_text(aes(label = paste0(round(pct,1),"%")),
            position = position_stack(vjust = 0.5),
            colour = "black", size = 3) +
  facet_wrap(~Dimension, labeller = labeller(Dimension = eq5d_dim_eng)) +
  scale_y_continuous(labels = function(x) paste0(x,"%"),
                     expand = expansion(mult = c(0,0.02))) +
  scale_fill_brewer(palette = "Blues", name="", labels=eq5d_labels_eng) +
  labs(title = "Stacked EQ-5D Responses by Smoking",
       x = "Smoking Status", y = "Percent") +
  theme_nice()
print(p_stack_smoke)
# ggsave("stack_smoke.pdf", p_stack_smoke, width=12, height=8)
# ggsave("stack_smoke.png", p_stack_smoke, width=12, height=8, dpi=1000)

# =============================
# 9. EQ VAS PLOTS
# =============================
# 9A. Histogram no group
p_vas_hist <- ggplot(df, aes(x=EQ_VAS)) +
  geom_histogram(binwidth=5, fill="darkblue", colour="white") +
  labs(title="EQ VAS Histogram", x="EQ VAS Score", y="Count") +
  theme_nice()
print(p_vas_hist)
# ggsave("vas_hist.pdf", p_vas_hist, width=8, height=5)
# ggsave("vas_hist.png", p_vas_hist, width=8, height=5, dpi=1000)

# 9B. Histogram by Education
p_vas_hist_ed <- ggplot(df, aes(x=EQ_VAS, fill=education)) +
  geom_histogram(position="dodge", binwidth=5, colour="black") +
  scale_fill_uchicago() +
  labs(title="EQ VAS Histogram by Education", x="EQ VAS", y="Count", fill="Education") +
  theme_nice()
print(p_vas_hist_ed)
# ggsave("vas_hist_ed.pdf", p_vas_hist_ed, width=8, height=5)
# ggsave("vas_hist_ed.png", p_vas_hist_ed, width=8, height=5, dpi=1000)

# 9C. Density no group
p_vas_den <- ggplot(df, aes(x=EQ_VAS)) +
  geom_density(fill="grey", colour="black") +
  labs(title="EQ VAS Density", x="EQ VAS", y="Density") +
  theme_nice()
print(p_vas_den)
# ggsave("vas_den.pdf", p_vas_den, width=8, height=5)
# ggsave("vas_den.png", p_vas_den, width=8, height=5, dpi=1000)

# 9D. Density by Education
p_vas_den_ed <- ggplot(df, aes(x=EQ_VAS, fill=education, colour=education)) +
  geom_density(alpha=0.5) +
  scale_fill_uchicago() + scale_colour_uchicago() +
  labs(title="EQ VAS Density by Education", x="EQ VAS", y="Density") +
  theme_nice()
print(p_vas_den_ed)
# ggsave("vas_den_ed.pdf", p_vas_den_ed, width=8, height=5)
# ggsave("vas_den_ed.png", p_vas_den_ed, width=8, height=5, dpi=1000)

# 9E. Boxplot no group
p_vas_box <- ggplot(df, aes(x=EQ_VAS, y=hospital)) +
  geom_boxplot(colour="black", fill="#6495ED") +
  coord_cartesian(xlim=c(0,100)) +
  labs(title="EQ VAS Boxplot by Hospital", x="EQ VAS", y="Hospital") +
  theme_nice()
print(p_vas_box)
# ggsave("vas_box.pdf", p_vas_box, width=10, height=6)
# ggsave("vas_box.png", p_vas_box, width=10, height=6, dpi=1000)

# 9F. Ridgeline
p_vas_ridge <- ggplot(df, aes(x=EQ_VAS, y=hospital, fill=after_stat(x))) +
  geom_density_ridges_gradient(scale=1, rel_min_height=0.01) +
  scale_fill_viridis_c(option="viridis", name="EQ VAS") +
  labs(title="EQ VAS Ridgeline by Hospital", x="EQ VAS Score", y="Hospital") +
  theme_nice()
print(p_vas_ridge)
# ggsave("vas_ridge.pdf", p_vas_ridge, width=10, height=8)
# ggsave("vas_ridge.png", p_vas_ridge, width=10, height=8, dpi=1000)

# =============================
# 10. TREND PLOTS
# =============================
# 10A. Observed means
p_trend_obs <- ggplot(df, aes(x=year, y=EQ_VAS, colour=gender, fill=gender)) +
  stat_summary(fun=mean, geom="line", size=1) +
  stat_summary(fun=mean, geom="point", shape=21, size=5) +
  scale_fill_uchicago() + scale_color_uchicago() +
  labs(title="Observed Mean EQ VAS by Year & Gender", x="Year", y="Mean EQ VAS") +
  theme_nice()
print(p_trend_obs)
# ggsave("trend_obs.pdf", p_trend_obs, width=10, height=5)
# ggsave("trend_obs.png", p_trend_obs, width=10, height=5, dpi=1000)

# 10B. Linear trend (LM)
p_trend_lm <- ggplot(df, aes(x=year, y=EQ_VAS, colour=gender, fill=gender)) +
  geom_smooth(method="lm", se=TRUE) +
  scale_fill_uchicago() + scale_color_uchicago() +
  labs(title="Linear Trend EQ VAS by Gender", x="Year", y="EQ VAS") +
  theme_nice()
print(p_trend_lm)
# ggsave("trend_lm.pdf", p_trend_lm, width=10, height=5)
# ggsave("trend_lm.png", p_trend_lm, width=10, height=5, dpi=1000)

# 4) LOESS trend by gender
# geom_smooth() with no method will:
#   • use LOESS if total n < 1000,
#   • switch to GAM if n ≥ 1000 (which may fail with few unique x's).
# Here we explicitly force LOESS to capture the non‐linear sine‐wave.

# 10C. LOESS trend
p_trend_loess <- ggplot(df, aes(x=year, y=EQ_VAS, colour=gender, fill=gender)) +
  geom_smooth(method="loess", se=TRUE, size=1.2) +
  scale_fill_uchicago() + scale_color_uchicago() +
  labs(title="LOESS Trend EQ VAS by Gender", x="Year", y="EQ VAS") +
  coord_cartesian(ylim=c(0,100)) +
  theme_nice()
print(p_trend_loess)
# ggsave("trend_loess.pdf", p_trend_loess, width=10, height=5)
# ggsave("trend_loess.png", p_trend_loess, width=10, height=5, dpi=1000)

# 10D. LOESS trend faceted by age
p_trend_loess_age <- ggplot(df, aes(x=year, y=EQ_VAS, colour=education, fill=education)) +
  geom_smooth(method="loess", se=TRUE, size=1.2, alpha = 0.2) +
  facet_wrap(~age) +
  coord_cartesian(ylim=c(0,100)) +
  scale_color_uchicago() + scale_fill_uchicago() +
  labs(title="LOESS Trend of EQ VAS by Education & Age", x="Year", y="EQ VAS") +
  theme_nice()
print(p_trend_loess_age)
# ggsave("trend_loess_age.pdf", p_trend_loess_age, width=12, height=8)
# ggsave("trend_loess_age.png", p_trend_loess_age, width=12, height=8, dpi=1000)



# ---- Common ggplot2 Tweaks ----
# size:
#   - Controls line thickness (in geom_line(), geom_smooth(), etc.)
#     or text size (in geom_text()).
#   - Larger values yield bolder lines or larger labels; smaller values
#     produce finer or more delicate elements.
#
# alpha:
#   - Governs transparency on a 0–1 scale (0 = fully transparent,
#     1 = fully opaque).
#   - Useful for overlaid geoms (histograms, density fills,
#     ridgelines) so you can still see underlying layers.
#
# se = TRUE (in geom_smooth()):
#   - Enables the shaded confidence interval around your smoothing line.
#   - To hide it, set se = FALSE.
#   - To change the interval width, use level = 0.8 (80%) or another value.
#
# coord_cartesian(xlim = ..., ylim = ...):
#   - Zooms into a region without dropping data points (unlike
#     xlim()/ylim() in scale_*).
#   - Handy for focusing axes (e.g., ylim = c(20, 80) on a 0–100 scale).

# ---- Additional Handy Options ----
# Custom breaks & labels:
#   scale_x_continuous(breaks = c(1,3,5),
#                      labels = c("Low","Medium","High"))
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1))
#
# Faceting tweaks:
#   facet_wrap(~group, nrow = 2, drop = TRUE)
#
# Legend positioning & styling:
#   theme(legend.position = "bottom")     # or "none" to hide
#   guides(fill = guide_legend(keywidth = unit(2, "lines")))
#
# Reference lines:
#   geom_hline(yintercept = 50, linetype = "dashed")
#   geom_vline(xintercept = 2020, linetype = "dotdash")
#
# Annotations & captions:
#   annotate("text", x = 2020, y = 90,
#            label = "Peak", size = 4)
#   labs(caption = "Source: Simulated data")
#
# Themes & fonts:
#   theme_minimal(), theme_classic(), etc.
#   theme(text = element_text(family = "Helvetica"))

# ---- Color Palettes ----
# RColorBrewer:
#   scale_fill_brewer(palette = "Set2")
#   # Example hex codes: c("#66C2A5","#FC8D62","#8DA0CB","#E78AC3","#A6D854")
#
# viridis discrete:
#   scale_fill_viridis_d(option = "magma")
#
# ggsci palettes:
#   scale_fill_uchicago()
#   scale_fill_npg()
#   scale_fill_lancet()

# ---- Manual Colours ----
# by gender:
#   scale_fill_manual(values = c(
#     "male"   = "#1f78b4",
#     "female" = "#e31a1c"
#   ))
#
# by age group:
#   scale_fill_manual(values = c(
#     "18-29" = "#a6cee3", "30-39" = "#1f78b4",
#     "40-49" = "#b2df8a", "50-59" = "#33a02c",
#     "60-69" = "#fb9a99", "70-79" = "#e31a1c",
#     "80-89" = "#fdbf6f", "90+"   = "#ff7f00"
#   ))

# ---- Colour Inspiration ----
# See https://colorbrewer2.org for more palettes and hex codes.

