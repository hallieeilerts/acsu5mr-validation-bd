
# sample size exercise for an RCT or non-inferior trial design to test the NLP-nFPH vs conventional FPH and/or HDSS.

library(tidyr)
library(dplyr)
library(kableExtra)
library(ggplot2)
library(pwr)

fn_sampsize <- function(qx, period_months, exp_months,
                        effect_size, deff, agegrp,
                        design = c("rct", "noninf"),
                        delta = NULL,
                        n_max = 200000){
  
  design <- match.arg(design)
  
  # convert qx (probability over full period) to monthly hazard
  h_monthly <- -log(1 - qx) / period_months
  # multiply hazard by observed months and convert back to qx
  p_untreated <- 1 - exp(-h_monthly * exp_months)
  
  # calculate monthly hazard with new fph tool (treatment)
  h_treated <- h_monthly * (1 - effect_size)
  # convert to qx
  p_treated <- 1 - exp(-h_treated * exp_months)
  
  delta <- delta * p_treated
  
  # effect size
  if (design == "rct") {
    
    # effect size for two-proportion test (cohen's h)
    h <- ES.h(p_untreated, p_treated)
    alpha <- 0.05
    alt <- "two.sided"
    
  } else if (design == "noninf") {
    
    if (is.null(delta)) stop("delta must be specified for non-inferiority")
    # NI compares to margin
    h <- ES.h(p_untreated + delta, p_treated)
    alpha <- 0.025
    alt <- "greater"
    
  }
  
  # sample size (SRS n per group)
  n_srs <- pwr.2p.test(h = h, power = 0.8,
                       sig.level = 0.05,
                       alternative = alt)$n
  
  # nominal sample required for 80% power
  n_nominal_required <- ceiling(n_srs * deff)
  
  # power curve for a range of nominal sample sizes for each group
  v_n <- seq(0, n_max, 100)
  power = sapply(v_n, function(n) {
    if (n == 0) return(NA)  # skip n=0
    # effective n is reduced by DEFF
    n_eff <- n / deff
    if (n_eff < 1) return(NA)  # skip if too small
    pwr.2p.test(h = h, 
                n = n_eff,
                sig.level = alpha,
                alternative = alt)$power
  })
  
  power_table <- data.frame(
    n_nominal = v_n,
    n_effective = v_n / deff,
    power = power,
    qx = qx,
    exp_months = exp_months,
    effect_size = effect_size,
    design = design,
    delta = ifelse(design == "noninf", delta, NA),
    deff = deff,
    agegrp = agegrp,
    n_required80_nominal = n_nominal_required,
    n_required80_srs = ceiling(n_srs)
  )
  
  return(power_table)
}

# 2022 rates
qx_peri <- 24.6/1000
qx_neo <- 15.7/1000
qx_postneo <- 4.1/1000
qx_inf <- 19.8/1000
qx_child <- 1.2/1000
qx_under5 <- 24.3/1000
qx_lb <- 5602
# set for formula
my_pm <- 60
my_ep <- 60
my_deff <- 1.2

# effect_size = what we think the intervention will do
# delta = how much worse we are willing to tolerate

# effect_size determines where p_treatment is
# delta defines that cutoff we're comparing against

# “Are the tools different?” → use RCT/superiority (effect_size = 0.10)
# “Are the tools equivalent enough?” → use non-inferiority (delta)

# Null: p_treated - p_untreated ≥ delta (worse)
# Alternative: p_treated - p_untreated < delta (non-inferior)
# non-inferiority margin on the risk scale (5% worse?)
my_delta <- 0.1 # ten percent relative difference in mortality


# RCT 10% effect size
my_qx <- qx_under5
my_agegrp <- "q(5y)"
res1 <- fn_sampsize(qx = my_qx, period_months = my_pm, exp_months = my_ep, deff = my_deff, 
                    effect_size = 0.1, agegrp = my_agegrp, design = "rct")
my_qx <- qx_peri
my_agegrp <- "peri"
res2 <- fn_sampsize(qx = my_qx, period_months = my_pm, exp_months = my_ep, deff = my_deff, 
                    effect_size = 0.1, agegrp = my_agegrp, design = "rct")
my_qx <- qx_neo
my_agegrp <- "q(28d)"
res3 <- fn_sampsize(qx = my_qx, period_months = my_pm, exp_months = my_ep, deff = my_deff, 
                    effect_size = 0.1, agegrp = my_agegrp, design = "rct")
df_res1 <- rbind(res1, res2, res3)
# RCT 15% effect size
my_qx <- qx_under5
my_agegrp <- "q(5y)"
res1 <- fn_sampsize(qx = my_qx, period_months = my_pm, exp_months = my_ep, deff = my_deff, 
                    effect_size = 0.15, agegrp = my_agegrp, design = "rct")
my_qx <- qx_peri
my_agegrp <- "peri"
res2 <- fn_sampsize(qx = my_qx, period_months = my_pm, exp_months = my_ep, deff = my_deff, 
                    effect_size = 0.15, agegrp = my_agegrp, design = "rct")
my_qx <- qx_neo
my_agegrp <- "q(28d)"
res3 <- fn_sampsize(qx = my_qx, period_months = my_pm, exp_months = my_ep, deff = my_deff, 
                    effect_size = 0.15, agegrp = my_agegrp, design = "rct")
df_res2 <- rbind(res1, res2, res3)

# Non-inferiority 10% effect size
my_qx <- qx_under5
my_agegrp <- "q(5y)"
res1 <- fn_sampsize(qx = my_qx, period_months = my_pm, exp_months = my_ep, deff = my_deff, 
                    effect_size = 0.1, agegrp = my_agegrp, design = "noninf", delta = my_delta)
my_qx <- qx_peri
my_agegrp <- "peri"
res2 <- fn_sampsize(qx = my_qx, period_months = my_pm, exp_months = my_ep, deff = my_deff, 
                    effect_size = 0.1, agegrp = my_agegrp, design = "noninf", delta = my_delta)
my_qx <- qx_neo
my_agegrp <- "q(28d)"
res3 <- fn_sampsize(qx = my_qx, period_months = my_pm, exp_months = my_ep, deff = my_deff, 
                    effect_size = 0.1, agegrp = my_agegrp, design = "noninf", delta = my_delta)
df_res3 <- rbind(res1, res2, res3)
# RCT 15% effect size
my_qx <- qx_under5
my_agegrp <- "q(5y)"
res1 <- fn_sampsize(qx = my_qx, period_months = my_pm, exp_months = my_ep, deff = my_deff, 
                    effect_size = 0.15, agegrp = my_agegrp, design = "noninf", delta = my_delta)
my_qx <- qx_peri
my_agegrp <- "peri"
res2 <- fn_sampsize(qx = my_qx, period_months = my_pm, exp_months = my_ep, deff = my_deff, 
                    effect_size = 0.15, agegrp = my_agegrp, design = "noninf", delta = my_delta)
my_qx <- qx_neo
my_agegrp <- "q(28d)"
res3 <- fn_sampsize(qx = my_qx, period_months = my_pm, exp_months = my_ep, deff = my_deff, 
                    effect_size = 0.15, agegrp = my_agegrp, design = "noninf", delta = my_delta)
df_res4 <- rbind(res1, res2, res3)


df_allres <- rbind(df_res1, df_res2, df_res3, df_res4)
df_allres$myline <- paste0(round(df_allres$effect_size*100), "%")
df_allres$myline <- factor(df_allres$myline, levels = c("15%", "10%"))
df_allres$qx_char <- paste0(round(df_allres$qx*1000), " per 1,000 births")
df_allres$design <- factor(df_allres$design, levels = c("rct", "noninf"))

# recode facet names with rates
df_allres$facet <- ifelse(df_allres$agegrp == "peri", paste0("perinatal mortality \n", round(qx_peri*1000), " per 1,000 births"), df_allres$agegrp)
df_allres$facet <- ifelse(df_allres$agegrp == "q(28d)", paste0("q(28d) \n", round(qx_neo*1000), " per 1,000 births"), df_allres$facet)
df_allres$facet <- ifelse(df_allres$agegrp == "q(5y)", paste0("q(5y) \n", round(qx_under5*1000), " per 1,000 births"), df_allres$facet)

df_lab <- df_allres %>%
  select(facet, design, n_required80_nominal) %>%
  mutate(n_required80_nominal = round(n_required80_nominal/1000)) %>%
  distinct()

ggplot() +
  geom_line(data = df_allres, aes(x = n_nominal/1000, y = power, linetype = myline, color = design), size = 1) +
  geom_hline(yintercept = 0.8, color = "red") +
  geom_text(data = df_lab, aes(x = n_required80_nominal, y = 0.8, label = n_required80_nominal),
            size = 3, angle = 45, vjust = .5) +
  scale_linetype_manual(values = c("solid", "dashed"), name = "effect_size") +
  #scale_color_manual(values = c( "green", "green", "hotpink", "hotpink", "blue", "blue"), name = "Difference, coverage") +
  #scale_x_continuous(breaks = seq(0, 200, 25)) +
  #scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  coord_cartesian(xlim = c(0,125), ylim = c(0,1), expand = FALSE) +
  labs(x = "Sample size per group (1000s)", y = "Power", title = "2022 rates") +
  theme_minimal() +
  theme(text = element_text(size = 10)) +
  facet_wrap(~facet, nrow = 1)
