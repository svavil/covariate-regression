covariate.regression <- function(dependent, predictor, data, dependent.units = "", predictor.units = "") {
  require(tidyverse)
  theme_set(theme_bw())
  theme_update(panel.grid.minor = element_blank())
  
  data_f <- data %>% filter(!is.na(!!ensym(dependent))) %>% filter(!is.na(!!ensym(predictor)))
  linreg <- lm(as.formula(paste0(dependent, " ~ ", predictor)), data = data_f, weights = Nsub)
  f <- summary(linreg)$fstatistic
  pval <- pf(f[1], f[2], f[3], lower.tail = F)
  desc <- paste0("Rsq = ", signif(summary(linreg)$r.squared, 3), ", p-value = ", signif(pval, 3))
  yticks <- signif(seq(min(data_f[[dependent]]), max(data_f[[dependent]]), length.out = 10), 2)
  if (is.numeric(data_f[[predictor]])) {
    xticks <- signif(seq(signif(min(data_f[[predictor]]),1), signif(max(data_f[[predictor]]), 1), length.out = 10), 2)
    p <- ggplot(data = data_f) + 
      geom_point(aes(x = !!ensym(predictor), y = !!ensym(dependent), size = Nsub)) + 
      geom_hline(aes(yintercept = 0), linetype = "dashed", color = "gray50") + 
      labs(x = paste(predictor, predictor.units), y = paste(dependent, dependent.units)) + 
      geom_smooth(aes(x = !!ensym(predictor), y = !!ensym(dependent), weight = Nsub), method = "lm") + 
      geom_label(aes(x = 0.5*(max(data_f[[predictor]]) + min(data_f[[predictor]])), y = 0.9*max(data_f[[dependent]])), label = desc) +
      scale_x_continuous(breaks = xticks) + 
      scale_y_continuous(breaks = yticks)
  } else {
    p <- ggplot(data = data) + 
      geom_boxplot(aes(x = !!ensym(predictor), y = !!ensym(dependent))) + 
      geom_hline(aes(yintercept = 0), linetype = "dashed", color = "gray50") + 
      labs(x = predictor, y = dependent, caption = desc) + 
      scale_y_continuous(breaks = yticks)
  }
  ggsave(filename = paste0(dependent, "_vs_", predictor, ".png"), plot = p, width = 8)
  return(invisible(p))
}
