library(ggplot2)
library(reshape2)

size_mod <- -1
cool_theme <- theme(plot.background=element_rect(fill = "transparent",
                                                 colour = "transparent"),
                    panel.grid.major = element_line(colour="lightgrey", linetype = "dashed"),
                    panel.background = element_rect(fill = "white", colour = "black"),
                    legend.background = element_rect(fill="NA"),
                    legend.position = "bottom",
                    axis.text = element_text(size = 12 + size_mod),
                    axis.title.x = element_text(size = 15 + size_mod, vjust = -0.1), 
                    axis.title.y = element_text(size = 15 + size_mod, vjust = 1),
                    strip.text = element_text(size = 15 + size_mod, face = "bold"),
                    strip.background = element_rect(fill = "#9ecae1", colour = "black"),
                    legend.text = element_text(size = 12 + size_mod), 
                    legend.title = element_text(size = 15 + size_mod),
                    legend.key = element_rect(fill = "white", colour = "black", size = 0.1),
                    plot.title = element_text(size = 20 + size_mod))

load("test_count_eval.RData")


# ratiovsGLM plot --------------------------

vline_data <- aggregate(value ~ method, data = madpcr_comp, FUN = mean)

pdf(file = "ratiovsGLM.pdf", pointsize = 1)
ggplot(data=madpcr_comp, aes(x = value, fill = method, colour = method)) +
  geom_density(alpha = 0.5, size = 1.2) + 
  scale_fill_manual("Multiple comparisons framework:", 
                    values = c("GLM" = "chocolate1", "MT" = "skyblue")) + 
  scale_colour_manual("Multiple comparisons framework:", 
                      values = c("GLM" = "chocolate1", "MT" = "skyblue")) + 
  scale_y_continuous("Density") + 
  scale_x_continuous("Fraction of wrongly assigned experiments") + 
  geom_vline(data = vline_data, mapping = aes(xintercept = value, colour = method), 
             size = 1.2, alpha = 0.9) +
  cool_theme
dev.off()


# coverage plot --------------------------

levels(m_coverage2[["method"]]) <- c("Adjusted\n(this study)", "Bhat", "Dube")

pdf(file = "coverage.pdf", pointsize = 1)
ggplot(m_coverage2, aes(x = prop, y = value, fill = method)) +
  geom_bar(stat="identity", position = "dodge") +
  scale_y_continuous("Probability coverage") + 
  scale_x_discrete(expression(lambda)) +
  scale_fill_discrete("Confidence intervals:") + 
  geom_hline(y = 0.95, colour = "black", size = 1.2, linetype = 5) +
  facet_wrap(~ coverage, nrow = 2) + 
  cool_theme
dev.off()

pdf(file = "success_rate.pdf", pointsize = 1)
ggplot(sucrate, aes(x = as.factor(m), y = mp_value, fill = test)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous("Mean p-value") + 
  scale_fill_manual("Multiple comparisons framework:", 
                    values = c("GLM" = "chocolate1", "MT" = "skyblue")) + 
#   scale_colour_manual("Multiple comparisons framework:", 
#                       values = c("GLM" = "chocolate1", "MT" = "skyblue")) + 
  geom_errorbar(aes(ymax = mp_value + sdp_value, ymin = mp_value - sdp_value), position = "dodge") +
  scale_x_discrete("Number of template molecules in experiment") +
  cool_theme
dev.off()