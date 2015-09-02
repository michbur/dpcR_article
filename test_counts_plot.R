library(ggplot2)
library(reshape2)

size_mod <- -5
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


pdf(file = "ratiovsGLM.pdf", pointsize = 1)
ggplot(data=madpcr_comp, aes(x = value, fill = method)) +
  geom_density(alpha = 0.3) + 
  scale_fill_discrete("Multiple comparision f
ramework:") + 
  scale_y_continuous("Density") + 
  scale_x_continuous("Fraction of wrongly assigned experiments") + 
  cool_theme
dev.off()

pdf(file = "coverage.pdf", pointsize = 1)
ggplot(m_coverage2, aes(x = prop, y = value, fill = method)) +
  geom_bar(stat="identity", position = "dodge") +
  scale_y_continuous("Probability coverage") + 
  scale_x_discrete(expression(lambda)) +
  scale_fill_discrete("Confidence intervals:") + 
  geom_hline(y = 0.95, colour = "black", size = 2, linetype = 5) +
  facet_wrap(~ coverage, nrow = 2) + 
  cool_theme
dev.off()