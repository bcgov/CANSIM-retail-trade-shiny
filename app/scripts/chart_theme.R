bcstats_chart_theme <-
  theme_bw() +
  theme(
    panel.border = element_rect(colour="white"),
    plot.title = element_text(face="bold"),
    legend.position=c(1,0),
    legend.justification=c(1,0),
    legend.title = element_text(size=12),
    legend.text = element_text(size=11),
    axis.line = element_line(colour="black"),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10)
  )
