ggarrange(fp.plot.delay + labs(col = "sharpshooting strategy", fill = "sharpshooting strategy"),
          fp.plot.sus, 
          common.legend = TRUE, legend = "bottom", nrow = 2, 
          labels = c("a", "b"))

ggarrange(sa.plot.delay + labs(col = "sharpshooting strategy", fill = "sharpshooting strategy"),
          sa.plot.sus, 
          common.legend = TRUE, legend = "bottom", nrow = 2, 
          labels = c("a", "b"))

ggarrange(mainplot.delay + 
            labs(col = "number of years before sharpshooting was implemented") +
            guides(colour = guide_legend(title.position="top", title.hjust = 0.5)),
                   mainplot.sus + 
            labs(col = "number of years before sharpshooting was suspended") +
            guides(colour = guide_legend(title.position="top", title.hjust = 0.5)),
          labels = c("a", "b")
          )
