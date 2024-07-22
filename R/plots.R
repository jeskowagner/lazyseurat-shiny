library(dplyr)
library(ggplot2)

base.plot.theme <- function() {
    require(ggplot2)
    theme_classic() +
        theme(
            plot.title = element_text(size = 25, hjust = 0.5),
            legend.position = "right", legend.title = element_text(size = 20),
            legend.key.size = unit(1.5, "line"),
            legend.text = element_text(size = 20),
            axis.line = element_line(
                colour = "black",
                linewidth = 1.25,
                linetype = "solid"
            ),
            axis.text.x = element_text(size = 20, colour = "black"),
            axis.text.y = element_text(size = 20, colour = "black"),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            axis.ticks.length = unit(0.2, "cm")
        )
}

plot.theme_vln <- function() {
    require(ggplot2)
    base.plot.theme() +
        theme(
            axis.text.x = element_text(
                size = 20,
                colour = "black",
                angle = 45,
                hjust = 1,
                vjust = 1
            ),
            #axis.title.x = element_blank(),
            legend.title = element_blank()
        )
}

VlnPlot <- function(con, gene, x=NULL, split.by=NULL) {
    require(shiny)
    require(ggplot2)
    req(con, gene)

    df <- read_gene_expression(con, gene)

    if (!is.numeric(df[[gene]])) {
        df[[gene]] <- as.numeric(df[[gene]])
    }

    aesthetics <- aes(y=.data[[gene]], x=factor(1))
    if(!is.null(x)) {
        aesthetics <- modifyList(aesthetics, aes(x=.data[[x]]))
    }
    if(!is.null(split.by)){
        aesthetics <- modifyList(aesthetics, aes(fill=.data[[split.by]]))
    }

    p <- ggplot(df, aesthetics) +
        geom_violin(scale = "width") +
        plot.theme_vln() +
        labs(title = gene, y = "Expression Level", x=x)
    return(p)
}
