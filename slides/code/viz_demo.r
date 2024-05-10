library(ggplot2)

# Load the Anscombe's Quartet dataset
data(anscombe)

# Create a plot for each dataset in the quartet
plots <- lapply(1:4, function(i) {
    ggplot(anscombe, aes_string(x = paste0("x", i), y = paste0("y", i))) +
        geom_point(size = 4, color = "dodgerblue") +
        geom_smooth(method = "lm", se = FALSE, color = "darkorange") +
        xlim(3, 20) +
        ylim(3, 13) +
        labs(title = paste("Dataset", i))
})

# Arrange the plots in a grid
gridExtra::grid.arrange(grobs = plots, ncol = 2)


if (!require(remotes)) {
    install.packages("remotes")
}
remotes::install_github("jorvlan/raincloudplots")

library(raincloudplots)

df_1x1 <- data_1x1(
  array_1 = iris$Sepal.Length[1:50],
  array_2 = iris$Sepal.Length[51:100],
  jit_distance = .09,
  jit_seed = 321)

raincloud_1_v <- raincloud_1x1(
  data = df_1x1,
  colors = (c("dodgerblue", "darkorange")),
  fills = (c("dodgerblue", "darkorange")),
  size = 4,
  alpha = .6,
  ort = "v") +
    scale_x_continuous(
            breaks=c(1, 2),
            labels=c("Group1", "Group2"),
            limits=c(0, 3)) +
        xlab("Groups") +
        ylab("Score") +
  theme_classic()

raincloud_1_v


raincloud_2 <- raincloud_1x1_repmes(
  data = df_1x1,
  colors = (c("dodgerblue", "darkorange")),
  fills = (c("dodgerblue", "darkorange")),
  line_color = "gray",
  line_alpha = .3,
  size = 4,
  alpha = .6,
  align_clouds = FALSE) +

scale_x_continuous(breaks=c(1,2), labels=c("Pre", "Post"), limits=c(0, 3)) +
  xlab("Time") +
  ylab("Score") +
  theme_classic()

raincloud_2


if (!require(plotly)) {
    install.packages("plotly")
}

library(plotly)
library(stats)
data(iris)

X <- subset(iris, select = -c(Species))
axis = list(showline=FALSE,
            zeroline=FALSE,
            gridcolor="#ffff",
            ticklen=4)
fig <- iris %>%
  plot_ly()  %>%
  add_trace(
    type = "splom",
    dimensions = list(
      list(label = "sepal_width",values=~Sepal.Width),
      list(label = "sepal_length",values=~Sepal.Length),
      list(label ="petal_width",values=~Petal.Width),
      list(label = "petal_length",values=~Petal.Length)),
    color = ~Species, colors = c("#636EFA","#EF553B","#00CC96")
  )
fig <- fig %>%
  layout(
    legend = list(title = list(text = "species")),
    hovermode = "closest",
    dragmode = "select",
    plot_bgcolor = "rgba(240, 240, 240, 0.95)",
    xaxis = list(domain = NULL, showline = F,
    zeroline = FALSE, gridcolor = "#ffff", ticklen = 4),
    yaxis = list(domain = NULL, showline = F,
    zeroline = FALSE, gridcolor = "#ffff", ticklen = 4),
    xaxis2 = axis,
    xaxis3 = axis,
    xaxis4 = axis,
    yaxis2 = axis,
    yaxis3 = axis,
    yaxis4 = axis
  )
fig

pca <- prcomp(X)
pca <- data.frame(PC1=pca$x[, 1],
                  PC2=pca$x[, 2],
                  Species=iris$Species)

fig <-  plot_ly(data = pca , x = ~PC1 , y = ~PC2 , type = "scatter",
                mode = "markers", split = ~iris$Species)

fig <- fig %>%
  layout(
    plot_bgcolor = "#e5ecf6"
  )
fig

if (!require(tsne)) {
    install.packages("tsne")
}

library(tsne)
features <- subset(iris, select = -c(Species))

set.seed(0)
tsne <- tsne(features, initial_dims = 2)
tsne <- data.frame(tsne)
pdb <- cbind(tsne, iris$Species)

options(warn = -1)
fig <-  plot_ly(data = pdb, x =  ~X1, y = ~X2,
                type = "scatter", mode = "markers", split = ~iris$Species)

fig <- fig %>%
  layout(
    plot_bgcolor = "#e5ecf6"
  )

fig