#' Title
#'
#' @param sidekick A dataset formatted to be plotted with the superskeleton function, the sidekick information
#' @param plot.title The title of your graphical output
#' @param x.legend The legend on the x-axis
#' @param y.legend The legend on the y-axis
#' @param fig.color A vector of colours, one colour per figure
#' @param size The size of the points in the geom_point function
#' @param shape The shape of the points in the geom_point function
#' @param span The amount of smoothing in the geom_smooth function
#' @param ratio The ration x-axis and y-axis
#'
#' @return The static representation of the sidekick data
#' @export
#'
#' @examples
#'
#' data(gaetan_apchagi)
#' data(human)
#'
#' S1_right_ankle <- sidekick(joint=gaetan_apchagi, num.joint=2, num.name=8,
#' num.x=6, num.y=4, joint1="RIGHT_ANKLE", joint2=NULL)
#' flash(sidekick = S1_right_ankle, plot.title="Right ankle trajectory",
#' x.legend="Frame", y.legend="Right ankle - y (cm)", fig.color="#3333FF")
#'
#' S2_right_ankle_knee <- sidekick(joint=gaetan_apchagi, num.joint=2, num.name=8,
#' num.x=4, num.y=4, joint1="RIGHT_ANKLE", joint2="RIGHT_KNEE")
#' flash(sidekick=S2_right_ankle_knee, plot.title="Right ankle - right knee trajectory",
#' x.legend="Right ankle - y (cm)", y.legend="Right knee - y (cm)", fig.color="#3333FF")

flash <- function(sidekick, plot.title, x.legend, y.legend, fig.color, size=1, shape=23, span=0.15, ratio=1){

  x <- NULL
  y <- NULL
  name <- NULL
  supinf <- filter(sidekick,frame=="0")

  g <- ggplot(data = sidekick, aes(x = x, y = y, colour = name, group = name)) +
    ggtitle(plot.title) +
    theme(plot.title = element_text(hjust = 0.5, size=12, face="bold")) +
    labs(x = x.legend, y = y.legend) +
    scale_color_manual(name = "Figure",
                       #labels = unique(sort(unique(sidekick$name))),
                       values = fig.color)

  if (unique(sidekick$object_type) == "one_singleton" | unique(sidekick$object_type) == "one_couple" | unique(sidekick$object_type) == "one_triplet"){
    g <- g +
      geom_point(size=size,shape=shape, color="black") +
      geom_smooth(formula = y ~ x, method = "loess", span = span, method.args = list(degree=1))

  }
  else{
    g <- g +
      geom_path() +
      geom_point(data=supinf,aes(colour=name), shape=shape)
  }

  g + coord_fixed(ratio = ratio)

}
