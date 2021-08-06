#' @title Graphical representation of the joints
#' @description This function allows you to represent the joints as a function of time.
#' The main argument is the dimension you want to represent on the y-axis. You can use this function to compare several figures at the same time, in that case, make sure to specify a proper vector of colours.
#'
#' @param joint The joint dataset: the coordinates of the joints as a function of time
#' @param num.joint The index of the column associated with the joint variable
#' @param num.frame The index of the column associated with the frame variable
#' @param num.y The index of the column associated with the y-axis variable represented on the graphical output
#' @param num.name The index of the column associated with the name variable
#' @param y.legend The legend on the y-axis
#' @param fig.color A vector of colours, one colour per figure
#' @param vect.joint A vector of joints
#' @param size The size of the points in the geom_point function
#' @param shape The shape of the points in the geom_point function
#' @param span The amount of smoothing in the geom_smooth function
#'
#' @return A representation of the joints
#' @export
#'
#' @examples
#' data(all_apchagi)
#' joint(joint=all_apchagi, num.joint=2, num.frame=6, num.y=4, num.name=8, y.legend="y",
#' fig.color=c("#3333FF", "#FF0099", "#00FFCC"),
#' vect.joint = c("RIGHT_KNEE", "RIGHT_ANKLE", "RIGHT_FOOT_INDEX"))
#'
#' data(gaetan_apchagi)
#' joint(joint=gaetan_apchagi, num.joint=2, num.frame=6, num.y=4, num.name=8, y.legend="y",
#' fig.color="#FF0099")
#'
#' @keywords motion capture
#'
joint <- function(joint, num.joint, num.frame, num.y, num.name, y.legend,
                  fig.color, vect.joint=NULL, size=1, shape=23, span=0.15) {

  # https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  # nom <- NULL

  # Selecting and renaming columns
  # https://tidyselect.r-lib.org/reference/faq-external-vector.html
  vars <- c(num.joint, num.y, num.frame, num.name)
  df <- select(joint, all_of(vars))
  colnames(df) <- c("loc", "dim", "frame", "nom")

  # Order of appearance of joints in facet
  if (is.null(vect.joint)){
    df$localisation <-  factor(df$loc)
    levels(df$localisation) <- sort(levels(df$localisation))
  }

  else{
    df$localisation <-  factor(df$loc, levels=vect.joint)
  }

  vect.joint <- levels(df$localisation)

  # Graph of trajectories
  df[df$loc %in% vect.joint,] %>%
    ggplot(aes(x = frame, y = dim, colour = .data$nom, group = .data$nom)) +
    geom_point(size=size,shape=shape, color="black") +
    geom_smooth(formula = y ~ x, method = "loess", span = span, method.args = list(degree=1)) +
    facet_wrap(~localisation) +
    labs(x = "Frame", y = y.legend) +
    scale_color_manual(name = "Figure",
                       #labels = unique(sort(unique(df$name))),
                       values = fig.color)

}
