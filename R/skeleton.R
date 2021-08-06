#' Title
#'
#' @param joint The joint dataset: the coordinates of the joints as a function of time
#' @param structure The structure dataset: a first column with the segments composing the structure, two other columns defining the extremities of the segments
#' @param num.joint The index of the column associated with the joint variable
#' @param num.frame The index of the column associated with the frame variable
#' @param num.x The index of the column associated with the x-axis variable represented on the graphical output
#' @param num.y The index of the column associated with the y-axis variable represented on the graphical output
#' @param num.name The index of the column associated with the name variable
#' @param frame.index The index of the frame you want to represent (static representation)
#' @param body.part The names of the segments you want to represent
#' @param color.part The colour you want to use to represent the segments
#' @param fps The number of frames per second
#'
#' @return An animation by default or a static representation for a given frame
#' @import tidyverse gganimate LearnGeom ggpubr ggplot2 dplyr grDevices
#' @importFrom graphics frame
#' @export
#
#' @examples
#'
#' \dontrun{
#' data(gaetan_apchagi)
#' data(human)
#' skeleton(joint=gaetan_apchagi, structure= human, num.joint=2,
#' num.frame=6, num.x=3, num.y=4, num.name=8)
#'
#' skeleton(joint=gaetan_apchagi, structure= human, num.joint=2,
#' num.frame=6, num.x=3, num.y=4, num.name=8, body.part = "tibia_r",color.part = "orange")
#' }
#'
#' data(gaetan_apchagi)
#' data(human)
#'
#' skeleton(joint=gaetan_apchagi, structure= human, num.joint=2,
#' num.frame=6, num.x=3, num.y=4, num.name=8, body.part = "tibia_r",
#' frame.index=25, color.part = "orange")
#'


skeleton <- function(joint, structure, num.joint, num.frame,
                     num.x, num.y, num.name, frame.index=NULL, body.part="",
                     color.part="black", fps=30) {

  # Build the appropriate data.frame
  arti_inter <- select(joint, c(num.joint, num.x, num.y, num.frame, num.name))
  names(arti_inter) <- c("loc", "x", "y", "frame", "name")

  extr1 <- structure[,c(1,2)]
  names(extr1)[2] <- "loc"
  extr2 <- structure[,c(1,3)]
  names(extr2)[2] <- "loc"
  struc_inter <- rbind(extr1, extr2)

  squelette <- merge(struc_inter, arti_inter, all.x=TRUE, all.y=TRUE, by="loc")
  squelette <- squelette[order(squelette[,5], squelette[,2]),]
  row.names(squelette) <- 1:nrow(squelette)


  if (is.null(frame.index)){
    # Animate skeleton

    bonhomme <- ggplot(squelette, aes(x=squelette[,3], y=squelette[,4], group = squelette[,2], color=squelette[,2] == body.part)) +
      geom_point() + geom_line() +
      coord_fixed(ratio = 1) +
      xlab("") +
      ylab("") +
      #geom_point(aes(x=min(joint$x), y=min(joint$y)), colour="white") +
      #geom_point(aes(x=max(joint$x), y=max(joint$y)), colour="white") +
      theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"),
            legend.position = "none") +
      scale_colour_manual(values=c(adjustcolor("black", alpha.f = 0.3), color.part))  +
      transition_manual(frame) +
      facet_wrap(~name) +
      labs(title="Frame = {frame}")

    animate(bonhomme, fps = fps)
  }

  else{
    # Static skeleton

    tmp <- subset(squelette, frame == frame.index)

    ggplot(tmp, aes(x=tmp[,3], y=tmp[,4], group = tmp[,2], color=tmp[,2] == body.part)) +
      geom_point() + geom_line() +
      coord_fixed(ratio = 1) +
      xlab("") +
      ylab("") +
      #geom_point(aes(x=min(joint$x), y=min(joint$y)), colour="white") +
      #geom_point(aes(x=max(joint$x), y=max(joint$y)), colour="white") +
      theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none") +
      scale_colour_manual(values=c(adjustcolor("black", alpha.f = 0.3), color.part))  +
      facet_wrap(~name) +
      labs(title=paste0("Frame : ", frame.index))
  }

}
