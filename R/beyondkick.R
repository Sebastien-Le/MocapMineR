#' @title Calculate the coordinates of an additional point defined by segments of the structure
#' 
#' @description Imagine you want to represent some kind of additional point, on the structure, within the structure, outside the structure. As you don't have the data for this representation, this function calculates the coordinates of such additional point based on a linear combination of two segments of the structure.
#'
#' @param joint The joint dataset: the coordinates of the joints as a function of time
#' @param num.joint The index of the column associated with the joint variable
#' @param num.frame The index of the column associated with the frame variable
#' @param num.x The index of the column associated with the x-axis variable represented on the graphical output
#' @param num.y The index of the column associated with the y-axis variable represented on the graphical output
#' @param origin The origin of the basis, a joint
#' @param seg1 A triplet, the definition of the segment in terms of joints, and the coordinate on this segment
#' @param seg2 A triplet, the definition of the segment in terms of joints, and the coordinate on this segment
#'
#' @return A supplementary dataset to be used with the beyondskeleton function
#' @export
#'
#' @examples
#' \dontrun{
#' data(gaetan_apchagi)
#' plexus <- beyondkick(joint=gaetan_apchagi, num.joint=2, num.x = 3, num.y =4,
#' num.frame=6, origin="RIGHT_SHOULDER", seg1 = c("RIGHT_SHOULDER", "LEFT_SHOULDER", 1/2),
#' seg2=c("RIGHT_SHOULDER", "RIGHT_HIP", 1/2))
#' }

beyondkick <- function(joint, num.joint, num.frame=NULL,
                       num.x, num.y, origin, seg1, seg2=NULL) {

  vars <- c(num.joint, num.x, num.y, num.frame)
  df <- select(joint, all_of(vars))
  names(df) <- c("loc", "x", "y", "frame")

  df.point <- data.frame(loc = rep(0, max(df$frame)+1),
                         segment = rep(0, max(df$frame)+1),
                         frame = unique(df$frame))

  poids_J1_seg1 <- as.numeric(seg1[3])

  if (is.null(seg2)){
    df.point$x <- poids_J1_seg1*(df$x[df$loc == seg1[2]]-df$x[df$loc == seg1[1]]) + df$x[df$loc == origin]
    df.point$y <- poids_J1_seg1*(df$y[df$loc == seg1[2]]-df$y[df$loc == seg1[1]]) + df$y[df$loc == origin]

  }

  else{
    poids_J1_seg2 <- as.numeric(seg2[3])
    df.point$x <- poids_J1_seg1*(df$x[df$loc == seg1[2]]-df$x[df$loc == seg1[1]]) + poids_J1_seg2*(df$x[df$loc == seg2[2]]-df$x[df$loc == seg2[1]]) + df$x[df$loc == origin]
    df.point$y <- poids_J1_seg1*(df$y[df$loc == seg1[2]]-df$y[df$loc == seg1[1]]) + poids_J1_seg2*(df$y[df$loc == seg2[2]]-df$y[df$loc == seg2[1]]) + df$y[df$loc == origin]
  }

  return(df.point)

}
