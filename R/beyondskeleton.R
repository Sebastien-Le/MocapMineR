#' @title Graphical representation of the structure with an additional point, combined with trajectories
#'
#' @description Same idea as the superskeleton function, but here you will represent an additional point and its trajectory. The idea of this function is to study the movement with respect to an additional point. In other words, you will be able to see when one particular part of the structure meets that additional point during the movement.  
#'  
#' @param joint The joint dataset: the coordinates of the joints as a function of time
#' @param structure The structure dataset: a first column with the segments composing the structure, two other columns defining the extremities of the segments
#' @param beyond The data generated by the beyondkick function
#' @param num.joint The index of the column associated with the joint variable
#' @param num.frame The index of the column associated with the frame variable
#' @param num.x The index of the column associated with the x-axis variable represented on the graphical output
#' @param num.y The index of the column associated with the y-axis variable represented on the graphical output
#' @param y.axis.beyond The dimension of the beyond data you want to represent on the y-axis
#' @param frame.index The index of the frame you want to represent (static representation)
#' @param body.part The name of the joint you want to represent (one joint, different from superskeleton)
#' @param color.part The colour you want to use to represent the joint (different from superskeleton)
#' @param plot.title The title of the graphical output
#' @param x.legend The legend on the x-axis
#' @param y1.legend The legend on the y-axis (left)
#' @param y2.legend The legend on the y-axis (right)
#' @param x.dilatation The dilatation coefficient on the x-axis
#' @param x.translation The translation coefficient on the x-axis
#' @param y.dilatation The dilatation coefficient on the y-axis
#' @param y.translation The translation coefficient on the y-axis
#' @param size.sup The size of the beyond element
#' @param shape.sup The shape of the beyond element
#' @param color.beyond The colour of the beyond element
#' @param fps The number of frames per second
#'
#' @return An animation by default or a static representation for a given frame
#' @export
#'
#' @examples
#' \dontrun{
#' data(gaetan_apchagi)
#' plexus <- beyondkick(joint=gaetan_apchagi, num.joint=2, num.x = 3, num.y =4,
#' num.frame=6, origin="RIGHT_SHOULDER", seg1 = c("RIGHT_SHOULDER", "LEFT_SHOULDER", 1/2),
#' seg2=c("RIGHT_SHOULDER", "RIGHT_HIP", 1/2))
#'
#' g <- beyondskeleton(joint=gaetan_apchagi, structure=human, beyond=plexus,
#' num.joint=2, num.frame=6, num.x=3, num.y=4,y.axis.beyond="y",
#' frame.index=25, body.part="RIGHT_KNEE", color.part="orange",
#' plot.title="Gaetan: trajectories of the right knee and the plexus
#' \n according to the y-axis", x.legend="Frame",
#' y1.legend="Height in cm",y2.legend="Trajectories according to the y-axis",
#' x.dilatation=1, x.translation=170, y.dilatation=1, y.translation=0, fps=30)
#'
#' g <- g + geom_hline(yintercept = 0,linetype="dashed") +
#' geom_vline(xintercept = 0,linetype="dashed") +
#' geom_vline(xintercept = 200,linetype="dotted") +
#' geom_segment(aes(x = 200, y = -2, xend = 200, yend = 2)) +
#' geom_text() +
#' annotate("text", label = "0",x = 195, y = -8, size = 3, colour = "red") +
#' annotate("text", label = "0",x = -5, y = -8, size = 3, colour = "red") +
#' annotate("text", label = "Length in cm",x = 75, y = -8, size = 3, colour = "red") +
#' annotate("text", label = "Frame",x = 250, y = -8, size = 3, colour = "red") +
#' geom_segment(aes(x = 225, y = 0, xend = 225, yend = 180),linetype="dotdash") +
#' geom_text() + annotate("text", label = "Frame 25", x = 225, y = 185, size = 3, colour = "red")
#' g
#' }
beyondskeleton <- function(joint, structure, beyond, num.joint, num.frame, num.x, num.y, y.axis.beyond="y",
                           frame.index=NULL, body.part, color.part, plot.title, x.legend, y1.legend,
                           y2.legend,x.dilatation=1, x.translation=200, y.dilatation=1,
                           y.translation=0, size.sup=4, shape.sup=10, color.beyond="green",fps=30) {

  # 1st data set
  # 69 frames from 0 to 68
  # 18 segments
  # 33 joints
  # 18*2 = 36 but on the foot several times the same joint
  # 36*69 = 2484

  x <- NULL
  y <- NULL
  segment <- NULL
  loc <- NULL

  arti_inter <- select(joint, c(num.joint, num.x, num.y, num.frame))
  names(arti_inter) <- c("loc", "x", "y", "frame")

  extr1 <- structure[,c(1,2)]
  names(extr1)[2] <- "loc"
  extr2 <- structure[,c(1,3)]
  names(extr2)[2] <- "loc"
  struc_inter <- rbind(extr1, extr2)

  squelette <- merge(struc_inter, arti_inter, all.x=TRUE, all.y=TRUE, by="loc")

  squelette <- squelette[order(squelette$frame, squelette$segment), ]
  row.names(squelette) <- 1:nrow(squelette)

  # squelette_point <- rbind(squelette, beyond)


  # 2nd data set

  if (is.null(frame.index)){
    # Animated graphic

    beyond_traj <- beyond
    beyond_traj$x <- unique(squelette$frame)

    if (y.axis.beyond=="x"){
      beyond_traj$y <- beyond$x
    }

    tmp1 <- data.frame()
    res <- data.frame()
    for (i in 1:nrow(beyond_traj)){
      res <- beyond_traj[1:i,]
      tmp1 <- rbind(tmp1, res)
    }

    # Modify the frame column
    tmp1$frame <- c(rep(0:max(tmp1$frame), 1:(max(tmp1$frame)+1)))

    # Dilate the axes to and offset in x to put the man and the trajectory side by side
    tmp1$x <- (tmp1$x + x.translation) * x.dilatation
    tmp1$y <- (tmp1$y + y.translation) * y.dilatation

    # We rename the loc column of tmp1 otherwise during the graph the point located on the skeleton and its trajectory are linked
    tmp1$loc <- rep("add_point", nrow(tmp1))
    tmp1$segment <- rep("add_point", nrow(tmp1))


    # We use the same structure of data.frame but we replace the y column of the additional point
    # with the x or y coordinates of the joint whose relative position to this additional point is being studied
    interest_joint <- beyond_traj

    if (y.axis.beyond=="x"){
      interest_joint$y <- arti_inter$x[arti_inter$loc == body.part]
    }

    else{
      interest_joint$y <- arti_inter$y[arti_inter$loc == body.part]
    }

    tmp2 <- data.frame()
    res <- data.frame()
    for (i in 1:nrow(interest_joint)){
      res <- interest_joint[1:i,]
      tmp2 <- rbind(tmp2, res)
    }

    # Modify the frame column
    tmp2$frame <- c(rep(0:max(tmp2$frame), 1:(max(tmp2$frame)+1)))

    # Dilate the axes to and offset in x to put the man and the trajectory side by side
    tmp2$x <- (tmp2$x + x.translation) * x.dilatation
    tmp2$y <- (tmp2$y + y.translation) * y.dilatation

    # We rename the loc column of tmp2 otherwise during the graph the point located on the skeleton and its trajectory are linked
    tmp2$loc <- rep("interest_joint", nrow(tmp2))
    tmp2$segment <- rep("interest_joint", nrow(tmp2))


    # Concatenate the 4 data sets
    two_data <- rbind(squelette, beyond, tmp1, tmp2)

    # Graphic
    g <- ggplot(two_data, aes(x, y, group = segment))  +
      #geom_point(data = df, aes(x=x, y=y)) +
      #orig geom_point(aes(color=loc %in% body.part, size=loc %in% body.part)) +
      geom_point(aes(color=loc %in% body.part)) +
      geom_line() +

      geom_point(data = subset(two_data, loc == 0), color = color.beyond, size=5, shape=15) +

      #geom_line(data = subset(two_data, loc == "interest_joint"), color = color.part) +
      geom_point(data = subset(two_data, loc == "interest_joint"), color = color.part) +

      #geom_line(data = subset(two_data, loc == "add_point"), color = "color.beyond") +
      geom_point(data = subset(two_data, loc == "add_point"), color = color.beyond) +

      coord_fixed(ratio = 1) +
      scale_colour_manual(values=c(adjustcolor("black", alpha.f = 0.3), color.part)) +
      transition_manual(frame) +
      labs(title = plot.title,
           subtitle="Frame = {frame}",
           x = x.legend) +
      theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"),
            plot.subtitle=element_text(hjust=0.5),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_text(hjust=1),
            legend.position = "none") +
      scale_y_continuous(
        y1.legend,
        sec.axis = sec_axis(~ . / y.dilatation - y.translation, name = y2.legend)
      )

    animate(g, fps = fps)

  }

  else{
    # Static graphic

    df <- subset(rbind(squelette, beyond), frame == frame.index)
    # dim(df)
    # in df the data from squelette and a supplementary point of interest

    # in the following part we plot the frame
    g <- ggplot() +
      ggtitle(plot.title) +
      theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_text(hjust=1),
            legend.position = "none") +


      geom_point(data = df, aes(x=x, y=y)) +
      #orig geom_point(data = df, aes(x=x, y=y, group = segment, color=loc %in% body.part, size=loc %in% body.part)) +
      #try geom_point(data = df, aes(x=x, y=y, group = segment, color=loc %in% body.part)) +
      #try geom_point(data = df, aes(x=x, y=y, group = segment, color=loc == body.part)) +
      geom_line(data = df, aes(x=x, y=y, group = segment)) +

      geom_point(data = subset(df, loc == 0), aes(x=x, y=y), color = color.beyond, size=size.sup, shape=shape.sup) +
      geom_point(data = subset(df, loc == body.part), aes(x=x, y=y), color = color.part, size=2, shape=16) +

      coord_fixed(ratio = 1) +
      #orig scale_colour_manual(values=c("black", color.part))  +
      labs(x = x.legend) +
      scale_y_continuous(
        y1.legend,
        sec.axis = sec_axis(~ . / y.dilatation - y.translation, name = y2.legend)
      )

    # Trajectoires

    if (y.axis.beyond=="x"){

      g <- g +
        #orig geom_path(data = subset(arti_inter, loc == body.part), aes(x=(frame + x.translation) * x.dilatation, y=(x + y.translation) * y.dilatation)) +
        geom_smooth(data = subset(arti_inter, loc == body.part), aes(x=(frame + x.translation) * x.dilatation, y=(x + y.translation) * y.dilatation),
                    formula = y ~ x, method = "loess", span = 0.15, method.args = list(degree=1), colour=color.part) +

        #orig geom_path(data = beyond, aes(x=(frame + x.translation) * x.dilatation, y=(x + y.translation) * y.dilatation)) +
        geom_smooth(data = beyond, aes(x=(frame + x.translation) * x.dilatation, y=(x + y.translation) * y.dilatation),
                    formula = y ~ x, method = "loess", span = 0.15, method.args = list(degree=1), colour=color.beyond)

    }

    else{

      g <- g +
        #orig geom_path(data = subset(arti_inter, loc == body.part), aes(x=(frame + x.translation) * x.dilatation, y=(y + y.translation) * y.dilatation)) +
        geom_smooth(data = subset(arti_inter, loc == body.part), aes(x=(frame + x.translation) * x.dilatation, y=(y + y.translation) * y.dilatation),
                    formula = y ~ x, method = "loess", span = 0.15, method.args = list(degree=1), colour=color.part) +

        #orig geom_path(data = beyond, aes(x=(frame + x.translation) * x.dilatation, y=(y + y.translation) * y.dilatation)) +
        geom_smooth(data = beyond, aes(x=(frame + x.translation) * x.dilatation, y=(y + y.translation) * y.dilatation),
                    formula = y ~ x, method = "loess", span = 0.15, method.args = list(degree=1), colour=color.beyond)


    }

    g

  }
}
