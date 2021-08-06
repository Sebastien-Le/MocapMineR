library(ggplot2)
library(gganimate)

data(gaetan_apchagi)
data(human)

# A regler
# Note: Using an external vector in selections is ambiguous.
# ℹ Use `all_of(num.name)` instead of `num.name` to silence this message.
# ℹ See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.


data(all_apchagi)
joint(joint=all_apchagi, num.joint=2, num.frame=6, num.y=4, num.name=8, y.legend="y",
fig.color=c("#3333FF", "#FF0099", "#00FFCC"),
vect.joint = c("RIGHT_KNEE", "RIGHT_ANKLE", "RIGHT_FOOT_INDEX"))

skeleton(joint=gaetan_apchagi, structure= human, num.joint=2,
num.frame=6, num.x=3, num.y=4, num.name=8)

skeleton(joint=gaetan_apchagi, structure= human, num.joint=2,
num.frame=6, num.x=3, num.y=4, num.name=8, body.part = "tibia_r",
frame.index=25, color.part = "orange")

superskeleton(joint=gaetan_apchagi, structure=human, sidekick=S1_right_ankle,
              num.joint=2, num.frame=6, num.x=3, num.y=4,
              frame.index=NULL, body.part="RIGHT_ANKLE", color.part="orange",
              plot.title="Gaetan - right ankle trajectory", x.legend="Frame",
              y.legend="Trajectory in y (cm)")


superskeleton(joint=gaetan_apchagi, structure=human, sidekick=S2_right_ankle_knee,
              num.joint=2, num.frame=6, num.x=3, num.y=4,
              frame.index=NULL, body.part=c("RIGHT_ANKLE","RIGHT_KNEE"),
              color.part="orange",
              plot.title="Gaetan - right ankle trajectory", x.legend="Ankle",
              y.legend="Knee")


b <- superskeleton(joint=gaetan_apchagi, structure=human, sidekick=S2_right_ankle_knee,
              num.joint=2, num.frame=6, num.x=3, num.y=4,
              frame.index=25, body.part=c("RIGHT_ANKLE","RIGHT_KNEE"),
              color.part="orange",
              plot.title="Gaetan - right ankle vs. knee trajectory", x.legend="Ankle - y (cm)",
              y.legend="Knee - y (cm)")
b



data(gaetan_apchagi)

S1_right_ankle <- sidekick(joint=gaetan_apchagi, num.joint=2, num.name=8,
num.x=6, num.y=4, joint1="RIGHT_ANKLE", joint2=NULL)

S1_right_knee <- sidekick(joint=gaetan_apchagi, num.joint=2, num.name=8,
                           num.x=6, num.y=4, joint1="RIGHT_KNEE", joint2=NULL)

C1_right_knee_ankle <- sidekick(joint=gaetan_apchagi, num.joint=2, num.name=8,
num.x = 6, num.y = 4, joint1=c("RIGHT_KNEE", "RIGHT_ANKLE"),
              joint2=NULL)

T1_right_knee_ankle <- sidekick(joint=gaetan_apchagi, num.joint=2, num.name=8,
                                num.x = 6, num.y = c(3,4), joint1=c("RIGHT_HIP","RIGHT_KNEE", "RIGHT_ANKLE"),
                                joint2=NULL)

S2_right_ankle_knee <- sidekick(joint=gaetan_apchagi, num.joint=2, num.name=8,
num.x=4, num.y=4, joint1="RIGHT_ANKLE", joint2="RIGHT_KNEE")

S1_right_ankle <- sidekick(joint=gaetan_apchagi, num.joint=2, num.name=8,
                           num.x=6, num.y=4, joint1="RIGHT_ANKLE", joint2=NULL)

flash(sidekick = S1_right_ankle, plot.title="Right ankle trajectory",
      x.legend="Frame", y.legend="Right ankle - y (cm)", fig.color="#3333FF")

data(gaetan_apchagi)
data(human)


S2_right_ankle_knee <- sidekick(joint=gaetan_apchagi, num.joint=2, num.name=8,
num.x=4, num.y=4, joint1="RIGHT_ANKLE", joint2="RIGHT_KNEE")

flash(sidekick = S2_right_ankle_knee, plot.title="Right ankle trajectory",
x.legend="Right ankle - y (cm)", y.legend="Right knee - y (cm)", fig.color="#3333FF")



data(gaetan_apchagi)
plexus <- beyondkick(joint=gaetan_apchagi, num.joint=2, num.x = 3, num.y =4,
num.frame=6, origin="RIGHT_SHOULDER", seg1 = c("RIGHT_SHOULDER", "LEFT_SHOULDER", 1/2),
seg2=c("RIGHT_SHOULDER", "RIGHT_HIP", 1/2))



g <- beyondskeleton(joint=gaetan_apchagi, structure=human, beyond=plexus,
                    num.joint=2, num.frame=6, num.x=3, num.y=4,y.axis.beyond="y",
                    frame.index=25, body.part="RIGHT_KNEE", color.part="orange",
                    plot.title="Gaetan: trajectories of the right knee and the plexus  \n according to the y-axis", x.legend="Frame",
                    y1.legend="Height in cm",y2.legend="Trajectories according to the y-axis",
                    x.dilatation=1, x.translation=200, y.dilatation=1, y.translation=0, fps=30)

g <- g + geom_hline(yintercept = 0,linetype="dashed") +
     geom_vline(xintercept = 0,linetype="dashed") +
     geom_vline(xintercept = 200,linetype="dotted") +
     geom_segment(aes(x = 200, y = -2, xend = 200, yend = 2)) +

     geom_text() +
     annotate("text", label = "0",x = 195, y = -8, size = 3, colour = "red") +
     annotate("text", label = "0",x = -5, y = -8, size = 3, colour = "red") +
     annotate("text", label = "Length in cm",x = 75, y = -8, size = 3, colour = "red") +
     annotate("text", label = "Frame",x = 250, y = -8, size = 3, colour = "red") +

     geom_segment(aes(x = 225, y = 0, xend = 225, yend = 180),linetype="dotdash") +
     geom_text() + annotate("text", label = "Frame 25", x = 225, y = 185, size = 3, colour = "red")

g

#g <- g + geom_vline(xintercept = 225,linetype="dotdash")

g <- g + geom_segment(aes(x = 225, y = 0, xend = 225, yend = 180),linetype="dotdash")
g

g <- g +
  geom_text() +
  annotate(
    "text", label = "0",
    x = -5, y = -8, size = 3, colour = "red"
  )

g <- g +
  geom_text() +
  annotate(
    "text", label = "Frame 25",
    x = 225, y = 185, size = 3, colour = "red"
  )

g <- g +
  geom_text() +
  annotate(
    "text", label = "Length in cm",
    x = 75, y = -8, size = 3, colour = "red"
  )

g <- g +
  geom_text() +
  annotate(
    "text", label = "Frame",
    x = 250, y = -8, size = 3, colour = "red"
  )
g

?geom_text
joint=gaetan_apchagi
structure=human
beyond=up_head

num.joint=2, num.frame=6, num.x=3, num.y=4,y.axis.beyond="x",
frame.index=25, displayed_part="RIGHT_KNEE", color.part="orange",
plot.title="Skeleton of Gaetan + Trajectory of the right ankle - y", x.legend="Frame",
y1.legend="Trajectory in y_C (cm)",y2.legend="2",
x.dilatation=1, x.translation=170, y.dilatation=1, y.translation=0, fps=30



beyondskeleton <- function(joint, structure, beyond, num.joint, num.frame, num.x, num.y, y.axis.beyond="y",
                           frame.index=NULL, displayed_part, color.part, plot.title, x.legend, y1.legend,
                           y2.legend,x.dilatation=1, x.translation=200, y.dilatation=1,
                           y.translation=0, size_sup=4, shape_sup=10, fps=30) {

  # 1st data set
  # 69 frames from 0 to 68
  # 18 segments
  # 33 joints
  # 18*2 = 36 but on the foot several times the same joint
  # 36*69 = 2484

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
      interest_joint$y <- arti_inter$x[arti_inter$loc == displayed_part]
    }

    else{
      interest_joint$y <- arti_inter$y[arti_inter$loc == displayed_part]
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
      #orig geom_point(aes(color=loc %in% displayed_part, size=loc %in% displayed_part)) +
      geom_point(aes(color=loc %in% displayed_part)) +
      geom_line() +

      geom_point(data = subset(two_data, loc == 0), color = "green", size=5, shape=15) +

      #geom_line(data = subset(two_data, loc == "interest_joint"), color = color.part) +
      geom_point(data = subset(two_data, loc == "interest_joint"), color = color.part) +

      #geom_line(data = subset(two_data, loc == "add_point"), color = "green") +
      geom_point(data = subset(two_data, loc == "add_point"), color = "green") +

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
        y2.legend,
        sec.axis = sec_axis(~ . / y.dilatation - y.translation, name = y1.legend)
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
      #orig geom_point(data = df, aes(x=x, y=y, group = segment, color=loc %in% displayed_part, size=loc %in% displayed_part)) +
      #try geom_point(data = df, aes(x=x, y=y, group = segment, color=loc %in% displayed_part)) +
      #try geom_point(data = df, aes(x=x, y=y, group = segment, color=loc == displayed_part)) +
      geom_line(data = df, aes(x=x, y=y, group = segment)) +

      geom_point(data = subset(df, loc == 0), aes(x=x, y=y), color = "green", size=size_sup, shape=shape_sup) +
      geom_point(data = subset(df, loc == displayed_part), aes(x=x, y=y), color = color.part, size=2, shape=16) +

      coord_fixed(ratio = 1) +
      #orig scale_colour_manual(values=c("black", color.part))  +
      labs(x = x.legend) +
      scale_y_continuous(
        y2.legend,
        sec.axis = sec_axis(~ . / y.dilatation - y.translation, name = y1.legend)
      )

    # Trajectoires

    if (y.axis.beyond=="x"){

      g <- g +
        #orig geom_path(data = subset(arti_inter, loc == displayed_part), aes(x=(frame + x.translation) * x.dilatation, y=(x + y.translation) * y.dilatation)) +
        geom_smooth(data = subset(arti_inter, loc == displayed_part), aes(x=(frame + x.translation) * x.dilatation, y=(x + y.translation) * y.dilatation),
                    formula = y ~ x, method = "loess", span = 0.15, method.args = list(degree=1), colour=color.part) +

        #orig geom_path(data = beyond, aes(x=(frame + x.translation) * x.dilatation, y=(x + y.translation) * y.dilatation)) +
        geom_smooth(data = beyond, aes(x=(frame + x.translation) * x.dilatation, y=(x + y.translation) * y.dilatation),
                    formula = y ~ x, method = "loess", span = 0.15, method.args = list(degree=1), colour="green")

    }

    else{

      g <- g +
        #orig geom_path(data = subset(arti_inter, loc == displayed_part), aes(x=(frame + x.translation) * x.dilatation, y=(y + y.translation) * y.dilatation)) +
        geom_smooth(data = subset(arti_inter, loc == displayed_part), aes(x=(frame + x.translation) * x.dilatation, y=(y + y.translation) * y.dilatation),
                    formula = y ~ x, method = "loess", span = 0.15, method.args = list(degree=1), colour=color.part) +

        #orig geom_path(data = beyond, aes(x=(frame + x.translation) * x.dilatation, y=(y + y.translation) * y.dilatation)) +
        geom_smooth(data = beyond, aes(x=(frame + x.translation) * x.dilatation, y=(y + y.translation) * y.dilatation),
                    formula = y ~ x, method = "loess", span = 0.15, method.args = list(degree=1), colour="green")


    }

    g

  }
}
