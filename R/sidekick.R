#' @title Create supplementary data to illustrate the representation of the structure
#'
#' @description This function creates a new dataset for the superskeleton function.
#'
#' @param joint The joint dataset: the coordinates of the joints as a function of time
#' @param num.joint The index of the column associated with the joint variable
#' @param num.frame The index of the column associated with the frame variable
#' @param num.name The index of the column associated with the name variable
#' @param num.x The index of the column associated with the x-axis variable represented on the graphical output
#' @param num.y The index of the column associated with the y-axis variable represented on the graphical output
#' @param joint1 A singleton, a couple or a triplet of joints
#' @param joint2 A singleton, a couple or a triplet of joints
#'
#' @return A dataset formatted to be plotted with the superskeleton function
#' @export
#'
#' @examples
#' data(gaetan_apchagi)
#' # To represent the ankle measured on the y-axis as a function of time
#' S1_right_ankle <- sidekick(joint=gaetan_apchagi, num.joint=2, num.name=8,
#' num.x=6, num.y=4, joint1="RIGHT_ANKLE", joint2=NULL)
#'
#' # To represent the difference between the knee and the ankle
#' # measured on the y-axis as a function of time
#' C1_right_knee_ankle <- sidekick(joint=gaetan_apchagi, num.joint=2, num.name=8,
#' num.x=6, num.y=4, joint1=c("RIGHT_KNEE", "RIGHT_ANKLE"),
#' joint2=NULL)
#'
#' # To represent the angle of the knee measured on the plane defined
#' # by the x-axis and the y-axis
#' T1_right_knee_ankle <- sidekick(joint=gaetan_apchagi, num.joint=2, num.name=8,
#' num.x=6, num.y=c(3,4), joint1=c("RIGHT_HIP","RIGHT_KNEE", "RIGHT_ANKLE"),
#' joint2=NULL)
#'
#' # To represent the trajectory of the ankle with respect to the knee
#' # measured both on the y-axis as a function of the time
#' S2_right_ankle_knee <- sidekick(joint=gaetan_apchagi, num.joint=2, num.name=8,
#' num.x=4, num.y=4, joint1="RIGHT_ANKLE", joint2="RIGHT_KNEE")
#'

sidekick <- function(joint, num.joint, num.frame=NULL,
                     num.name, num.x, num.y, joint1, joint2=NULL) {

  # character_names <- unique(joint[, num.name])
  loc <- NULL
  object_type <- NULL
  segment <- NULL
  x <- NULL
  y <- NULL

  if (is.null(joint2)){
    # Trajectory according to the frame number

    if (length(joint1)==1){

      vars <- c(num.joint,num.x,num.y,num.name)
      df <- select(joint, all_of(vars))
      names(df) <- c("loc", "x", "y", "name")
      df.traj <- subset(df, loc == joint1)
      df.traj$object_type <- rep("one_singleton", nrow(df.traj))

    }

    else if (length(joint1)==2){

      # We keep only the 2 joints we are interested in and we calculate the difference

      # Selection and renaming of columns

      vars <- c(num.joint, num.x, num.y, num.name)
      df <- select(joint, all_of(vars))
      names(df) <- c("loc", "num.x", "num.y", "name")

      df1 <- df[df$loc == joint1[1],]
      df2 <- df[df$loc == joint1[2],]
      names(df1)[c(1,3)] <- c("arti1", "num.y.1")
      names(df2)[c(1,3)] <- c("arti2", "num.y.2")
      df <- cbind(df1, df2)
      df$diff <- df$num.y.1 - df$num.y.2
      df.traj <- df[,c(6,8,9)]
      names(df.traj)[c(1,3)] <- c("x", "y")
      df.traj$object_type <- rep("one_couple", nrow(df.traj))

    }

    else{

      vars <- c(num.joint, num.x, num.y, num.name)
      df <- select(joint, all_of(vars))
      names(df) <- c("loc", "frame", "num.y.1", "num.y.2", "name")

      all_angles <- c()

      angles <- rep(0, max(df$frame))

      for (nom in unique(df$name)){

        tmp <- subset(df, name == nom)

        for (i in 0:max(tmp$frame)){

          A <- c(unique(tmp$num.y.1[tmp$loc == joint1[1] & tmp$frame == i]),
                 unique(tmp$num.y.2[tmp$loc == joint1[1] & tmp$frame == i]))

          B <- c(unique(tmp$num.y.1[tmp$loc == joint1[2] & tmp$frame == i]),
                 unique(tmp$num.y.2[tmp$loc == joint1[2] & tmp$frame == i]))

          C <- c(unique(tmp$num.y.1[tmp$loc == joint1[3] & tmp$frame == i]),
                 unique(tmp$num.y.2[tmp$loc == joint1[3] & tmp$frame == i]))

          angles[i+1] <- Angle(A, B, C)

        }

        all_angles <- c(all_angles, angles)


      }

      df.traj <- data.frame(y = all_angles,
                            name = rep(unique(df$name), each = max(df$frame)+1))
      df.traj$x <- rep(0:((nrow(df.traj)/nrow(unique(joint[, num.name])))-1),
                       nrow(unique(joint[, num.name])))
      df.traj$object_type <- rep("one_triplet", nrow(df.traj))

    }
  }

  else{

    if (length(joint1)==1){

      if (num.x == num.y){

        vars <- c(num.joint, num.x, num.name)
        df <- select(joint, all_of(vars))
        names(df) <- c("loc", "axis", "name")

        res1 <- subset(df, loc == joint1)
        res2 <- subset(df, loc == joint2)
        res1 <- res1[, c(2,3)]
        res2 <- res2[, c(2,3)]

      }

      else{

        vars <- c(num.joint, num.x, num.y, num.name)
        df <- select(joint, all_of(vars))
        names(df) <- c("loc", "num.x", "num.y", "name")

        res1 <- subset(df, loc == joint1)
        res2 <- subset(df, loc == joint2)
        res1 <- res1[, c(3,4)]
        res2 <- res2[, c(2,4)]

      }

      names(res1)[1] <- "x"
      names(res2)[1] <- "y"
      df.traj <- cbind(res1, res2)
      df.traj <- df.traj[, 1:3]
      df.traj$object_type <- rep("two_singletons", nrow(df.traj))

    }

    else if (length(joint1)==2){

      if (num.x == num.y){

        vars <- c(num.joint, num.x, num.name)
        df <- select(joint, all_of(vars))
        names(df) <- c("loc", "axis", "name")

        df.num.y.1 <- subset(df, loc == joint1[1])
        df.num.y.2 <- subset(df, loc == joint1[2])
        df.num.x.1 <- subset(df, loc == joint2[1])
        df.num.x.2 <- subset(df, loc == joint2[2])
        diff_num.y <- df.num.y.1$axis - df.num.y.2$axis
        diff_num.x <- df.num.x.1$axis - df.num.x.2$axis

      }

      else{

        vars <- c(num.joint, num.x, num.y, num.name)
        df <- select(joint, all_of(vars))
        names(df) <- c("loc", "num.x", "num.y", "name")

        df.num.y.1 <- subset(df, loc == joint1[1])
        df.num.y.2 <- subset(df, loc == joint1[2])
        df.num.x.1 <- subset(df, loc == joint2[1])
        df.num.x.2 <- subset(df, loc == joint2[2])
        diff_num.y <- df.num.y.1$num.y - df.num.y.2$num.y
        diff_num.x <- df.num.x.1$num.x - df.num.x.2$num.x

      }

      df.traj <- data.frame(x=diff_num.x, y=diff_num.y)
      df.traj$name <- rep(unique(df$name), each = nrow(df.traj)/length(unique(df$name)))
      df.traj$object_type <- rep("two_couples", nrow(df.traj))

    }

    else{

      all_angles_num.x <- c()
      all_angles_num.y <- c()

      if (identical(num.x, num.y)){

        vars <- c(num.joint, num.frame, num.x, num.name)
        df <- select(joint, all_of(vars))
        names(df) <- c("loc", "frame", "dim1", "dim2", "name")

        angles_num.x <- rep(0, max(df$frame))
        angles_num.y <- rep(0, max(df$frame))

        for (nom in unique(df$name)){

          tmp <- subset(df, name == nom)

          for (i in 0:max(tmp$frame)){

            A_num.x <- c(unique(tmp$dim1[tmp$loc == joint2[1] & tmp$frame == i]),
                          unique(tmp$dim2[tmp$loc == joint2[1] & tmp$frame == i]))

            B_num.x <- c(unique(tmp$dim1[tmp$loc == joint2[2] & tmp$frame == i]),
                          unique(tmp$dim2[tmp$loc == joint2[2] & tmp$frame == i]))

            C_num.x <- c(unique(tmp$dim1[tmp$loc == joint2[3] & tmp$frame == i]),
                          unique(tmp$dim2[tmp$loc == joint2[3] & tmp$frame == i]))


            A_num.y <- c(unique(tmp$dim1[tmp$loc == joint1[1] & tmp$frame == i]),
                          unique(tmp$dim2[tmp$loc == joint1[1] & tmp$frame == i]))

            B_num.y <- c(unique(tmp$dim1[tmp$loc == joint1[2] & tmp$frame == i]),
                          unique(tmp$dim2[tmp$loc == joint1[2] & tmp$frame == i]))

            C_num.y <- c(unique(tmp$dim1[tmp$loc == joint1[3] & tmp$frame == i]),
                          unique(tmp$dim2[tmp$loc == joint1[3] & tmp$frame == i]))


            angles_num.x[i+1] <- Angle(A_num.x, B_num.x, C_num.x)
            angles_num.y[i+1] <- Angle(A_num.y, B_num.y, C_num.y)

          }

          all_angles_num.x <- c(all_angles_num.x, angles_num.x)
          all_angles_num.y <- c(all_angles_num.y, angles_num.y)

        }

        name <- rep(unique(df$name), each = max(df$frame)+1)

      }


      else{

        vars <- c(num.joint, num.frame, num.x, num.y, num.name)
        df <- select(joint, all_of(vars))
        names(df) <- c("loc", "frame", "num.x.dim1", "num.x.dim2", "num.y.dim1", "num.y.dim2", "name")

        angles_num.x <- rep(0, max(df$frame))
        angles_num.y <- rep(0, max(df$frame))

        for (nom in unique(df$name)){

          tmp <- subset(df, name == nom)

          for (i in 0:max(tmp$frame)){

            A_num.x <- c(unique(tmp$num.x.dim1[tmp$loc == joint2[1] & tmp$frame == i]),
                          unique(tmp$num.x.dim2[tmp$loc == joint2[1] & tmp$frame == i]))

            B_num.x <- c(unique(tmp$num.x.dim1[tmp$loc == joint2[2] & tmp$frame == i]),
                          unique(tmp$num.x.dim2[tmp$loc == joint2[2] & tmp$frame == i]))

            C_num.x <- c(unique(tmp$num.x.dim1[tmp$loc == joint2[3] & tmp$frame == i]),
                          unique(tmp$num.x.dim2[tmp$loc == joint2[3] & tmp$frame == i]))


            A_num.y <- c(unique(tmp$num.y.dim1[tmp$loc == joint1[1] & tmp$frame == i]),
                          unique(tmp$num.y.dim2[tmp$loc == joint1[1] & tmp$frame == i]))

            B_num.y <- c(unique(tmp$num.y.dim1[tmp$loc == joint1[2] & tmp$frame == i]),
                          unique(tmp$num.y.dim2[tmp$loc == joint1[2] & tmp$frame == i]))

            C_num.y <- c(unique(tmp$num.y.dim1[tmp$loc == joint1[3] & tmp$frame == i]),
                          unique(tmp$num.y.dim2[tmp$loc == joint1[3] & tmp$frame == i]))


            angles_num.x[i+1] <- Angle(A_num.x, B_num.x, C_num.x)
            angles_num.y[i+1] <- Angle(A_num.y, B_num.y, C_num.y)

          }

          all_angles_num.x <- c(all_angles_num.x, angles_num.x)
          all_angles_num.y <- c(all_angles_num.y, angles_num.y)

        }

        name <- rep(unique(df$name), each = max(df$frame)+1)


      }


      df.traj <- data.frame(x=all_angles_num.x, y=all_angles_num.y, name)
      df.traj$object_type <- rep("two_triplets", nrow(df.traj))

    }
  }

  df.traj$frame <- rep(0:((nrow(df.traj)/nrow(unique(joint[, num.name])))-1),
                       nrow(unique(joint[, num.name])))
  df.traj$loc <- rep(0, nrow(df.traj))
  df.traj$segment <- rep(0, nrow(df.traj))

  col_order <- c("name", "object_type", "loc", "segment", "x", "y", "frame")
  df.traj <- df.traj[,col_order]
  return(df.traj)

}
