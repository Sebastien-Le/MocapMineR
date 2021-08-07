#' @title From Mediapipe data to R data 
#'
#' @description Once you collect data from Mediapipe, you can easily analyze them with that function after transforming them.
#' 
#' @param data A mediapipe type of data set
#' @param width The width of the video
#' @param height The height of the video
#' @param fps The number of frames per second
#' @param fig.height  The height of the figure in the video
#' @param name  The name  of the figure in the video
#'
#' @return A data set to be used with the MocapMineR package
#' @import tidyr
#' @importFrom tidyr pivot_wider

#' @export
#'
#' @examples
#' 
#' data(gaetan_apchagi_mediapipe)
#' gaetan_apchagi <- mediapipe2R(gaetan_apchagi_mediapipe, width=1080, height=1920, name="Gaetan")
#' 
mediapipe2R <- function(data, width, height, fps=30, fig.height=170, name) {
  
  # We remove the parts of the face and the fingers of the hand
  loc <- NULL
  donnees_mod <- subset(data, !(loc %in% c("NOSE", "LEFT_EYE_INNER", "LEFT_EYE", "LEFT_EYE_OUTER", "RIGHT_EYE_INNER", "RIGHT_EYE", "RIGHT_EYE_OUTER", 
                                           "LEFT_EAR", "RIGHT_EAR", "MOUTH_LEFT", "MOUTH_RIGHT", 	
                                           "LEFT_INDEX", "LEFT_PINKY", "LEFT_THUMB", "RIGHT_INDEX", "RIGHT_PINKY", "RIGHT_THUMB")))
  
  # We put the x, y, z in the form of columns
  # https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  donnees_mod <- donnees_mod %>%
    pivot_wider(names_from = .data$coord, values_from = .data$value)
  
  # Multiplication by -1
  donnees_mod$x <- -donnees_mod$x
  donnees_mod$y <- -donnees_mod$y
  
  # I place the origin of the mark (0,0) at the bottom left (otherwise it is at the top)
  donnees_mod$y <- donnees_mod$y - min(donnees_mod$y)
  donnees_mod$x <- donnees_mod$x - min(donnees_mod$x)
  
  # Application of the expansion coefficient  
  coef_dilat_x <- 1/width
  coef_dilat_y <- 1/height
  donnees_mod$x <- donnees_mod$x/coef_dilat_x
  donnees_mod$y <- donnees_mod$y/coef_dilat_y
  
  
  # Left and right inversion
  donnees_mod = donnees_mod %>% mutate(loc = case_when(grepl("RIGHT", loc) ~ gsub("RIGHT","LEFT",loc), grepl("LEFT", loc) ~ gsub("LEFT","RIGHT",loc)))
  
  
  # Adding the frame column
  
  ens_temps <- unique(donnees_mod$time)
  
  donnees_mod$frame <- rep(0, nrow(donnees_mod))
  
  for(i in 1:nrow(donnees_mod)){
    for(j in 1:length(ens_temps)){
      if (donnees_mod$time[i]==ens_temps[j]) donnees_mod$frame[i] <- j-1
    }
  }
  
  # Adding the fps column
  donnees_mod$fps <- rep(fps, nrow(donnees_mod))
  
  # Modification of the time column
  donnees_mod$time <- donnees_mod$frame / fps
  
  # Add a first name column
  donnees_mod$name <- rep(name, nrow(donnees_mod))
  
  # Scaling according to the size of the femur
  femur_reel <- fig.height/4
  femur_MP <- unique(donnees_mod$y[donnees_mod$frame == 1 & donnees_mod$loc == "RIGHT_HIP"]) - unique(donnees_mod$y[donnees_mod$frame == 1 & donnees_mod$loc == "RIGHT_KNEE"])
  donnees_mod$y <- donnees_mod$y * femur_reel / femur_MP
  donnees_mod$x <- donnees_mod$x * femur_reel / femur_MP
  
  return(donnees_mod)
  
}