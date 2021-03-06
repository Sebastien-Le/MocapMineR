% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mediapipe2R.R
\name{mediapipe2R}
\alias{mediapipe2R}
\title{From Mediapipe data to R data}
\usage{
mediapipe2R(data, width, height, fps = 30, fig.height = 170, name)
}
\arguments{
\item{data}{A mediapipe type of data set}

\item{width}{The width of the video}

\item{height}{The height of the video}

\item{fps}{The number of frames per second}

\item{fig.height}{The height of the figure in the video}

\item{name}{The name  of the figure in the video}
}
\value{
A data set to be used with the MocapMineR package
}
\description{
Once you collect data from Mediapipe, you can easily analyze them with that function after transforming them.
}
\examples{

data(gaetan_apchagi_mediapipe)
gaetan_apchagi <- mediapipe2R(gaetan_apchagi_mediapipe, width=1080, height=1920, name="Gaetan")

}
