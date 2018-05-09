\name{repeat_rows}
\alias{repeat_rows}

\title{
Repeat rows of a dataframe for n times
}
\description{
Repeat the rows for certain times based on the number specified in the column "NUMBER_OF_LABELS_TO_CREATE".
}
\usage{
repeat_rows(dat = NULL)
}

\arguments{
  \item{dat}{
A dataframe with a column "NUMBER_OF_LABELS_TO_CREATE". If the column is missing, the function return the original dataframe.
}
}
\details{
This function helps to create multiple labels for one collector's number. Its output can then be supplied to the function `herbarium_label()` to generate labels. For NUMBER_OF_LABELS_TO_CREATE, any missing values (NA) will be replaced by 1. Anything in NUMBER_OF_LABELS_TO_CREATE that could not be converted into a number, will be treated as NA, but not being replaced.
}
\value{
dataframe with rows repeated for a certain number of times as specified in NUMBER_OF_LABELS_TO_CREATE.
}

\author{
Jinlong Zhang
}

\seealso{
\code{\link{herbarium_label}}
}
\examples{
### Just an Example
ddd <- c(LETTERS[1:19], "code")
dim(ddd) <- c(4, 5)
ddd
ddd <- data.frame(ddd)
ddd
ddd2 <- cbind(ddd, NUMBER_OF_LABELS_TO_CREATE = c(2, NA, "A", 5))
ddd2
repeat_rows(ddd2)
}
